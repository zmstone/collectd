/**
 * collectd - src/erlang.c
 * Copyright (C) 2009  Florian octo Forster
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; only version 2 of the License is applicable.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 *
 * Authors:
 *   Florian octo Forster <octo at verplant.org>
 **/

#include "collectd.h"
#include "common.h"
#include "plugin.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#include <pthread.h>

#include <erl_interface.h>
#include <ei.h>

/* 
 * Private data structures
 */
struct ce_connection_info_s
{
	int fd;
	ErlConnect conn;
};
typedef struct ce_connection_info_s ce_connection_info_t;

struct ce_callback_info_s
{
	int fd;
	ETERM *fun;
};
typedef struct ce_callback_info_s ce_callback_info_t;

/*
 * Private variables
 */
static pthread_t listen_thread_id;

static const char *config_keys[] =
{
	"BindTo",
	"BindPort",
	"Cookie",
	"NodeName"
};
static int config_keys_num = STATIC_ARRAY_SIZE (config_keys);

static char conf_node[NI_MAXHOST] = "";
static char conf_service[NI_MAXSERV] = "29157";
static char conf_cookie[256] = "ceisaequ";
static char conf_hostname[256] = "alyja";
static char conf_nodename[256] = "collectd";
static char conf_fullname[256] = "collectd@alyja.office.noris.de";

static int connection_counter = 1;
static pthread_mutex_t connection_lock = PTHREAD_MUTEX_INITIALIZER;

/*
 * Private functions
 */
static void ce_free_callback_info (ce_callback_info_t *ci) /* {{{ */
{
	if (ci == NULL)
		return;

	if (ci->fd >= 0)
	{
		erl_close_connection (ci->fd);
		ci->fd = -1;
	}

	if (ci->fun != NULL)
		erl_free_compound (ci->fun);

	free (ci);
} /* }}} void ce_free_callback_info */

static int send_atom (int fd, ETERM *to, const char *atom) /* {{{ */
{
	ETERM *reply;
	int status;

	reply = erl_mk_atom (atom);
	if (reply == NULL)
		return (ENOMEM);

	status = erl_send (fd, to, reply);
	erl_free_term (reply);

	if (status == 1)
		return (0);
	else
		return (erl_errno);
} /* }}} int send_atom */

static int send_error (int fd, ETERM *to, const char *message) /* {{{ */
{
	ETERM *reply;
	int status;

	DEBUG ("erlang plugin: send_error: message = %s.", message);
	reply = erl_format ("{~a,~s}", "error", message);

	status = erl_send (fd, to, reply);
	if (status != 1)
		status = erl_errno;
	else
		status = 0;

	erl_free_compound (reply);

	return (status);
} /* }}} int send_error */

static int eterm_to_time_t(const ETERM *term, time_t *ret_time) /* {{{ */
{
	if ((term == NULL) || (ret_time == NULL))
		return (EINVAL);

	if (ERL_IS_NIL (term)
			|| (ERL_IS_ATOM (term)
				&& ((strcmp ("now", ERL_ATOM_PTR (term)) == 0)
					|| (strcmp ("undefined", ERL_ATOM_PTR (term)) == 0))))
	{
		*ret_time = time (NULL);
		return (0);
	}

	switch (ERL_TYPE (term))
	{
		case ERL_INTEGER:
			*ret_time = (time_t)ERL_INT_VALUE (term);
			break;

		case ERL_U_INTEGER:
			*ret_time = (time_t)ERL_INT_UVALUE (term);
			break;

		case ERL_FLOAT:
			*ret_time = (time_t)(ERL_FLOAT_VALUE (term) + .5);
			break;

#ifdef ERL_LONGLONG
		case ERL_LONGLONG:
			*ret_time = (time_t)ERL_LL_VALUE (term);
			break;
#endif /* ERL_LONGLONG */

#ifdef ERL_U_LONGLONG
		case ERL_U_LONGLONG:
			*ret_time = (time_t)ERL_LL_UVALUE (term);
			break;
#endif /* ERL_U_LONGLONG */

		default:
			ERROR ("erlang plugin: Don't know how to cast "
					"erlang type %#x to time_t.", ERL_TYPE (term));
			return (ENOTSUP);
	} /* switch (ERL_TYPE (term)) */

	return (0);
} /* }}} int eterm_to_time_t*/

static int eterm_to_string (const ETERM *term, char *buffer, size_t buffer_size) /* {{{ */
{
	char *tmp;

	if ((term == NULL) || (buffer == NULL) || (buffer_size <= 0))
		return (EINVAL);

	memset (buffer, 0, buffer_size);

	if (ERL_IS_EMPTY_LIST (term)
			|| ERL_IS_NIL (term)
			|| (ERL_IS_ATOM (term)
				&& (strcmp ("undefined", ERL_ATOM_PTR (term)) == 0)))
	{
		buffer[0] = 0;
		return (0);
	}

	if (!ERL_IS_LIST (term))
		return (-1);

	tmp = erl_iolist_to_string (term);
	if (tmp == NULL)
		return (-1);

	strncpy (buffer, tmp, buffer_size - 1);
	erl_free (tmp);

	return (0);
} /* }}} int eterm_to_string */

static int eterm_to_value (const ETERM *term, int ds_type, /* {{{ */
		value_t *value)
{
	if ((term == NULL) || (value == NULL))
		return (EINVAL);

	switch (ERL_TYPE (term))
	{
		case ERL_INTEGER:
		{
			int v = ERL_INT_VALUE (term);
			switch (ds_type)
			{
				case DS_TYPE_COUNTER:  value->counter  = (counter_t)  v; break;
				case DS_TYPE_GAUGE:    value->gauge    = (gauge_t)    v; break;
				case DS_TYPE_DERIVE:   value->derive   = (derive_t)   v; break;
				case DS_TYPE_ABSOLUTE: value->absolute = (absolute_t) v; break;
			}
			break;
		}

		case ERL_U_INTEGER:
		{
			unsigned int v = ERL_INT_UVALUE (term);
			switch (ds_type)
			{
				case DS_TYPE_COUNTER:  value->counter  = (counter_t)  v; break;
				case DS_TYPE_GAUGE:    value->gauge    = (gauge_t)    v; break;
				case DS_TYPE_DERIVE:   value->derive   = (derive_t)   v; break;
				case DS_TYPE_ABSOLUTE: value->absolute = (absolute_t) v; break;
			}
			break;
		}

		case ERL_FLOAT:
		{
			double v = ERL_FLOAT_VALUE (term);
			switch (ds_type)
			{
				case DS_TYPE_COUNTER:  value->counter  = (counter_t)  v; break;
				case DS_TYPE_GAUGE:    value->gauge    = (gauge_t)    v; break;
				case DS_TYPE_DERIVE:   value->derive   = (derive_t)   v; break;
				case DS_TYPE_ABSOLUTE: value->absolute = (absolute_t) v; break;
			}
			break;
		}

#ifdef ERL_LONGLONG
		case ERL_LONGLONG:
		{
			long long v = ERL_LL_VALUE (term);
			switch (ds_type)
			{
				case DS_TYPE_COUNTER:  value->counter  = (counter_t)  v; break;
				case DS_TYPE_GAUGE:    value->gauge    = (gauge_t)    v; break;
				case DS_TYPE_DERIVE:   value->derive   = (derive_t)   v; break;
				case DS_TYPE_ABSOLUTE: value->absolute = (absolute_t) v; break;
			}
			break;
		}
#endif /* ERL_LONGLONG */

#ifdef ERL_U_LONGLONG
		case ERL_U_LONGLONG:
		{
			unsigned long long v = ERL_LL_UVALUE (term);
			switch (ds_type)
			{
				case DS_TYPE_COUNTER:  value->counter  = (counter_t)  v; break;
				case DS_TYPE_GAUGE:    value->gauge    = (gauge_t)    v; break;
				case DS_TYPE_DERIVE:   value->derive   = (derive_t)   v; break;
				case DS_TYPE_ABSOLUTE: value->absolute = (absolute_t) v; break;
			}
			break;
		}
#endif /* ERL_U_LONGLONG */

		default:
			ERROR ("erlang plugin: Don't know how to cast "
					"erlang type %#x to value_t.", (unsigned int) ERL_TYPE (term));
			return (ENOTSUP);
	} /* switch (ERL_TYPE (term)) */

	return (0);
} /* }}} int eterm_to_value */

static int eterm_list_len(const ETERM *term) /* {{{ */
{
	int len = 0;
	while(!ERL_IS_EMPTY_LIST(term)){
		term = ERL_CONS_TAIL(term);
		len ++;
	}
	return len;
}/* }}} int eterm_list_check */

static int eterm_to_values (const ETERM *term, const data_set_t *ds, /* {{{ */
		value_list_t *vl)
{
	int ds_index = 0;
	int status = 0;

	if ((term == NULL) || (ds == NULL) || (vl == NULL))
		return (EINVAL);

	if (!ERL_IS_LIST (term)){
		ERROR("erlang plugin: Bad eterm type, expecting list.");
		return (-1);
	}

	/* free (vl->values); */ /* why free ? it's not enitialized */
	vl->values_len = eterm_list_len(term);
	if(0 == vl->values_len){
		ERROR("erlang plugin: Bad eterm, expecting a non-empty list.");
		return (-1);
	}
	if(ds->ds_num < vl->values_len){
		ERROR("erlang plugin: Bad eterm, too many values for type %s, expecting %i.",
			ds->type, ds->ds_num);
		return (-1);
	}
	vl->values = malloc(vl->values_len * sizeof(value_t));

	while(!ERL_IS_EMPTY_LIST (term)) {
		const ETERM *eterm_value;
		eterm_value = ERL_CONS_HEAD (term);
		status = eterm_to_value (eterm_value, ds->ds[ds_index].type,
				vl->values + ds_index);
		if (status != 0) break;
		term = ERL_CONS_TAIL (term);
		ds_index++;
	}
	if ((status != 0) || (ds_index != ds->ds_num))
	{
		ERROR("erlang plugin: failed to decode value list.");
		free (vl->values);
		vl->values = NULL;
		vl->values_len = 0;
	}

	return (status);
} /* }}} int eterm_to_values */

static int tuple_elem_to_name_str(const ETERM * term, int index, char * name) /* {{{ */
{
	ETERM * tmp;
	int status;
	tmp = erl_element(index, term);
	status = eterm_to_string(tmp, name, DATA_MAX_NAME_LEN);
	erl_free_term(tmp);
  return status;
} /* }}} tuple_elem_to_name_str */

static int eterm_to_value_list (const ETERM *term, value_list_t *vl) /* {{{ */
{
	ETERM *tmp;
	int status;
	const data_set_t *ds;
	if ((term == NULL) || (vl == NULL)) return (EINVAL);
	if (!ERL_IS_TUPLE (term) || (ERL_TUPLE_SIZE (term) != 8)) return (EINVAL);

	tmp = erl_element (1, term);
	if (!ERL_IS_ATOM (tmp) || (strcmp ("value_list", ERL_ATOM_PTR (tmp)) != 0)) {
		erl_free_term (tmp);
		return (-1);
	}
	erl_free_term (tmp);

	status = 0;
	do {
		if(0 != (status = tuple_elem_to_name_str(term, 2, vl->host))){
			ERROR("erlang plugin: failed to decode host name.");
			break;
		}
		if(0 != (status = tuple_elem_to_name_str(term, 3, vl->plugin))){
			ERROR("erlang plugin: failed to decode plugin name.");
			break;
		}
		if(0 != (status = tuple_elem_to_name_str(term, 4, vl->plugin_instance))){
			ERROR("erlang plugin: failed to decode plugin instance.");
			break;
		}
		if(0 != (status = tuple_elem_to_name_str(term, 5, vl->type))){
			ERROR("erlang plugin: failed to decode type name.");
			break;
		}
		if(0 != (status = tuple_elem_to_name_str(term, 6, vl->type_instance))){
			ERROR("erlang plugin: failed to decode type instance.");
			break;
		}

		ds = plugin_get_ds (vl->type);
		if(ds == NULL){
			ERROR("erlang plugin: failed to get data set.");
			status = -1;
			break;
		}

		vl->time = 0;
		tmp = erl_element (7, term);
		time_t interval = 0;
		status = eterm_to_time_t(tmp, &interval);
		erl_free_term (tmp);
		if (status != 0) break;
		if (interval < 1) vl->interval = interval_g;
		else vl->interval = TIME_T_TO_CDTIME_T(interval);

		tmp = erl_element (8, term);
		status = eterm_to_values (tmp, ds, vl);
		erl_free_term (tmp);
		if (status != 0){
			ERROR("erlang plugin: failed to decode values.");
			break;
		}
	} while (0);

	if (status != 0) return (status);

	/* validate the struct */
	if ((vl->host[0] == 0) || (vl->plugin[0] == 0) || (vl->type[0] == 0)){
		ERROR("erlang plugin: bad bad value list structure.");
		return (-1);
	}

	if (ds->ds_num != vl->values_len){
		ERROR("erlang plugin: bad number of data sets.");
		return (-1);
	}
	/*
  ERROR("###            host=%s", vl->host);
  ERROR("###          plugin=%s", vl->plugin);
  ERROR("### plugin_instance=%s", vl->plugin_instance);
  ERROR("###            type=%s", vl->type);
  ERROR("###   type_instance=%s", vl->type_instance);
  ERROR("###      values_len=%i", vl->values_len);
  ERROR("###        interval=%llu", vl->interval);
  ERROR("###       values[0]=%lf", (vl->values[0]).gauge);
	*/
	return (0);
} /* }}} int eterm_to_value_list */

static int test_value_list(value_list_t *vl) /* {{{ */
{
	value_t * values = NULL;
	values = malloc (sizeof (value_t));
	values[0].gauge= rand() % 100;
	vl -> values = values;
	vl -> values_len = 1;
	sstrncpy(vl->host, hostname_g, DATA_MAX_NAME_LEN);
	sstrncpy(vl->plugin, "erlang", DATA_MAX_NAME_LEN);
	sstrncpy(vl->plugin_instance, "p_ins", DATA_MAX_NAME_LEN);
	sstrncpy(vl->type, "gauge", DATA_MAX_NAME_LEN);
	sstrncpy(vl->type_instance, "t_ins", DATA_MAX_NAME_LEN);
	return 0;
} /* }}} int test_value_list */

static int ce_read (user_data_t *ud) /* {{{ */
{
	ce_callback_info_t *ci;
	ETERM *rpc_args;
	ETERM *rpc_reply;

	if ((ud == NULL) || (ud->data == NULL))
		return (-1);

	ci = ud->data;

	rpc_args = erl_format ("[~w,[]]", erl_copy_term (ci->fun));
	if (rpc_args == NULL)
	{
		ERROR ("erlang plugin: erl_format failed.");
		return (-1);
	}

	DEBUG ("erlang plugin: Making remote procedure call ...");
	rpc_reply = erl_rpc (ci->fd,
			/* module = */ "erlang", /* function = */ "apply",
			/* arguments = */ rpc_args);
	DEBUG ("erlang plugin: ... done.");
	erl_free_compound (rpc_args);
	if (rpc_reply == NULL)
	{
			char errbuf[1024];
			ERROR ("erlang plugin: erl_rpc failed: %s",
					sstrerror (erl_errno, errbuf, sizeof (errbuf)));
			return (-1);
	}

	/* FIXME: The return value is not yet used. */
	erl_free_compound (rpc_reply);

	return (0);
} /* }}} int ce_read */

/* Returns non-zero only if the request could not be handled gracefully. */
static int handle_dispatch_values (ce_connection_info_t *cinfo, /* {{{ */
		const ErlMessage *req)
{
	ETERM *eterm_vl;
	value_list_t vl = VALUE_LIST_INIT;
	int status;
	int stupid_switch = 0;

	eterm_vl = erl_element (2, req->msg);
	if(stupid_switch < 1) {
		status = eterm_to_value_list (eterm_vl, &vl);
	}else{
		status = test_value_list(&vl);
	}
	erl_free_term (eterm_vl);

	if (status != 0)
	{
		free (vl.values);
		status = send_error (cinfo->fd, req->from, "Cannot parse argument as value list.");
		return (status);
	}

	status = plugin_dispatch_values (&vl);
	if (status != 0)
	{
		free (vl.values);
		status = send_error (cinfo->fd, req->from, "plugin_dispatch_values failed.");
		return (status);
	}

	free (vl.values);
	status = send_atom (cinfo->fd, req->from, "success");

	return (status);
} /* }}} int handle_dispatch_values */

/* Returns non-zero only if the request could not be handled gracefully. */
static int handle_register_read (ce_connection_info_t *cinfo, /* {{{ */
		const ErlMessage *req)
{
	ETERM *eterm_cb;
	ce_callback_info_t *ci;
	user_data_t ud;
	int status;
	int connection_number;
	char callback_name[64];

	if ((cinfo == NULL) || (req == NULL))
		return (EINVAL);

	eterm_cb = erl_element (2, req->msg);

	if (ERL_TYPE (eterm_cb) != ERL_FUNCTION)
	{
		erl_free_term (eterm_cb);
		status = send_error (cinfo->fd, req->from,
				"Argument to `register_read' must be a callback function.");
		return (status);
	}

	ci = malloc (sizeof (ci));
	if (ci == NULL)
	{
		erl_free_term (eterm_cb);
		status = send_error (cinfo->fd, req->from, "malloc failed.");
		return (status);
	}

	/* Lock around `erl_connect_init' and `erl_connect'. */
	pthread_mutex_lock (&connection_lock);

	connection_number = connection_counter;
	connection_counter++;

	/* Create a new `cnode' for each connection. Otherwise we cannot determine
	 * which RPC call a message belongs to. */
	status = erl_connect_init (connection_number, conf_cookie,
			/* creation = */ 0);
	if (!status) /* Yes, it's this way around in this case ... {{{ */
	{
			char errbuf[1024];
			pthread_mutex_unlock (&connection_lock);
			ERROR ("erlang plugin: erl_connect_init failed: %s",
					sstrerror (erl_errno, errbuf, sizeof (errbuf)));
			sfree (ci);
			erl_free_term (eterm_cb);
			status = send_error (cinfo->fd, req->from, "erl_connect failed.");
			return (status);
	} /* }}} */

	ci->fd = erl_connect (cinfo->conn.nodename);
	if (ci->fd < 0) /* {{{ */
	{
			char errbuf[1024];
			pthread_mutex_unlock (&connection_lock);
			ERROR ("erlang plugin: erl_connect(%s) failed: %s",
					cinfo->conn.nodename,
					sstrerror (erl_errno, errbuf, sizeof (errbuf)));
			sfree (ci);
			erl_free_term (eterm_cb);
			status = send_error (cinfo->fd, req->from, "erl_connect failed.");
			return (status);
	} /* }}} */

	pthread_mutex_unlock (&connection_lock);

	ci->fun = eterm_cb;

	memset (&ud, 0, sizeof (ud));
	ud.data = ci;
	ud.free_func = (void (*) (void *)) ce_free_callback_info;

	ssnprintf (callback_name, sizeof (callback_name), "erlang:%i",
			connection_number);

	status = plugin_register_complex_read (/* group  = */ NULL,
			callback_name, ce_read, /* interval = */ NULL, &ud);
	if (status == 0)
		status = send_atom (cinfo->fd, req->from, "success");
	else
		status = send_error (cinfo->fd, req->from,
				"plugin_register_complex_read failed.");

	return (status);
} /* }}} int handle_dispatch_values */

static void *handle_client_thread (void *arg) /* {{{ */
{
	ce_connection_info_t *cinfo;
	ErlMessage emsg;
	unsigned char buffer[4096];

	cinfo = arg;

	DEBUG ("erlang plugin: handle_client_thread[%i]: Handling client %s.",
			cinfo->fd, cinfo->conn.nodename);

	emsg.from = NULL;
	emsg.to = NULL;
	emsg.msg = NULL;

	while (42)
	{
		int status;

		erl_free_term (emsg.from);
		emsg.from = NULL;
		erl_free_term (emsg.to);
		emsg.to = NULL;
		erl_free_term (emsg.msg);
		emsg.msg = NULL;

		status = erl_receive_msg (cinfo->fd, buffer, sizeof (buffer), &emsg);
		if (status == ERL_TICK)
			continue;

		if (status == ERL_ERROR)
			break;

		if (emsg.type == ERL_REG_SEND)
		{
			ETERM *func;
			/* ETERM *reply; */

			if (!ERL_IS_TUPLE (emsg.msg))
			{
				ERROR ("erlang plugin: Message is not a tuple.");
				send_atom (cinfo->fd, emsg.from, "error");
				continue;
			}

			func = erl_element (1, emsg.msg);
			if (!ERL_IS_ATOM (func))
			{
				ERROR ("erlang plugin: First element is not an atom!");
				send_atom (cinfo->fd, emsg.from, "error");
				erl_free_term (func);
				continue;
			}

			DEBUG ("erlang plugin: Wanted function is: %s.", ERL_ATOM_PTR (func));
			/* reply = NULL; */
			if (strcmp ("dispatch_values", ERL_ATOM_PTR (func)) == 0)
				status = handle_dispatch_values (cinfo, &emsg);
			else if (strcmp ("register_read", ERL_ATOM_PTR (func)) == 0)
				status = handle_register_read (cinfo, &emsg);
			else
			{
				ERROR ("erlang plugin: Received request for invalid function `%s'.",
						ERL_ATOM_PTR (func));
				send_atom (cinfo->fd, emsg.from, "error");
				status = 0;
			}

			/* Check for fatal errors in the callback functions. */
			if (status != 0)
			{
				ERROR ("erlang plugin: Handling request for `%s' failed.",
						ERL_ATOM_PTR (func));
				erl_free_term (func);
				break;
			}

			erl_free_term (func);
		}
		else if (emsg.type == ERL_EXIT)
		{
			DEBUG ("erlang plugin: handle_client_thread[%i]: "
					"Received exit message.", cinfo->fd);
			break;
		}
		else
		{
			ERROR ("erlang plugin: Message type not handled: %i.", emsg.type);
		}
	} /* while (42) */

	erl_free_term (emsg.from);
	emsg.from = NULL;
	erl_free_term (emsg.to);
	emsg.to = NULL;
	erl_free_term (emsg.msg);
	emsg.msg = NULL;

	DEBUG ("erlang plugin: handle_client_thread[%i]: Exiting.", cinfo->fd);

	close (cinfo->fd);
	free (cinfo);

	pthread_exit ((void *) 0);
	return ((void *) 0);
} /* }}} void *handle_client_thread */

static int create_listen_socket (void) /* {{{ */
{
	struct addrinfo ai_hints;
	struct addrinfo *ai_list;
	struct addrinfo *ai_ptr;
	int sock_descr;
	int status;
	int numeric_serv;

	sock_descr = -1;

	memset (&ai_hints, 0, sizeof (ai_hints));
	/* AI_PASSIVE => returns INADDR_ANY */
	ai_hints.ai_flags = AI_PASSIVE;
#ifdef AI_ADDRCONFIG
	ai_hints.ai_flags |= AI_ADDRCONFIG;
#endif
	/* IPv4 only :( */
	ai_hints.ai_family = AF_INET;
	ai_hints.ai_socktype = SOCK_STREAM;

	ai_list = NULL;
	status = getaddrinfo (/* node = */ NULL, /* service = */ conf_service,
			&ai_hints, &ai_list);
	if (status != 0)
	{
		ERROR ("erlang plugin: getaddrinfo failed: %s", gai_strerror (status));
		return (-1);
	}

	for (ai_ptr = ai_list; ai_ptr != NULL; ai_ptr = ai_ptr->ai_next)
	{
		struct sockaddr_in *sa_in;
		struct in_addr *sin_addr;
		int yes;

		assert (ai_ptr->ai_family == AF_INET);
		sa_in = (struct sockaddr_in *) ai_ptr->ai_addr;
		sin_addr = &sa_in->sin_addr;
		numeric_serv = (int) ntohs (sa_in->sin_port);

		/* Dunno if calling this multiple times is legal. Since it wants to have
		 * the sin_addr for some reason this is the best place to call this,
		 * though. -octo */
		status = erl_connect_xinit (/* host name = */ conf_hostname,
				/* plain node name = */ conf_nodename,
				/* full node name  = */ conf_fullname,
				/* our address     = */ sin_addr,
				/* secret cookie   = */ conf_cookie,
				/* instance number = */ 0);
		if (status < 0)
		{
			ERROR ("erlang plugin: erl_connect_xinit failed with status %i.",
					status);
			continue;
		}

		sock_descr = socket (ai_ptr->ai_family, ai_ptr->ai_socktype,
				ai_ptr->ai_protocol);
		if (sock_descr < 0)
		{
			ERROR ("erlang plugin: socket(2) failed.");
			continue;
		}

		yes = 1;
		status = setsockopt (sock_descr, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(yes));
		if (status != 0)
		{
			ERROR ("erlang plugin: setsockopt(2) failed.");
			close (sock_descr);
			sock_descr = -1;
			continue;
		}

		status = bind (sock_descr, ai_ptr->ai_addr, ai_ptr->ai_addrlen);
		if (status != 0)
		{
			ERROR ("erlang plugin: bind(2) failed.");
			close (sock_descr);
			sock_descr = -1;
			continue;
		}

		status = listen (sock_descr, /* backlog = */ 10);
		if (status != 0)
		{
			ERROR ("erlang plugin: listen(2) failed.");
			close (sock_descr);
			sock_descr = -1;
			continue;
		}

		break;
	} /* for (ai_list) */

	freeaddrinfo (ai_list);

	if (sock_descr >= 0)
	{
		status = erl_publish (numeric_serv);
		if (status < 0)
		{
			ERROR ("erlang plugin: erl_publish (%i) failed with status %i. "
					"Is epmd running?", numeric_serv, status);
			close (sock_descr);
			sock_descr = -1;
			return (-1);
		}
	}

	if (sock_descr >= 0)
	{
		INFO ("erlang plugin: Created Erlang socket: Nodename %s, Port %i, "
				"Cookie %s.",
				conf_fullname, numeric_serv, conf_cookie);
	}

	return (sock_descr);
} /* }}} int create_listen_socket */

void *listen_thread (void *arg) /* {{{ */
{
	int listen;
	int fd;

	ErlConnect conn;

	/* I have no fucking idea what this does, nor what the arguments are. Didn't
	 * find any comprehensive docs yet. */
	erl_init (/* void *x = */ NULL, /* long y = */ 0);

	listen = create_listen_socket ();
	if (listen < 0)
		exit (EXIT_FAILURE);

	while (42)
	{
		pthread_t tid;
		pthread_attr_t tattr;
		ce_connection_info_t *arg;

		fd = erl_accept (listen, &conn);
		if (fd < 0)
		{
			char errbuf[1024];
			ERROR ("erlang plugin: erl_accept failed: %s",
					sstrerror (erl_errno, errbuf, sizeof (errbuf)));
			continue;
		}
		DEBUG ("erlang plugin: Got connection from %s on fd %i.",
				conn.nodename, fd);

		pthread_attr_init (&tattr);
		pthread_attr_setdetachstate (&tattr, PTHREAD_CREATE_DETACHED);

		arg = malloc (sizeof (*arg));
		if (arg == NULL)
		{
			ERROR ("erlang plugin: malloc failed.");
			close (fd);
			continue;
		}
		memset (arg, 0, sizeof (*arg));

		arg->fd = fd;
		memcpy (&arg->conn, &conn, sizeof (conn));

		pthread_create (&tid, &tattr, handle_client_thread, arg);
	} /* while (42) */

	pthread_exit ((void *) 0);
	return ((void *) 0);
} /* }}} void *listen_thread */

static int listen_thread_running = 0;
static int ce_init (void) /* {{{ */
{
	if (listen_thread_running == 0)
	{
		int status;

		status = pthread_create (&listen_thread_id,
				/* attr = */ NULL,
				listen_thread,
				/* args = */ NULL);
		if (status == 0)
			listen_thread_running = 1;
	}

	return (0);
} /* }}} int ce_init */

static int ce_config (const char *key, const char *value) /* {{{ */
{
	if (strcasecmp ("BindTo", key) == 0)
	{
		sstrncpy (conf_node, value, sizeof (conf_node));
	}
	else if (strcasecmp ("BindPort", key) == 0)
	{
		sstrncpy (conf_service, value, sizeof (conf_service));
	}
	else if (strcasecmp ("Cookie", key) == 0)
	{
		sstrncpy (conf_cookie, value, sizeof (conf_cookie));
	}
	else if (strcasecmp ("NodeName", key) == 0)
	{
		const char *host;

		host = strchr (value, '@');
		if (host == NULL)
		{
			sstrncpy (conf_nodename, value, sizeof (conf_nodename));
			sstrncpy (conf_hostname, hostname_g, sizeof (conf_hostname));
			ssnprintf (conf_fullname, sizeof (conf_fullname), "%s@%s",
					conf_nodename, conf_hostname);
		}
		else /* if (host != NULL) */
		{
			char *tmp;

			sstrncpy (conf_nodename, value, sizeof (conf_nodename));
			sstrncpy (conf_hostname, host + 1, sizeof (conf_hostname));
			sstrncpy (conf_fullname, value, sizeof (conf_fullname));

			tmp = strchr (conf_nodename, '@');
			if (tmp != NULL)
				*tmp = 0;
		}
	}
	else
	{
		return (-1);
	}

	return (0);
} /* }}} int ce_config */

void module_register (void)
{
	plugin_register_config ("erlang", ce_config, config_keys, config_keys_num);
	plugin_register_init ("erlang", ce_init);
}

/* vim: set sw=2 ts=2 noet fdm=marker : */
