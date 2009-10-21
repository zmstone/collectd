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

/*
 * Private variables
 */
static pthread_t listen_thread_id;
static _Bool     listen_thread_running = false;

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

/*
 * Private functions
 */
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

	erl_free_term (reply);

	return (status);
} /* }}} int send_error */

static int eterm_to_int (const ETERM *term, int *ret_int) /* {{{ */
{
	if ((term == NULL) || (ret_int == NULL))
		return (EINVAL);

	switch (ERL_TYPE (term))
	{
		case ERL_INTEGER:
			*ret_int = (int) ERL_INT_VALUE (term);
			break;

		case ERL_U_INTEGER:
			*ret_int = (int) ERL_INT_UVALUE (term);
			break;

		case ERL_FLOAT:
			*ret_int = (int) (ERL_FLOAT_VALUE (term) + .5);
			break;

#ifdef ERL_LONGLONG
		case ERL_LONGLONG:
			*ret_int = (int) ERL_LL_VALUE (term);
			break;
#endif /* ERL_LONGLONG */

#ifdef ERL_U_LONGLONG
		case ERL_U_LONGLONG:
			*ret_int = (int) ERL_LL_UVALUE (term);
			break;
#endif /* ERL_U_LONGLONG */

		default:
			ERROR ("erlang plugin: Don't know how to cast "
					"erlang type %#x to int.", (unsigned int) ERL_TYPE (term));
			return (ENOTSUP);
	} /* switch (ERL_TYPE (term)) */

	return (0);
} /* }}} int eterm_to_int */

static int eterm_to_time_t (const ETERM *term, time_t *ret_time) /* {{{ */
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
			*ret_time = (time_t) ERL_INT_VALUE (term);
			break;

		case ERL_U_INTEGER:
			*ret_time = (time_t) ERL_INT_UVALUE (term);
			break;

		case ERL_ATOM:
			if ((strcmp ("now", ERL_ATOM_PTR (term)) == 0)
					|| (strcmp ("undefined", ERL_ATOM_PTR (term)) == 0))
			{
				*ret_time = time (NULL);
			}
			else
			{
				ERROR ("erlang plugin: Invalid atom for time: %s.",
						ERL_ATOM_PTR (term));
				return (ENOTSUP);
			}
			break;

		case ERL_FLOAT:
			*ret_time = (time_t) (ERL_FLOAT_VALUE (term) + .5);
			break;

#ifdef ERL_LONGLONG
		case ERL_LONGLONG:
			*ret_time = (time_t) ERL_LL_VALUE (term);
			break;
#endif /* ERL_LONGLONG */

#ifdef ERL_U_LONGLONG
		case ERL_U_LONGLONG:
			*ret_time = (time_t) ERL_LL_UVALUE (term);
			break;
#endif /* ERL_U_LONGLONG */

		default:
			ERROR ("erlang plugin: Don't know how to cast "
					"erlang type %#x to time_t.", (unsigned int) ERL_TYPE (term));
			return (ENOTSUP);
	} /* switch (ERL_TYPE (term)) */

	return (0);
} /* }}} int eterm_to_time_t */

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

static int eterm_to_values (const ETERM *term, const data_set_t *ds, /* {{{ */
		value_list_t *vl)
{
	int ds_index;
	int status;

	if ((term == NULL) || (ds == NULL) || (vl == NULL))
		return (EINVAL);

	if (!ERL_IS_LIST (term))
		return (-1);

	free (vl->values);
	vl->values = NULL;
	vl->values_len = 0;

	while (!ERL_IS_EMPTY_LIST (term))
	{
		const ETERM *eterm_value;
		value_t *tmp;

		if (ds_index >= ds->ds_num)
		{
			ds_index = ds->ds_num + 1;
			status = 0;
			break;
		}

		tmp = realloc (vl->values, sizeof (*tmp) * (vl->values_len + 1));
		if (tmp == NULL)
		{
			status = ENOMEM;
			break;
		}
		vl->values = tmp;

		eterm_value = ERL_CONS_HEAD (term);
		term = ERL_CONS_TAIL (term);

		status = eterm_to_value (eterm_value, ds->ds[ds_index].type,
				vl->values + vl->values_len);
		if (status != 0)
			break;

		vl->values_len++;
		ds_index++;
	}

	if ((status == 0) && (ds_index != ds->ds_num))
		NOTICE ("erlang plugin: Incorrect number of values received for type %s: "
				"Expected %i, got %i.", ds->type, ds->ds_num, ds_index);

	if ((status != 0) || (ds_index != ds->ds_num))
	{
		free (vl->values);
		vl->values = NULL;
		vl->values_len = 0;
		return (status);
	}

	return (0);
} /* }}} int eterm_to_values */

static int eterm_to_value_list (const ETERM *term, value_list_t *vl) /* {{{ */
{
	ETERM *tmp;
	int status;
	const data_set_t *ds;

	if ((term == NULL) || (vl == NULL))
		return (EINVAL);

	if (!ERL_IS_TUPLE (term) || (ERL_TUPLE_SIZE (term) != 9))
		return (EINVAL);

	tmp = erl_element (1, term);
	if (!ERL_IS_ATOM (tmp)
			|| (strcmp ("value_list", ERL_ATOM_PTR (tmp)) != 0))
	{
		erl_free_term (tmp);
		return (-1);
	}
	erl_free_term (tmp);

	status = 0;
	do
	{
#define TUPLE_ELEM_TO_CHAR_ARRAY(idx,buf) \
		tmp = erl_element ((idx), term); \
		status = eterm_to_string (tmp, (buf), sizeof (buf)); \
		erl_free_term (tmp); \
		if (status != 0) \
			break;

		TUPLE_ELEM_TO_CHAR_ARRAY (2, vl->host);
		TUPLE_ELEM_TO_CHAR_ARRAY (3, vl->plugin);
		TUPLE_ELEM_TO_CHAR_ARRAY (4, vl->plugin_instance);
		TUPLE_ELEM_TO_CHAR_ARRAY (5, vl->type);
		TUPLE_ELEM_TO_CHAR_ARRAY (6, vl->type_instance);

		ds = plugin_get_ds (vl->type);
		if (ds == NULL)
		{
			status = -1;
			break;
		}

		tmp = erl_element (7, term);
		status = eterm_to_time_t (tmp, &vl->time);
		erl_free_term (tmp);
		if (status != 0)
			break;

		tmp = erl_element (8, term);
		status = eterm_to_int (tmp, &vl->interval);
		erl_free_term (tmp);
		if (status != 0)
			break;
		if (vl->interval < 1)
			vl->interval = interval_g;

		tmp = erl_element (9, term);
		status = eterm_to_values (tmp, ds, vl);
		erl_free_term (tmp);
		if (status != 0)
			break;

#undef TUPLE_ELEM_TO_CHAR_ARRAY
	} while (0);

	if (status != 0)
		return (status);

	/* validate the struct */
	if ((vl->host[0] == 0) || (vl->plugin[0] == 0) || (vl->type[0] == 0))
		return (-1);

	if (ds->ds_num != vl->values_len)
		return (-1);

	return (0);
} /* }}} int eterm_to_value_list */

/* Returns non-zero only if the request could not be handled gracefully. */
static int handle_dispatch_values (ce_connection_info_t *cinfo, /* {{{ */
		const ErlMessage *req)
{
	ETERM *eterm_vl;
	value_list_t vl;
	int status;

	memset (&vl, 0, sizeof (vl));
	vl.values = NULL;

	eterm_vl = erl_element (2, req->msg);
	status = eterm_to_value_list (eterm_vl, &vl);
	erl_free_term (eterm_vl);

	if (status != 0)
	{
		free (vl.values);
		send_error (cinfo->fd, req->from, "Cannot parse argument as value list.");
		return (0);
	}

	status = plugin_dispatch_values (&vl);
	if (status != 0)
	{
		free (vl.values);
		send_error (cinfo->fd, req->from, "plugin_dispatch_values failed.");
		return (0);
	}

	free (vl.values);
	send_atom (cinfo->fd, req->from, "success");

	return (0);
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
			ETERM *reply;

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
			reply = NULL;
			if (strcmp ("dispatch_values", ERL_ATOM_PTR (func)) == 0)
				status = handle_dispatch_values (cinfo, &emsg);
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
			ERROR ("erlang plugin: erl_publish (%i) failed with status %i.", numeric_serv, status);
			close (sock_descr);
			sock_descr = -1;
			return (-1);
		}
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
			ERROR ("erlang plugin: erl_accept failed with status %i.", fd);
			close (listen);
			exit (EXIT_FAILURE);
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

static int ce_init (void) /* {{{ */
{
	if (!listen_thread_running)
	{
		int status;

		status = pthread_create (&listen_thread_id,
				/* attr = */ NULL,
				listen_thread,
				/* args = */ NULL);
		if (status == 0)
			listen_thread_running = true;
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
