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

static char conf_service[NI_MAXSERV] = "29157";
static char conf_cookie[256] = "ceisaequ";

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

/* Returns non-zero only if the request could not be handled gracefully. */
static int handle_dispatch_values (ce_connection_info_t *cinfo, /* {{{ */
		const ErlMessage *req)
{
	ETERM *vl;

	vl = erl_element (2, req->msg);
	if ((vl == NULL) || !ERL_IS_TUPLE (vl))
	{
		erl_free_term (vl);
		send_error (cinfo->fd, req->from, "Invalid format: VL not a tubple.");
		return (0);
	}

	/* We need: Identifier (5 parts), time, interval, values
	 * => 8 parts */
	if (ERL_TUPLE_SIZE (vl) != 8)
	{
		erl_free_term (vl);
		send_error (cinfo->fd, req->from, "Invalid format: "
				"VL needs eight components.");
		return (0);
	}

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
		status = erl_connect_xinit (/* host name = */ "leeloo",
				/* plain node name = */ "collectd",
				/* full node name  = */ "collectd@leeloo.lan.home.verplant.org",
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

void module_register (void)
{
	plugin_register_init ("erlang", ce_init);
}

/* vim: set sw=2 ts=2 noet fdm=marker : */
