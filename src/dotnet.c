/**
 * collectd - src/dotnet.c
 * Copyright (C) 2010  Florian Forster
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Authors:
 *   Florian Forster <ff at octo.it>
 **/

#include "collectd.h"
#include "plugin.h"
#include "common.h"
#include "configfile.h"

#include <glib.h>
#include <mono/jit/jit.h>
#include <mono/metadata/assembly.h>
#include <mono/metadata/threads.h>

struct dotnet_value_list_s
{
  value_t *values;
  int values_len;
  double time;
  double interval;
  char *host;
  char *plugin;
  char *plugin_instance;
  char *type;
  char *type_instance;
};
typedef struct dotnet_value_list_s dotnet_value_list_t;

#define CB_TYPE_INIT         2
struct dotnet_callback_info_s;
typedef struct dotnet_callback_info_s dotnet_callback_info_t;
struct dotnet_callback_info_s
{
  char *name;
  int type;
  void *callback;
  dotnet_callback_info_t *next;
};

typedef int (*dotnet_read_cb) (void);

static MonoDomain *_domain = NULL;
static dotnet_callback_info_t *callback_list = NULL;

static int dotnet_init (void) /* {{{ */
{
  dotnet_callback_info_t *ci;

  for (ci = callback_list; ci != NULL; ci = ci->next)
  {
    plugin_init_cb cb;
    int status;

    if (ci->type != CB_TYPE_INIT)
      continue;

    cb = ci->callback;
    status = (*cb) ();
    if (status != 0)
      ERROR ("dotnet plugin: The init function \"%s\" failed with status %i.",
          ci->name, status);
  } /* for (callback_list) */

  return (0);
} /* }}} int dotnet_init */

static int dotnet_read (user_data_t *ud) /* {{{ */
{
  dotnet_read_cb cb = ud->data;

  return ((*cb) ());
} /* }}} int dotnet_read */

/*
 * Functions exposed to .Net
 */
int dotnet_register_init (const char *name, plugin_init_cb cb) /* {{{ */
{
  dotnet_callback_info_t *ci;

  if ((name == NULL) || (cb == NULL))
    return (EINVAL);

  ci = malloc (sizeof (*ci));
  if (ci == NULL)
    return (ENOMEM);
  memset (ci, 0, sizeof (*ci));

  ci->name = strdup (name);
  if (ci->name == NULL)
  {
    sfree (ci);
    return (ENOMEM);
  }

  ci->type = CB_TYPE_INIT;
  ci->callback = cb;
  
  ci->next = callback_list;
  callback_list = ci;

  return (0);
} /* }}} int dotnet_register_init */

int dotnet_register_read (const char *name, dotnet_read_cb cb) /* {{{ */
{
  user_data_t ud;

  memset (&ud, 0, sizeof (ud));
  ud.data = cb;
  ud.free_func = NULL;

  plugin_register_complex_read (/* group = */ "dotnet",
      /* name      = */ name,
      /* callback  = */ dotnet_read,
      /* interval  = */ NULL,
      /* user data = */ &ud);

  return (0);
} /* }}} int dotnet_register_read */

int dotnet_dispatch_values (dotnet_value_list_t *dvl) /* {{{ */
{
  value_list_t vl = VALUE_LIST_INIT;

  vl.values = dvl->values;
  vl.values_len = dvl->values_len;

  if (dvl->time > 0.0)
    vl.time = DOUBLE_TO_CDTIME_T (dvl->time);
  else
    vl.interval = 0;

  if (dvl->interval > 0.0)
    vl.interval = DOUBLE_TO_CDTIME_T (dvl->interval);
  else
    vl.interval = interval_g;

  sstrncpy (vl.host, dvl->host, sizeof (vl.host));
  sstrncpy (vl.plugin, dvl->plugin, sizeof (vl.plugin));
  sstrncpy (vl.plugin_instance, dvl->plugin_instance, sizeof (vl.plugin_instance));
  sstrncpy (vl.type, dvl->type, sizeof (vl.type));
  sstrncpy (vl.type_instance, dvl->type_instance, sizeof (vl.type_instance));

  return (plugin_dispatch_values (&vl));
} /* }}} int dotnet_dispatch_values */

/*
 * Initialization functions
 */
static int dotnet_load_class (const char *assembly_name, /* {{{ */
    const char *name_space, const char *class_name)
{
  MonoDomain *domain;
  MonoThread *thread;
  MonoAssembly *assembly;
  MonoImage *image;
  MonoClass *class;
  MonoObject *obj;

  domain = mono_domain_create ();
  if (domain == NULL)
  {
    ERROR ("dotnet plugin: mono_domain_create failed.");
    return (-1);
  }

  thread = mono_thread_attach (domain);
  if (thread == NULL)
  {
    ERROR ("dotnet plugin: mono_thread_attach failed.");
    mono_domain_free (domain, /* force = */ 0);
    return (-1);
  }

  if (!mono_domain_set (domain, /* force = */ 0))
  {
    ERROR ("dotnet plugin: mono_domain_set failed.");
    mono_thread_detach (thread);
    mono_domain_free (domain, /* force = */ 0);
    return (-1);
  }

  assembly = mono_domain_assembly_open (domain, assembly_name);
  if (assembly == NULL)
  {
    ERROR ("dotnet plugin: mono_domain_assembly_open (\"%s\") failed.",
      assembly_name);
    mono_thread_detach (thread);
    mono_domain_free (domain, /* force = */ 0);
    return (-1);
  }

  image = mono_assembly_get_image (assembly);
  if (image == NULL)
  {
    ERROR ("dotnet plugin: mono_assembly_get_image failed.");
    mono_thread_detach (thread);
    mono_domain_free (domain, /* force = */ 0);
    return (-1);
  }

  class = mono_class_from_name (image,
      (name_space != NULL) ? name_space : "",
      class_name);
  if (class == NULL)
  {
    ERROR ("dotnet plugin: Looking up class \"%s\" in assembly \"%s\" failed.",
        class_name, assembly_name);
    mono_thread_detach (thread);
    mono_domain_free (domain, /* force = */ 0);
    return (-1);
  }

  obj = mono_object_new (domain, class);
  if (obj == NULL)
  {
    ERROR ("dotnet plugin: Creating a \"%s\" object failed.", class_name);
    mono_thread_detach (thread);
    mono_domain_free (domain, /* force = */ 0);
    return (-1);
  }

  mono_runtime_object_init (obj);

  DEBUG ("dotnet plugin: Successfully created a \"%s\" object.", class_name);

  mono_thread_detach (thread);
  return (0);
} /* }}} int dotnet_load_class */

/*
 * <Plugin dotnet>
 *   <LoadPlugin "Foobar">
 *     NameSpace "MyCompany"
 *     Assembly "path/to/file.dll"
 *   </LoadPlugin>
 *
 *   <Plugin "Foobar">
 *     ...
 *   </Plugin>
 * </Plugin>
 */
static int dotnet_config_loadplugin (oconfig_item_t *ci) /* {{{ */
{
  char *class_name = NULL;
  char *name_space = NULL;
  char *assembly_name = NULL;
  int status;
  int i;

  status = cf_util_get_string (ci, &class_name);
  if (status != 0)
    return (status);
  assert (class_name != NULL);

  for (i = 0; i < ci->children_num; i++)
  {
    oconfig_item_t *child = ci->children + i;

    if (strcasecmp ("NameSpace", child->key) == 0)
      cf_util_get_string (child, &name_space);
    else if (strcasecmp ("Assembly", child->key) == 0)
      cf_util_get_string (child, &assembly_name);
    else
      WARNING ("dotnet plugin: Config option \"%s\" is not allowed here.",
          child->key);
  }

  if (assembly_name == NULL)
  {
    ERROR ("dotnet plugin: No \"Assembly\" option within this \"LoadPlugin\" "
        "block (class \"%s\").", class_name);
    sfree (class_name);
    sfree (name_space);
  }

  status = dotnet_load_class (assembly_name, name_space, class_name);
  if (status != 0)
    return (status);

  return (0);
} /* }}} int dotnet_config_loadplugin */

static int dotnet_config (oconfig_item_t *ci) /* {{{ */
{
  int i;

  for (i = 0; i < ci->children_num; i++)
  {
    oconfig_item_t *child = ci->children + i;

    if (strcasecmp ("LoadPlugin", child->key) == 0)
      dotnet_config_loadplugin (child);
    else
      WARNING ("dotnet plugin: Ignoring unknown config option \"%s\".",
          child->key);
  }

  return (0);
} /* }}} int dotnet_config */

void module_register (void)
{
  _domain = mono_jit_init (PACKAGE_NAME);
  if (_domain == NULL)
  {
    ERROR ("dotnet plugin: mono_jit_init failed.");
    return;
  }

  plugin_register_init ("dotnet", dotnet_init);
  plugin_register_complex_config ("dotnet", dotnet_config);
} /* void module_register */

/* vim: set sw=2 sts=2 et fdm=marker : */
