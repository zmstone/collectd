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

struct dotnet_callback_info_s
{
  gint32 domain_id;
  guint32 obj_handle;
};
typedef struct dotnet_callback_info_s dotnet_callback_info_t;

static MonoDomain *_domain = NULL;

static int dotnet_read (user_data_t *ud) /* {{{ */
{
  dotnet_callback_info_t *ci = ud->data;
  MonoDomain *domain;
  MonoThread *thread;
  MonoClass  *class;
  MonoObject *object;
  MonoMethod *method;
  MonoObject *ret;

  DEBUG ("dotnet plugin: mono_domain_get_by_id (%"PRIi32") ...",
      (int32_t) ci->domain_id);
  domain = mono_domain_get_by_id (ci->domain_id);
  if (domain == NULL)
  {
    ERROR ("dotnet plugin: mono_domain_get_by_id failed.");
    return (-1);
  }

  thread = mono_thread_attach (domain);
  if (thread == NULL)
  {
    ERROR ("dotnet plugin: mono_thread_attach failed.");
    return (-1);
  }

  if (!mono_domain_set (domain, /* force = */ 0))
  {
    ERROR ("dotnet plugin: mono_domain_set failed.");
    return (-1);
  }

  DEBUG ("dotnet plugin: mono_gchandle_get_target ...");
  object = mono_gchandle_get_target (ci->obj_handle);
  if (object == NULL)
  {
    ERROR ("dotnet plugin: mono_gchandle_get_target failed.");
    mono_thread_detach (thread);
    return (-1);
  }

  DEBUG ("dotnet plugin: mono_object_get_class ...");
  class = mono_object_get_class (object);
  if (class == NULL)
  {
    ERROR ("dotnet plugin: mono_object_get_class failed.");
    mono_thread_detach (thread);
    return (-1);
  }

  DEBUG ("dotnet plugin: mono_class_get_method_from_name ...");
  method = mono_class_get_method_from_name (class,
      /* name = */ "read", /* nargs = */ 0);
  if (method == NULL)
  {
    ERROR ("dotnet plugin: mono_class_get_method_from_name failed.");
    mono_thread_detach (thread);
    return (-1);
  }

  DEBUG ("dotnet plugin: mono_runtime_invoke ...");
  ret = mono_runtime_invoke (method, object, /* params = */ NULL,
      /* exception ptr = */ NULL);
  DEBUG ("dotnet plugin: mono_runtime_invoke returned %p.", (void *) ret);

  mono_thread_detach (thread);
  return (0);
} /* }}} int dotnet_read */

static void dotnet_callback_info_free (void *ptr) /* {{{ */
{
  dotnet_callback_info_t *ci = ptr;

  if (ci == NULL)
    return;

  mono_gchandle_free (ci->obj_handle);
  sfree (ci);
} /* }}} void dotnet_callback_info_free */

static MonoMethod *dotnet_object_method_get_from_name (MonoObject *object, /* {{{ */
    const char *name, int param_count)
{
  MonoClass *class;

  class = mono_object_get_class (object);
  if (class == NULL)
    return (NULL);

  while (42)
  {
    MonoClass *pclass;
    MonoMethod *method;

    method = mono_class_get_method_from_name (class, name, param_count);
    if (method != NULL)
      return (method);

    pclass = mono_class_get_parent (class);
    if ((pclass == NULL) || (pclass == class))
    {
      ERROR ("dotnet plugin: Unable to find method \"%s\" in class \"%s\".",
          name, mono_class_get_name (mono_object_get_class (object)));
      return (NULL);
    }
    class = pclass;
  } /* while (42) */
  /* Not reached */
  return (NULL);
} /* }}} MonoMethod *dotnet_object_method_get_from_name */

static MonoProperty *dotnet_object_get_property_from_name (MonoObject *object, /* {{{ */
    const char *name)
{
  MonoClass *class;

  class = mono_object_get_class (object);

  while (42)
  {
    MonoClass *pclass;
    MonoProperty *prop;

    prop = mono_class_get_property_from_name (class, name);
    if (prop != NULL)
      return (prop);

    pclass = mono_class_get_parent (class);
    if ((pclass == NULL) || (pclass == class))
    {
      ERROR ("dotnet plugin: Unable to find property \"%s\" in class \"%s\".",
          name, mono_class_get_name (mono_object_get_class (object)));
      return (NULL);
    }
    class = pclass;
  } /* while (42) */
  /* Not reached */
  return (NULL);
} /* }}} MonoProperty *dotnet_object_get_property_from_name */

static int dotnet_object_method_get_string (MonoObject *obj, /* {{{ */
    const char *method_name, char *buffer, size_t buffer_size)
{
  MonoMethod *method;
  MonoObject *ret;
  char *tmp;

  method = dotnet_object_method_get_from_name (obj, method_name, /* nargs = */ 0);
  if (method == NULL)
    return (-1);

  ret = mono_runtime_invoke (method, obj, /* params = */ NULL, /* exception = */ NULL);
  if (ret == NULL)
    return (-2);

  tmp = mono_string_to_utf8 ((MonoString *) ret);
  if (tmp == NULL)
  {
    ERROR ("dotnet plugin: mono_string_to_utf8 failed.");
    return (-3);
  }
  DEBUG ("dotnet plugin: Method \"%s\" returned string \"%s\".",
      mono_method_get_name (method), tmp);

  sstrncpy (buffer, tmp, buffer_size);
  return (0);
} /* }}} int dotnet_object_method_get_string */

static int dotnet_object_method_get_cdtime (MonoObject *obj, /* {{{ */
    const char *method_name, cdtime_t *ret_value)
{
  MonoMethod *method;
  MonoObject *ret_obj;
  double tmp;

  method = dotnet_object_method_get_from_name (obj, method_name, /* nargs = */ 0);
  if (method == NULL)
    return (-1);

  ret_obj = mono_runtime_invoke (method, obj, /* params = */ NULL, /* exception = */ NULL);
  if (ret_obj == NULL)
    return (-2);

  tmp = *((double *) mono_object_unbox (ret_obj));
  DEBUG ("dotnet plugin: Method \"%s\" returned value %.3f.",
      mono_method_get_name (method), tmp);
  *ret_value = DOUBLE_TO_CDTIME_T (tmp);

  return (0);
} /* }}} int dotnet_object_method_get_cdtime */

static int dotnet_object_method_get_values (MonoObject *obj, /* {{{ */
    value_list_t *vl)
{
  MonoMethod *method;
  MonoObject *ilist_obj;
  MonoObject *tmp_obj;
  MonoProperty *count_prop;
  MonoProperty *item_prop;
  int32_t i;

  method = dotnet_object_method_get_from_name (obj, "getValues", /* nargs = */ 0);
  if (method == NULL)
    return (-1);

  ilist_obj = mono_runtime_invoke (method, obj, /* params = */ NULL, /* exception = */ NULL);
  if (ilist_obj == NULL)
    return (-2);

  count_prop = dotnet_object_get_property_from_name (ilist_obj, "Count");
  if (count_prop == NULL)
  {
    ERROR ("dotnet plugin: dotnet_object_get_property_from_name (\"Count\") failed.");
    return (-4);
  }

  item_prop = dotnet_object_get_property_from_name (ilist_obj, "Item");
  if (item_prop == NULL)
  {
    ERROR ("dotnet plugin: dotnet_object_get_property_from_name (\"Item\") failed.");
    return (-5);
  }

  method = mono_property_get_get_method (count_prop);
  tmp_obj = mono_runtime_invoke (method, ilist_obj, /* params = */ NULL, /* exception = */ NULL);
  if (tmp_obj == NULL)
  {
    ERROR ("dotnet plugin: mono_runtime_invoke failed.");
    return (-6);
  }
  DEBUG ("dotnet plugin: Type of the Count property is \"%s\".",
      mono_class_get_name (mono_object_get_class (tmp_obj)));

  vl->values_len = (int) *((int32_t *) mono_object_unbox (tmp_obj));
  DEBUG ("dotnet plugin: vl->values_len = %i;", vl->values_len);

  if (vl->values_len <= 0)
  {
    vl->values_len = 0;
    return (-7);
  }

  vl->values = calloc ((size_t) vl->values_len, sizeof (*vl->values));
  if (vl->values == NULL)
  {
    vl->values_len = 0;
    return (-8);
  }

  for (i = 0; i < ((int32_t) vl->values_len); i++)
  {
    void *args[2] = { &i, NULL };
    MonoObject *value_obj;
    const char *class_name;

    method = mono_property_get_get_method (item_prop);
    value_obj = mono_runtime_invoke (method, ilist_obj, /* params = */ args, /* exception = */ NULL);
    if (value_obj == NULL)
    {
      ERROR ("dotnet plugin: Item(%i) failed.", i);
      return (-7);
    }

    class_name = mono_class_get_name (mono_object_get_class (value_obj));
    if (strcmp ("gaugeValue", class_name) == 0)
    {
      MonoObject *gauge_obj;

      method = dotnet_object_method_get_from_name (value_obj, "toDouble", /* nargs = */ 0);
      if (method == NULL)
        return (-9);

      gauge_obj = mono_runtime_invoke (method, value_obj, /* args = */ NULL, /* exception = */ NULL);
      if (gauge_obj == NULL)
        return (-10);

      vl->values[i].gauge = (gauge_t) *((double *) mono_object_unbox (gauge_obj));
      DEBUG ("dotnet plugin: Gauge value %"PRIi32" = %g;",
          i, vl->values[i].gauge);
    }
    else if (strcmp ("deriveValue", class_name) == 0)
    {
      MonoObject *derive_obj;

      method = dotnet_object_method_get_from_name (value_obj, "toLong", /* nargs = */ 0);
      if (method == NULL)
        return (-9);

      derive_obj = mono_runtime_invoke (method, value_obj, /* args = */ NULL, /* exception = */ NULL);
      if (derive_obj == NULL)
        return (-10);

      vl->values[i].derive = (derive_t) *((int64_t *) mono_object_unbox (derive_obj));
      DEBUG ("dotnet plugin: Derive value %"PRIi32" = %"PRIi64";",
          i, vl->values[i].derive);
    }
  }

  return (0);
} /* }}} int dotnet_object_method_get_values */

/*
 * Functions exposed to .Net
 */
static int dotnet_log (int severity, MonoString *message) /* {{{ */
{
  char *tmp = mono_string_to_utf8 (message);

  DEBUG ("dotnet_log (severity = %i, message = \"%s\");", severity, tmp);

  return (0);
} /* }}} int dotnet_log */

static int dotnet_register_read (MonoString *name, MonoObject *obj) /* {{{ */
{
  user_data_t ud;
  dotnet_callback_info_t *ci;

  MonoClass *class;
  MonoMethod *method;

  /* Sanity checks: Make sure this object actually has the required method. */
  class = mono_object_get_class (obj);
  if (class == NULL)
  {
    ERROR ("dotnet plugin: mono_object_get_class failed.");
    return (-1);
  }

  method = mono_class_get_method_from_name (class,
      /* name = */ "read", /* param count = */ 0);
  if (method == NULL)
  {
    ERROR ("dotnet plugin: mono_class_get_method_from_name failed.");
    return (-1);
  }

  ci = malloc (sizeof (*ci));
  if (ci == NULL)
  {
    ERROR ("dotnet plugin: malloc failed.");
    return (-1);
  }
  memset (ci, 0, sizeof (*ci));

  ci->domain_id = mono_domain_get_id (mono_domain_get ());
  ci->obj_handle = mono_gchandle_new (obj, /* pinned = */ 1);

  ud.data = ci;
  ud.free_func = dotnet_callback_info_free;

  plugin_register_complex_read (/* group = */ "dotnet",
      /* name      = */ mono_string_to_utf8 (name),
      /* callback  = */ dotnet_read,
      /* interval  = */ NULL,
      /* user data = */ &ud);

  return (0);
} /* }}} int dotnet_register_read */

static int dotnet_dispatch_values (MonoObject *obj) /* {{{ */
{
  value_list_t vl = VALUE_LIST_INIT;
  int status;

  status = dotnet_object_method_get_string (obj, "getHost", vl.host, sizeof (vl.host));
  if (status != 0)
    return (status);

  status = dotnet_object_method_get_string (obj, "getPlugin", vl.plugin, sizeof (vl.plugin));
  if (status != 0)
    return (status);

  status = dotnet_object_method_get_string (obj, "getPluginInstance", vl.plugin_instance, sizeof (vl.plugin_instance));
  if (status != 0)
    return (status);

  status = dotnet_object_method_get_string (obj, "getType", vl.type, sizeof (vl.type));
  if (status != 0)
    return (status);

  status = dotnet_object_method_get_string (obj, "getTypeInstance", vl.type_instance, sizeof (vl.type_instance));
  if (status != 0)
    return (status);

  status = dotnet_object_method_get_cdtime (obj, "getInterval", &vl.interval);
  if (status != 0)
    return (status);

  status = dotnet_object_method_get_cdtime (obj, "getTime", &vl.time);
  if (status != 0)
    return (status);

  status = dotnet_object_method_get_values (obj, &vl);
  if (status != 0)
    return (status);

  status = plugin_dispatch_values (&vl);
  sfree (vl.values);

  return (status);
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

  mono_add_internal_call ("CollectdAPI.Collectd::log", dotnet_log);
  mono_add_internal_call ("CollectdAPI.Collectd::registerRead", dotnet_register_read);
  mono_add_internal_call ("CollectdAPI.Collectd::dispatchValues", dotnet_dispatch_values);

  plugin_register_complex_config ("dotnet", dotnet_config);
} /* void module_register */

/* vim: set sw=2 sts=2 et fdm=marker : */
