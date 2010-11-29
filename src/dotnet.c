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

static MonoDomain *domain = NULL;

static int dotnet_log (int severity, MonoString *message) /* {{{ */
{
  char *tmp = mono_string_to_utf8 (message);

  DEBUG ("dotnet_log (severity = %i, message = \"%s\");", severity, tmp);

  return (0);
} /* }}} int dotnet_log */

static int dotnet_register_read (MonoString *name, MonoObject *obj) /* {{{ */
{
  char *name_utf8 = mono_string_to_utf8 (name);
  MonoClass *class;
  MonoMethod *method;

  DEBUG ("dotnet_register_read: name = \"%s\";", name_utf8);

  class = mono_object_get_class (obj);

  method = mono_class_get_method_from_name (mono_object_get_class (obj),
      /* name = */ "read", /* param count = */ 0);
  if (method == NULL)
  {
    ERROR ("dotnet_register_read: Can't find method read():int.");
    return (-1);
  }

  DEBUG ("dotnet_register_read: Class %s successfully registered.",
      mono_class_get_name (class));
  return (0);
} /* }}} int dotnet_register_read */

static int dotnet_load_class (const char *assembly_name, /* {{{ */
    const char *name_space, const char *class_name)
{
  MonoAssembly *assembly;
  MonoImage *image;
  MonoClass *class;
  MonoObject *obj;

  assembly = mono_domain_assembly_open (domain, assembly_name);
  if (assembly == NULL)
  {
    ERROR ("dotnet plugin: mono_domain_assembly_open (\"%s\") failed.",
        assembly_name);
    return (-1);
  }

  image = mono_assembly_get_image (assembly);
  if (image == NULL)
  {
    ERROR ("mono_assembly_get_image failed.");
    mono_jit_cleanup (domain);
    return (-1);
  }

  class = mono_class_from_name (image,
      (name_space != NULL) ? name_space : "",
      class_name);
  if (class == NULL)
  {
    ERROR ("dotnet plugin: Looking up class \"%s\" in assembly \"%s\" failed.",
        class_name, assembly_name);
    return (-1);
  }

  obj = mono_object_new (domain, class);
  if (obj == NULL)
  {
    ERROR ("Creating a \"%s\" object failed.", class_name);
    return (-1);
  }

  mono_runtime_object_init (obj);

  DEBUG ("dotnet plugin: Successfully created a \"%s\" object.", class_name);

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
  domain = mono_jit_init (PACKAGE_NAME);
  if (domain == NULL)
  {
    ERROR ("dotnet plugin: mono_jit_init failed.");
    return;
  }

  mono_add_internal_call ("CollectdAPI.Collectd::log", dotnet_log);
  mono_add_internal_call ("CollectdAPI.Collectd::registerRead", dotnet_register_read);

  plugin_register_complex_config ("dotnet", dotnet_config);
} /* void module_register */

/* vim: set sw=2 sts=2 et fdm=marker : */
