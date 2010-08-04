/**
 * collectd - src/wpar.c
 * Copyright (C) 2010  Manuel Sanmartin
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
 *   Manuel Sanmartin <manuel.luis at gmail.com>
 **/

#include "collectd.h"
#include "common.h"
#include "plugin.h"

#if !HAVE_PERFSTAT
# error "No applicable input method."
#endif

#include <sys/proc.h> /* AIX 5 */
#include <sys/protosw.h>
#include <libperfstat.h>

static int pagesize;
static int nwpar = -1;
static int pwpar;
static perfstat_wpar_total_t *wpar_total;
static perfstat_memory_total_wpar_t wmemory;
static perfstat_cpu_total_wpar_t wcpu;

static int wpar_init(void) /* {{{ */
{
  pagesize = getpagesize ();
  return (0);
} /* }}} int wpar_init */

static void memory_submit (const char *plugin_instance, const char *type_instance, gauge_t value) /* {{{ */
{
  value_t values[1];
  value_list_t vl = VALUE_LIST_INIT;

  values[0].gauge = value;

  vl.values = values;
  vl.values_len = 1;
  sstrncpy (vl.host, hostname_g, sizeof (vl.host));
  sstrncpy (vl.plugin, "wpar", sizeof (vl.plugin));
  sstrncpy (vl.plugin_instance, plugin_instance, sizeof (vl.plugin_instance));
  sstrncpy (vl.type, "memory", sizeof (vl.type));
  sstrncpy (vl.type_instance, type_instance, sizeof (vl.type_instance));

  plugin_dispatch_values (&vl);
} /* }}} void memory_submit */

static void cpu_submit (const char *plugin_instance, const char *type_instance, counter_t value) /* {{{ */
{
  value_t values[1];
  value_list_t vl = VALUE_LIST_INIT;

  values[0].counter = value;

  vl.values = values;
  vl.values_len = 1;
  sstrncpy (vl.host, hostname_g, sizeof (vl.host));
  sstrncpy (vl.plugin, "wpar", sizeof (vl.plugin));
  sstrncpy (vl.plugin_instance, plugin_instance, sizeof (vl.plugin_instance));
  sstrncpy (vl.type, "cpu", sizeof (vl.type));
  sstrncpy (vl.type_instance, type_instance, sizeof (vl.type_instance));

  plugin_dispatch_values (&vl);
} /* }}} void cpu_submit */

static void load_submit (const char *plugin_instance, gauge_t snum, gauge_t mnum, gauge_t lnum) /* {{{ */
{
  value_t values[3];
  value_list_t vl = VALUE_LIST_INIT;

  values[0].gauge = snum;
  values[1].gauge = mnum;
  values[2].gauge = lnum;

  vl.values = values;
  vl.values_len = STATIC_ARRAY_SIZE (values);
  sstrncpy (vl.host, hostname_g, sizeof (vl.host));
  sstrncpy (vl.plugin, "load", sizeof (vl.plugin));
  sstrncpy (vl.plugin_instance, plugin_instance, sizeof (vl.plugin_instance));
  sstrncpy (vl.type, "load", sizeof (vl.type));

  plugin_dispatch_values (&vl);
} /* }}} void load_submit */

static int wpar_read (void) /* {{{ */
{
  int i,wpars;
  float snum, mnum, lnum;
  perfstat_id_wpar_t id_wpar;

  nwpar = perfstat_wpar_total(NULL, NULL, sizeof(perfstat_wpar_total_t), 0);
  if (nwpar == -1)
  {
    char errbuf[1024];
    WARNING ("wpar plugin: perfstat_wpar_total: %s",
        sstrerror (errno, errbuf, sizeof (errbuf)));
    return (-1);
  }

  if (pwpar != nwpar ||  wpar_total == NULL)
  {
    if (wpar_total != NULL)
      free(wpar_total);
    wpar_total = malloc(nwpar * sizeof(perfstat_wpar_total_t));
  }
  pwpar = nwpar;

  bzero(&id_wpar, sizeof(perfstat_id_wpar_t));
  id_wpar.spec = WPARID;
  id_wpar.u.wpar_id = FIRST_WPARID;
  if ((wpars = perfstat_wpar_total(&id_wpar, wpar_total, sizeof(perfstat_wpar_total_t), nwpar)) < 0)
  {
    char errbuf[1024];
    WARNING ("cpu plugin: perfstat_wpar_total: %s",
        sstrerror (errno, errbuf, sizeof (errbuf)));
    return (-1);
  }

  for (i = 0; i < wpars; i++)
  {
    char *wname = wpar_total[i].name;

    bzero(&id_wpar, sizeof(perfstat_id_wpar_t));
    id_wpar.spec = WPARID;
    id_wpar.u.wpar_id = wpar_total[i].wpar_id;

    if (perfstat_memory_total_wpar(&id_wpar, &wmemory, sizeof(perfstat_memory_total_wpar_t), 1) < 0)
    {
      char errbuf[1024];
      WARNING ("memory plugin: perfstat_memory_total_wpar failed: %s",
          sstrerror (errno, errbuf, sizeof (errbuf)));
      return (-1);
    }
    memory_submit (wname, "used",   wmemory.real_inuse * pagesize);
    memory_submit (wname, "free",   wmemory.real_free * pagesize);
    memory_submit (wname, "cached", wmemory.numperm * pagesize);
    memory_submit (wname, "total",  wmemory.real_total * pagesize);


    if (perfstat_cpu_total_wpar(&id_wpar, &wcpu, sizeof(perfstat_cpu_total_wpar_t), 1) < 0)
    {
      char errbuf[1024];
      WARNING ("memory plugin: perfstat_cpu_total_wpar failed: %s",
          sstrerror (errno, errbuf, sizeof (errbuf)));
      return (-1);
    }

    snum = (float)wcpu.loadavg[0]/(float)(1<<SBITS);
    mnum = (float)wcpu.loadavg[1]/(float)(1<<SBITS);
    lnum = (float)wcpu.loadavg[2]/(float)(1<<SBITS);

    load_submit (wname, snum, mnum, lnum);

    cpu_submit (wname, "idle",   (counter_t) wcpu.pidle);
    cpu_submit (wname, "system", (counter_t) wcpu.psys);
    cpu_submit (wname, "user",   (counter_t) wcpu.puser);
    cpu_submit (wname, "wait",   (counter_t) wcpu.pwait);
  }

  return (0);
} /* }}} int wpar_read */

void module_register (void)
{
  plugin_register_init ("wpar", wpar_init);
  plugin_register_read ("wpar", wpar_read);
}

/* vim: set sw=2 sts=2 et fdm=marker : */
