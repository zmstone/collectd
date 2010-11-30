/**
 * collectd - bindings/csharp/collectd.cs
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

using System;
using System.Collections;
using System.Runtime.CompilerServices;

namespace CollectdAPI
{
  public interface IRead /* {{{ */
  {
    int read ();
  } /* }}} interface IRead */

  public class Collectd /* {{{ */
  {
    [MethodImplAttribute(MethodImplOptions.InternalCall)]
      public extern static int log (int severity, string message);

    [MethodImplAttribute(MethodImplOptions.InternalCall)]
      public extern static int registerRead (string name, IRead obj);

    [MethodImplAttribute(MethodImplOptions.InternalCall)]
      public extern static int dispatchValues (ValueList vl);
  } /* }}} class Collectd */

  public abstract class Value /* {{{ */
  {
    public abstract long toLong ();
    public abstract double toDouble ();
  } /* }}} class Value */

  public class gaugeValue: Value /* {{{ */
  {
    private double _value;

    public gaugeValue (double v)
    {
      this._value = v;
    }

    public override long toLong () { return ((long) this._value); }
    public override double toDouble () { return (this._value); }
  } /* }}} class gaugeValue */

  public class deriveValue: Value /* {{{ */
  {
    private long _value;

    public deriveValue (long v)
    {
      this._value = v;
    }

    public override long toLong () { return (this._value); }
    public override double toDouble () { return ((double) this._value); }
  } /* }}} class deriveValue */

  public class Identifier /* {{{ */
  {
    protected string _host;
    protected string _plugin;
    protected string _pluginInstance;
    protected string _type;
    protected string _typeInstance;

    public Identifier (string host,
        string plugin, string pluginInstance,
        string type, string typeInstance)
    {
      this._host = host;
      this._plugin = plugin;
      this._pluginInstance = pluginInstance;
      this._type = type;
      this._typeInstance = typeInstance;
    } /* Identifier() */

    public string getHost           () { return (this._host);           }
    public string getPlugin         () { return (this._plugin);         }
    public string getPluginInstance () { return (this._pluginInstance); }
    public string getType           () { return (this._type);           }
    public string getTypeInstance   () { return (this._typeInstance);   }

    public void setHost           (string s) { this._host           = s; }
    public void setPlugin         (string s) { this._plugin         = s; }
    public void setPluginInstance (string s) { this._pluginInstance = s; }
    public void setType           (string s) { this._type           = s; }
    public void setTypeInstance   (string s) { this._typeInstance   = s; }
  } /* }}} class Identifier */

  public class ValueList: Identifier /* {{{ */
  {
    protected double _time;
    protected double _interval;
    protected IList  _values;
    
    public ValueList (string host,
        string plugin, string pluginInstance,
        string type, string typeInstance)
      :base (host, plugin, pluginInstance, type, typeInstance)
    {
      this._interval = 10.0;
      this._values = new ArrayList ();
      this.setTime (DateTime.Now);
    }

    public IList getValues ()
    {
      return (this._values);
    }

    public void setValues (IList values)
    {
      this._values = new ArrayList (values);
    }

    public void addValue (Value v)
    {
      this._values.Add (v);
    }

    public void clearValues ()
    {
      this._values.Clear ();
    }

    public double getInterval ()
    {
      return (this._interval);
    }

    public void setInterval (double interval)
    {
      if (interval > 0.0)
        this._interval = interval;
    }

    public double getTime ()
    {
      return (this._time);
    }

    public void setTime (DateTime dt)
    {
      DateTime dtBase = new DateTime (1970,1,1,0,0,0);
      TimeSpan tsDiff = dt.ToUniversalTime () - dtBase;

      this._time = (double) tsDiff.TotalSeconds;
    }

    public void setTime (double t)
    {
      if (t > 0.0)
        this._time = t;
    }
  } /* }}} class ValueList */
}

/* vim: set sw=2 sts=2 et fdm=marker : */
