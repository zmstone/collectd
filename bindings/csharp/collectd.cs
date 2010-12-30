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
using System.Text;
using System.Runtime.InteropServices;

namespace CollectdAPI
{
  public delegate int CollectdInitCallback ();
  public delegate int CollectdReadCallback ();
  public delegate int CollectdWriteCallback (ValueList vl);

  public interface IValue /* {{{ */
  {
    long ToLong ();
    double ToDouble ();
  } /* }}} class IValue */

  public class GaugeValue: IValue /* {{{ */
  {
    private double _value;

    public GaugeValue (double v)
    {
      this._value = v;
    }

    public long ToLong () { return ((long) this._value); }
    public double ToDouble () { return (this._value); }

    public override string ToString ()
    {
      return (this._value.ToString ());
    }
  } /* }}} class GaugeValue */

  public class DeriveValue: IValue /* {{{ */
  {
    private long _value;

    public DeriveValue (long v)
    {
      this._value = v;
    }

    public long ToLong () { return (this._value); }
    public double ToDouble () { return ((double) this._value); }

    public override string ToString ()
    {
      return (this._value.ToString ());
    }
  } /* }}} class DeriveValue */

  public class Identifier /* {{{ */
  {
    [MarshalAs (UnmanagedType.LPStr)]
    protected string _host;
    [MarshalAs (UnmanagedType.LPStr)]
    protected string _plugin;
    [MarshalAs (UnmanagedType.LPStr)]
    protected string _pluginInstance;
    [MarshalAs (UnmanagedType.LPStr)]
    protected string _type;
    [MarshalAs (UnmanagedType.LPStr)]
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

    public Identifier (Identifier id)
    {
      this._host           = id._host;
      this._plugin         = id._plugin;
      this._pluginInstance = id._pluginInstance;
      this._type           = id._type;
      this._typeInstance   = id._typeInstance;
    } /* Identifier() */

    public string Host /* {{{ */
    {
      get
      {
        return (this._host);
      }
      set
      {
        if ((value == null) || (value.Length < 1))
          throw new ArgumentException ();
        this._host = value;
      }
    } /* }}} */

    public string Plugin /* {{{ */
    {
      get
      {
        return (this._plugin);
      }
      set
      {
        if ((value == null) || (value.Length < 1))
          throw new ArgumentException ();
        this._plugin = value;
      }
    } /* }}} string Plugin */

    public string PluginInstance /* {{{ */
    {
      get
      {
        return (this._pluginInstance);
      }
      set
      {
        if (value == null)
          this._pluginInstance = "";
        else
          this._pluginInstance = value;
      }
    } /* }}} string PluginInstance */

    public string Type /* {{{ */
    {
      get
      {
        return (this._type);
      }
      set
      {
        if ((value == null) || (value.Length < 1))
          throw new ArgumentException ();
        this._type = value;
      }
    } /* }}} string Type */

    public string TypeInstance /* {{{ */
    {
      get
      {
        return (this._typeInstance);
      }
      set
      {
        if (value == null)
          this._typeInstance = "";
        else
          this._typeInstance = value;
      }
    } /* }}} string TypeInstance */

    public override string ToString ()
    {
      StringBuilder sb = new StringBuilder ();

      sb.Append (this._host).Append ("/").Append (this._plugin);
      if (!String.Equals ("", this._pluginInstance))
        sb.Append ("-").Append (this._pluginInstance);
      sb.Append ("/").Append (this._type);
      if (!String.Equals ("", this._typeInstance))
        sb.Append ("-").Append (this._typeInstance);

      return (sb.ToString ());
    } /* string ToString */
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
      this._time = 0.0;
    }

    internal ValueList (value_data_s vd,
        int values_len, value_u[] values, int[] values_types)
      :base (vd.host,
          vd.plugin, vd.plugin_instance,
          vd.type, vd.type_instance)
    {
      this._time = vd.time;
      this._interval = vd.interval;

      this._values = new ArrayList (values_len);
      for (int i = 0; i < values_len; i++)
      {
        IValue iv;

        if (values_types[i] == Collectd.DS_TYPE_GAUGE)
          iv = new GaugeValue (values[i].gauge);
        else
          iv = new DeriveValue (values[i].derive);

        this._values.Insert (i, iv);
      }
    }

    public ValueList (ValueList vl)
      :base (vl)
    {
      this._interval = vl._interval;
      this._values   = new ArrayList (vl._values);
      this._time     = vl._time;
    }

    public IList GetValues ()
    {
      return (this._values);
    }

    public void SetValues (IList values)
    {
      this._values = new ArrayList (values);
    }

    public void AddValue (IValue v)
    {
      this._values.Add (v);
    }

    public void ClearValues ()
    {
      this._values.Clear ();
    }

    public double Interval
    {
      get
      {
        return (this._interval);
      }
      set
      {
        if (value > 0.0)
          this._interval = value;
      }
    }

    public double Time
    {
      get
      {
        return (this._time);
      }
      set
      {
        if (value >= 0.0)
          this._time = value;
      }
    }

    public void SetTime (DateTime dt)
    {
      DateTime dtBase = new DateTime (1970,1,1,0,0,0);
      TimeSpan tsDiff = dt.ToUniversalTime () - dtBase;

      this._time = (double) tsDiff.TotalSeconds;
    }

    public override string ToString ()
    {
      StringBuilder sb = new StringBuilder ("{");

      sb.Append ("\"identifier\":\"").Append (base.ToString ()).Append ("\", ");
      sb.Append ("\"time\":").Append (this._time).Append (", ");
      sb.Append ("\"interval\":").Append (this._interval).Append (", ");
      sb.Append ("\"values\":[");

      for (int i = 0; i < this._values.Count; i++)
      {
        if (i != 0)
          sb.Append (",");
        sb.Append (this._values[i]);
      }

      sb.Append ("]}");
      return (sb.ToString ());
    } /* string ToString */
  } /* }}} class ValueList */

  [StructLayout (LayoutKind.Explicit)]
  struct value_u /* {{{ */
  {
    /* Emulate a union */
    [FieldOffset (0)] public double gauge;
    [FieldOffset (0)] public long   derive;
  } /* }}} struct value_u */

  [StructLayout (LayoutKind.Sequential)]
  struct value_data_s /* {{{ */
  {
    public double time;
    public double interval;
    [MarshalAs(UnmanagedType.LPStr)]
    public string host;
    [MarshalAs(UnmanagedType.LPStr)]
    public string plugin;
    [MarshalAs(UnmanagedType.LPStr)]
    public string plugin_instance;
    [MarshalAs(UnmanagedType.LPStr)]
    public string type;
    [MarshalAs(UnmanagedType.LPStr)]
    public string type_instance;

    /* Create a value_data_s structure from a ValueList object */
    public value_data_s (ValueList vl)
    {
      this.time = vl.Time;
      this.interval = vl.Interval;

      this.host = vl.Host;
      this.plugin = vl.Plugin;
      this.plugin_instance = vl.PluginInstance;
      this.type = vl.Type;
      this.type_instance = vl.TypeInstance;
    }
  } /* }}} struct value_data_s */

  internal class WriteMarshaler /* {{{ */
  {
    private CollectdWriteCallback _func;

    public WriteMarshaler (CollectdWriteCallback func)
    {
      this._func = func;
    }

    public int invoke (value_data_s vd,
        int values_len, value_u[] values, int[] values_types)
    {
      return (this._func (new ValueList (vd, values_len, values, values_types)));
    }
  } /* }}} class WriteMarshaler */

  /// <summary>
  /// This is the main class for interacting with collectd.
  /// </summary>
  /// <remarks>
  /// The functions exported by collectd are available from this class as
  /// public static methods.
  /// </remarks>
  public class Collectd /* {{{ */
  {
    private static Hashtable _initFunctions = new Hashtable ();
    private static Hashtable _readFunctions = new Hashtable ();
    private static Hashtable _writeFunctions = new Hashtable ();

    private delegate int MarshaledWriteCallback (value_data_s vd,
        int values_len,
        [MarshalAs(UnmanagedType.LPArray, SizeParamIndex=1)] value_u[] values,
        [MarshalAs(UnmanagedType.LPArray, SizeParamIndex=1)] int[] values_types);

    /* Const membes are static by default, so we're not allowed to specify that
     * again. */
    public const int DS_TYPE_COUNTER  = 0;
    public const int DS_TYPE_GAUGE    = 1;
    public const int DS_TYPE_DERIVE   = 2;
    public const int DS_TYPE_ABSOLUTE = 3;

    [DllImport("__Internal", EntryPoint="plugin_log")]
    private extern static int _log (
        [MarshalAs(UnmanagedType.SysInt)] int severity,
        [MarshalAs(UnmanagedType.LPStr)]  string message);

    [DllImport("__Internal", EntryPoint="dotnet_register_init")]
    private extern static int _registerInit (
        [MarshalAs(UnmanagedType.LPStr)] string name,
        CollectdInitCallback func);

    [DllImport("__Internal", EntryPoint="dotnet_register_read")]
    private extern static int _registerRead (
        [MarshalAs(UnmanagedType.LPStr)] string name,
        CollectdReadCallback func);

    [DllImport("__Internal", EntryPoint="dotnet_register_write")]
    private extern static int _registerWrite (
        [MarshalAs(UnmanagedType.LPStr)] string name,
        MarshaledWriteCallback func);

    [DllImport("__Internal", EntryPoint="dotnet_dispatch_values")]
    private extern static int _dispatchValues (ref value_data_s vd,
        int values_len,
        /* [MarshalAs(UnmanagedType.LPArray)] */ value_u[] values
        );

    /// <summary>Logs an error message.</summary>
    /// <remarks>Do not include a newline at the end.</remarks>
    public static int LogError (string message)
    {
      return (_log (3, message));
    }

    /// <summary>Logs a warning message.</summary>
    /// <remarks>Do not include a newline at the end.</remarks>
    public static int LogWarning (string message)
    {
      return (_log (4, message));
    }

    /// <summary>Logs a message with severity &quot;notice&quot;.</summary>
    /// <remarks>Do not include a newline at the end.</remarks>
    public static int LogNotice (string message)
    {
      return (_log (5, message));
    }

    /// <summary>Logs a message with severity &quot;info&quot;.</summary>
    /// <remarks>Do not include a newline at the end.</remarks>
    public static int LogInfo (string message)
    {
      return (_log (6, message));
    }

    /// <summary>Logs a debug message.</summary>
    /// <remarks>Do not include a newline at the end.</remarks>
    public static int LogDebug (string message)
    {
      return (_log (7, message));
    }

    /// <summary>Register an init callback.</summary>
    /// <param name="name">Name uniquely identifying the callback function.</param>
    public static int RegisterInit (string name, CollectdInitCallback func)
    {
      if (_initFunctions.Contains (name))
        return (-1);
      _initFunctions.Add (name, func);

      return (_registerInit (name, func));
    } /* int RegisterInit */

    /// <summary>Register a read callback.</summary>
    /// <param name="name">Name uniquely identifying the callback function.</param>
    public static int RegisterRead (string name, CollectdReadCallback func)
    {
      if (_readFunctions.Contains (name))
        return (-1);
      _readFunctions.Add (name, func);

      return (_registerRead (name, func));
    }

    public static int RegisterWrite (string name, CollectdWriteCallback func)
    {
      WriteMarshaler marshaler = new WriteMarshaler (func);

      /* Keep a copy of the object so it isn't garbage collected */
      if (_writeFunctions.Contains (name))
        return (-1);
      _writeFunctions.Add (name, marshaler);

      return (_registerWrite (name, new MarshaledWriteCallback (marshaler.invoke)));
    }

    /// <summary>Dispatches a <code>ValueList</code> to the daemon.</summary>
    public static int DispatchValues (ValueList vl)
    {
      IList        list       = vl.GetValues ();
      value_data_s vd         = new value_data_s (vl);
      int          values_len = list.Count;
      value_u[]    values     = new value_u[values_len];

      for (int i = 0; i < values_len; i++)
      {
        IValue v = list[i] as IValue;

        if (v is GaugeValue)
          values[i].gauge = v.ToDouble ();
        else
          values[i].derive = v.ToLong ();
      }

      return (_dispatchValues (ref vd, values_len, values));
    }
  } /* }}} class Collectd */
}

/* vim: set sw=2 sts=2 et fdm=marker : */
