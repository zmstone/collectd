using CollectdAPI;

public class EmbedTest
{
	public EmbedTest ()
	{
		Collectd.LogInfo ("EmbedTest plugin: Initializing the module.");
		Collectd.RegisterRead ("EmbedTest", new CollectdReadCallback (read));
	}

	public int read ()
	{
		ValueList vl = new ValueList ("localhost", "dotnet", "", "gauge", "test");

		vl.Interval = 10.0;
		vl.AddValue (new GaugeValue (31337));

		Collectd.LogDebug ("vl: " + vl);

		Collectd.DispatchValues (vl);
		return (0);
	}
}

