using CollectdAPI;

public class EmbedTest: IRead
{
	public EmbedTest ()
	{
		System.Console.WriteLine ("EmbedTest ();");
		Collectd.registerRead ("EmbedTest::read", this);
	}

	public int read ()
	{
		ValueList vl = new ValueList ("host", "plugin", "pinst", "type", "tinst");

		vl.setInterval (10.0);
		vl.addValue (new gaugeValue (3.1337));

		System.Console.WriteLine ("vl.getHost () = "
				+ vl.getHost ());

		Collectd.dispatchValues (vl);
		return (0);
	}

	public static void Main()
	{
		System.Console.WriteLine("Hello, World!");
	}
}

