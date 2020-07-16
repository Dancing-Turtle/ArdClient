package ape;

import java.io.*;
import java.util.*;
import java.text.*;

public class Logging {
	public static PrintWriter
			out;

	static {
		try {
			File folder = new File("logs");
			if (!folder.exists()) folder.mkdir();

			String fn = "logs/log - Updater";
			new File(fn).createNewFile();
			out = new PrintWriter(new BufferedWriter(new FileWriter(fn)));
		} catch(Exception e) { e.printStackTrace(); }
	}

	public static void error(Exception e, boolean kill) {
		out.print("[ERROR] ");
		e.printStackTrace(out);
		out.flush();
		e.printStackTrace();

		if(kill)
			System.exit(1);
	}

	public static void log_nf(final String s) {
		out.print("[LOG] ");
		out.println(s);
		System.out.println("[LOG] " + s);
	}

	public static void flush() {
		out.flush();
	}

	public static void log(final String s) {
		out.print("[LOG] ");
		out.println(s);
		out.flush();
		System.out.println("[LOG] " + s);
	}
}