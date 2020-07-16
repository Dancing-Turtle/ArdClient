package modification;

import haven.AuthClient;
import haven.Coord;
import haven.Resource;
import haven.Session;
import haven.Tex;
import haven.TexI;
import haven.Utils;
import haven.Widget;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class configuration {

	public static boolean customTitleBoolean = Utils.getprefb("custom-title-bol", false);
	public static String defaultCustomTitleName(String name) {
		return "♂" + name + "♂: ♂right version♂";
	}
	public static String defaultTitle = "Ardennes Hafen Revived " + "1.03: Reign of the Toad King";
	public static String defaultCustomTitle = "♂right version♂: You turn ♂Leatherman♂";
	public static String defaultUtilsCustomTitle = Utils.getpref("custom-title", defaultCustomTitle);
	public static String tittleCheck(Session sess) {
		String name, title;

		if (sess == null)
			name = "";
		else
			name = " \u2013 " + sess.username;

		if (configuration.customTitleBoolean)
			title = configuration.defaultUtilsCustomTitle;
		else
			title = defaultTitle;

		return title + name;
	}

	public static boolean defaultUtilsCustomLoginScreenBgBoolean = Utils.getprefb("custom-login-background-bol", false);
	public static String defaultCustomLoginScreenBg = "modification/loginscr.png";
	public static String defaultUtilsCustomLoginScreenBg = Utils.getpref("custom-login-background", defaultCustomLoginScreenBg);
	public static Tex bgCheck() {
		Tex bg;
		if (defaultUtilsCustomLoginScreenBgBoolean)
			bg = configuration.imageToTex(defaultUtilsCustomLoginScreenBg, 800, 600, Resource.loadtex("gfx/loginscr"));
		else bg = Resource.loadtex("gfx/loginscr");
		return bg;
	}

	public static boolean autoclick = Utils.getprefb("autoclick", false);

	public static boolean statustooltip = Utils.getprefb("statustooltip", false);

	public static boolean newCropStageOverlay = Utils.getprefb("newCropStageOverlay", false);

	public static boolean newQuickSlotWdg = Utils.getprefb("newQuickSlotWdg", false);

	public static boolean scaletree = Utils.getprefb("scaletree", false);
	public static int scaletreeint = Utils.getprefi("scaletreeint", 25);

	public static String[] customMenuGrid = new String[] {Utils.getpref("customMenuGrid0", "6"), Utils.getpref("customMenuGrid1", "4")};
	public static Coord getMenuGrid() {
		return new Coord(Integer.parseInt(configuration.customMenuGrid[0]), Integer.parseInt(configuration.customMenuGrid[1]));
	}

	public static boolean logging = Utils.getprefb("msglogging", false);      //allow log in console
	public static boolean loadLog = false;

	public static boolean msg_log_skip_boolean = true;     //allow chosen skip
	public static ArrayList<String> msg_log_skip = new ArrayList<String>() {{       //chosen msg
		addAll(Arrays.asList("glut", "click"));
	}};

	public static String serverSender = "_SERVER_MSG";
	public static String clientSender = "_CLIENT_MSG";
	private static String oldStackTraceElementClass = "";
	private static String oldStackTraceElementMethod = "";
	private static int oldStackTraceElementLine = -1;
	private static String oldSender = "";


	public static void sysPrintStackTrace(String text) {
		StackTraceElement[] stackTraceElements = new Throwable().getStackTrace();
		int stackTraceElementsLength = stackTraceElements.length;

		System.out.print(text + " || ");
		for (int i = 1; i < stackTraceElementsLength; i++) {
			System.out.print("{" + stackTraceElements[i].getClassName() + "(" + stackTraceElements[i].getMethodName() + ")(" + stackTraceElements[i].getLineNumber() + ")");
			if (i != stackTraceElementsLength - 1) System.out.print(">");
		}

		System.out.println();
	}

	public static void Syslog(String who, Widget widget, int id, String msg, Object... args) {
		StackTraceElement[] stackTraceElements = new Throwable().getStackTrace();
		int stackTraceElementsLength = stackTraceElements.length;

		int max_wdg_length = 40;
		int max_msg_length = 10;

		boolean skip_log = false;
		for (String s : msg_log_skip) {
			if (s.equals(msg) && msg_log_skip_boolean) skip_log = true;
		}

		if (stackTraceElements[1].getMethodName().equals("uimsg")) {
			oldStackTraceElementClass = stackTraceElements[2].getClassName();
			oldStackTraceElementMethod = stackTraceElements[2].getMethodName();
			oldStackTraceElementLine = stackTraceElements[2].getLineNumber();
		} else {
			oldStackTraceElementClass = "";
			oldStackTraceElementMethod = "";
			oldStackTraceElementLine = -1;
		}
		oldSender = who;

		if (!skip_log && logging) {
			System.out.print("[" + LocalTime.now() + "]");
			//System.out.print(" || ");
			/*for (int i = 1; i < stackTraceElementsLength; i++) {
				System.out.print("{" + stackTraceElements[i].getClassName() + "(" + stackTraceElements[i].getMethodName() + ")(" + stackTraceElements[i].getLineNumber() + ")");
				if (i != stackTraceElementsLength - 1) System.out.print(">");
			}*/

			System.out.print(" || " + who);
			System.out.print(" || " + widget + "(" + id + ") ");

			int a;
			if (id == -1) a = 1;
			else if ((id / 10) == 0) a = 0;
			else if ((id / 10) < 10) a = 1;
			else a = 2;

			/*if (widget != null)
				for (int i = widget.toString().length() + a; i < max_wdg_length; i++)
					System.out.print(" ");
			else
				for (int i = 4 + a; i < max_wdg_length; i++)
					System.out.print(" ");*/
			System.out.print("|| " + msg + " ");
			/*for (int i = msg.length(); i < max_msg_length; i++)
				System.out.print(" ");*/
			System.out.print("|| [" + args.length + "]:");


			try {
				for (int i = 0; i < args.length; i++) {
					if (args[i] instanceof AuthClient.NativeCred) {
						AuthClient.NativeCred arg = (AuthClient.NativeCred) args[i];
						System.out.print("{(AuthClient.NativeCred):" + arg.name() + "}");
					} else if (args[i] instanceof Integer) {
						System.out.print("i{" + args[i] + "}");
					} else if (args[i] instanceof String) {
						System.out.print("s{" + args[i] + "}");
					} else if (args[i] instanceof Boolean) {
						System.out.print("b{" + args[i] + "}");
					} else if (args[i] instanceof Coord) {
						Coord coord = (Coord) args[i];
						System.out.print("{x:" + coord.x + ", y:" + coord.y + "}");
					} else {
						if (stackTraceElements[1].getMethodName().equals("wdgmsg"))
							System.out.print("{" + args[i].getClass().getName() + ":" + args[i] + "}");
						else
							System.out.print("{" + args[i] + "}");
					}
					if (i != args.length - 1) System.out.print(", ");
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
			System.out.println();
		}
	}

	public static void SyslogRemote(String who, Widget widget, int id, String name, String type, int parent, Object[] pargs, Object... cargs) {
		StackTraceElement[] stackTraceElements = new Throwable().getStackTrace();
		int stackTraceElementsLength = stackTraceElements.length;

		boolean skip_log = false;
		for (String s : msg_log_skip) {
			if (s.equals(name) && msg_log_skip_boolean) skip_log = true;
		}

		if (stackTraceElements[1].getMethodName().equals("run") && oldSender.equals(serverSender)) {
			if (oldStackTraceElementClass.equals(stackTraceElements[1].getClassName()) &&
					oldStackTraceElementMethod.equals(oldStackTraceElementMethod = stackTraceElements[1].getMethodName()) &&
					oldStackTraceElementLine == stackTraceElements[1].getLineNumber() - 3)
				skip_log = true;
		} else {
			oldStackTraceElementClass = "";
			oldStackTraceElementMethod = "";
			oldStackTraceElementLine = -1;
		}
		oldSender = who;

		if (!skip_log && logging) {
			System.out.print("[" + LocalTime.now() + "]");
			System.out.print(" || ");
			for (int i = 1; i < stackTraceElementsLength; i++) {
				System.out.print("{" + stackTraceElements[i].getClassName() + "(" + stackTraceElements[i].getMethodName() + ")(" + stackTraceElements[i].getLineNumber() + ")");
				if (i != stackTraceElementsLength - 1) System.out.print(">");
			}
			System.out.print(" || " + who);
			System.out.print(" || " + widget + "(" + id + ") ");

			if (name != null) System.out.print(" || " + name);
			if (type != null) System.out.print(" || " + type);
			if (parent != -1) System.out.print(" || " + parent);

			argsMethod(pargs);
			argsMethod(cargs);

			System.out.println();
		}
	}

	private static void argsMethod(Object[] pargs) {
		if (pargs != null) {
			System.out.print(" || [" + pargs.length + "]:");
			for (int i = 1; i < pargs.length; i++) {
				System.out.print("{" + pargs[i] + "}");
				if (i != pargs.length - 1) System.out.print(", ");
			}
		}
	}

	public static BufferedImage scaleImage(BufferedImage before, int scale) {
		try {
			int w = before.getWidth();
			int h = before.getHeight();
			// Create a new image of the proper size
			int w2 = (int) (w * scale);
			int h2 = (int) (h * scale);
			BufferedImage after = new BufferedImage(w2, h2, BufferedImage.TYPE_INT_ARGB);
			AffineTransform scaleInstance = AffineTransform.getScaleInstance(scale, scale);
			AffineTransformOp scaleOp = new AffineTransformOp(scaleInstance, AffineTransformOp.TYPE_BILINEAR);

			scaleOp.filter(before, after);
			return after;
		} catch (Exception e) {
			e.printStackTrace();
			return before;
		}
	}

	public static BufferedImage scaleImage(BufferedImage before, int newWidth, int newHeight) {
		try {
			int w = before.getWidth();
			int h = before.getHeight();
			// Create a new image of the proper size
			int w2 = newWidth;
			int h2 = newHeight;
			double scale1 = (double) w2 / w;
			double scale2 = (double) h2 / h;
			BufferedImage after = new BufferedImage(w2, h2, BufferedImage.TYPE_INT_RGB);
			AffineTransform scaleInstance = AffineTransform.getScaleInstance(scale1, scale2);
			AffineTransformOp scaleOp = new AffineTransformOp(scaleInstance, AffineTransformOp.TYPE_BILINEAR);

			scaleOp.filter(before, after);
			return after;
		} catch (Exception e) {
			e.printStackTrace();
			return before;
		}
	}

	private static Tex getTex(String name) throws IOException {
		BufferedImage in;
		File img = new File(name);
		in = ImageIO.read(img);

		BufferedImage newImage = new BufferedImage(in.getWidth(), in.getHeight(), BufferedImage.TYPE_INT_ARGB);

		Graphics2D g = newImage.createGraphics();
		g.drawImage(in, 0, 0, null);
		g.dispose();
		Tex tex = new TexI(newImage);
		return tex;
	}

	private static Tex getTex(String name, int width, int height) throws IOException {
		BufferedImage in;
		File img = new File(name);
		in = ImageIO.read(img);

		BufferedImage newImage = new BufferedImage(in.getWidth(), in.getHeight(), BufferedImage.TYPE_INT_ARGB);


		Graphics2D g = newImage.createGraphics();
		g.drawImage(in, 0, 0, null);
		g.dispose();

		Tex tex = new TexI(scaleImage(newImage, width, height));
		return tex;
	}

	public static Tex imageToTex(String name) {

		BufferedImage in;
		try {
			return getTex(name);
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	public static Tex imageToTex(String name, Tex defaultTex) {

		BufferedImage in;
		try {
			return getTex(name);
		} catch (Exception e) {
			e.printStackTrace();
			return defaultTex;
		}
	}

	public static Tex imageToTex(String name, int width, int height, Tex defaultTex) {

		BufferedImage in;
		try {
			return getTex(name, width, height);
		} catch (Exception e) {
			e.printStackTrace();
			return defaultTex;
		}
	}

	public static ArrayList<String> findFiles(String dir, List<String> exts) {
		try {
			File file = new File(dir);

			ArrayList<String> list = new ArrayList<String>();
			if (!file.exists()) System.out.println(dir + " folder not exists");
			for (String ext : exts) {
				File[] listFiles = file.listFiles(new MyFileNameFilter(ext));
				if (listFiles.length == 0) {
					//System.out.println(dir + " не содержит файлов с расширением " + ext);
				} else {
					for (File f : listFiles) {
						list.add(dir + File.separator + f.getName());
						//System.out.println("File: " + dir + File.separator + f.getName());
					}
				}
			}
			return list;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	public static class MyFileNameFilter implements FilenameFilter{

		private String ext;

		public MyFileNameFilter(String ext){
			this.ext = ext.toLowerCase();
		}
		@Override
		public boolean accept(File dir, String name) {
			return name.toLowerCase().endsWith(ext);
		}
	}
}
