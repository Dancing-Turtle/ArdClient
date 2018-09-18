package haven.automation;

import haven.Button;
import haven.*;
import haven.Label;
import haven.Window;
import haven.purus.BotUtils;
import haven.purus.pbot.PBotAPI;

import java.awt.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import static haven.OCache.posres;

public class PepperGrinderRun extends Window implements Runnable {
	private Coord rc1, rc2;
	private ArrayList<Gob> crops = new ArrayList<Gob>();
	private ArrayList<Gob> tables = new ArrayList<Gob>();
	private boolean stopThread = false;
	private Label lblProg;
	private ArrayList<String> cropName = new ArrayList<String>();
	private ArrayList<String> seedName = new ArrayList<String>();
	private String trellis = "gfx/terobjs/plants/trellis";
	private boolean harvest = false;
	private boolean destroy = false;
	public Gob htable;
	private boolean replant = false;
	public Button stopBtn;
	public int x, y;
	private Gob chest, water, rowmarker, cauldron, barrel, hfire, grinder;
	private final int rowgap = 4200;
	private final int travel = 20000;
	public List<Gob> blacklist;
	private int section, direction;
	public Widget craftall;
	private Boolean boilmode = false;
	private Coord finalloc;

	public PepperGrinderRun(Coord rc1, Coord rc2, Gob grinder, int section, Gob hfire, int direction) {
		super(new Coord(120, 45), "Pepper Grinder");
		this.grinder = grinder;
		this.rc1 = rc1;
		this.rc2 = rc2;
		this.direction = direction;
		this.hfire = hfire;
		this.section = section;

		// Initialise arraylists

		lblProg = new Label("Initialising...");
		add(lblProg, new Coord(0, 35));

		stopBtn = new Button(120, "Stop") {
			@Override
			public void click() {
				stop();
			}
		};
		add(stopBtn, new Coord(0, 0));
	}

	public void run() {
		//section = 4;
		tables = Tables();
		BotUtils.sysMsg("Pepper Grinder Bot started!", Color.white);
		GameUI gui = gameui();
		if(gui.getwnd("Crafting")!= null)
			gui.getwnd("Crafting").close();
		gui.wdgmsg("act", "craft", "blackpepper");
		PBotAPI.waitForWindow("Crafting");
		Window crafting =gui.getwnd("Crafting");
		for (Widget a = crafting.lchild; a != null; a = a.prev) {
			for (Widget aa = a.lchild; aa != null; aa = aa.prev) {
				if (aa instanceof Button) {
					if (((Button) aa).text.text == "Craft All") {
						craftall = aa;
					}
				}
			}
		}

		if (stopThread) // Checks if aborted
			return;

while(tables.size()>0) {
	// Check if stamina is under 30%, drink if needed
	gui = HavenPanel.lui.root.findchild(GameUI.class);
	IMeter.Meter stam = gui.getmeter("stam", 0);
	if (stam.a <= 60) {
		lblProg.settext("Drinking");
		new Thread(new BeltDrink(gui), "BeltDrink").start();
		BotUtils.sleep(5000);
	}


	if (stopThread)
		return;


	if (section == 2) {
		lblProg.settext("Status - Moving");
		Gob player = gui.map.player();
		Coord location = player.rc.floor(posres);
		if (direction == 1) {
			x = location.x;
			y = location.y - rowgap;
		}
		if (direction == 2) {
			x = location.x;
			y = location.y + rowgap;
		}
		if (direction == 3) {
			x = location.x + rowgap;
			y = location.y;
		}
		if (direction == 4) {
			x = location.x - rowgap;
			y = location.y;
		}
		finalloc = new Coord(x, y);
		gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
		BotUtils.sleep(2000);
	} else if (section != 1 && section != 2) {
		lblProg.settext("Status - Moving");
		Gob player = gui.map.player();
		Coord location = player.rc.floor(posres);
		if (direction == 1) {
			x = location.x;
			y = location.y - ((rowgap * section) - rowgap);
		}
		if (direction == 2) {
			x = location.x;
			y = location.y + ((rowgap * section) - rowgap);
		}
		if (direction == 3) {
			x = location.x + ((rowgap * section) - rowgap);
			y = location.y;
		}
		if (direction == 4) {
			x = location.x - ((rowgap * section) - rowgap);
			y = location.y;
		}
		finalloc = new Coord(x, y);
		gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
		BotUtils.sleep(6000);
	}
	int finishtimeout = 0;
	while (BotUtils.invFreeSlots() > 0 && !stopThread) {
		finishtimeout++;
		if(finishtimeout > 10000)
			stopBtn.click();
		lblProg.settext("Status - Collecting");
		//while (BotUtils.findObjectByNames(10, "gfx/terobjs/htable") == null)
		//BotUtils.sleep(10);
		//BotUtils.sysLogAppend("tablecoutn : "+tables.size(), "white");
		while (htable == null) {
			//BotUtils.sysLogAppend("while loop", "white");
			for (Gob tablelol : tables) {
				//BotUtils.sysLogAppend("table loop", "white");
				if (tablelol.ols.size() > 0)
					htable = tablelol;
			}
		}
		BotUtils.sleep(500);
	//	BotUtils.sysLogAppend("Found table, clicking", "white");
		gui.map.wdgmsg("click", htable.sc, htable.rc.floor(posres), 3, 0, 0, (int) htable.id, htable.rc.floor(posres), 0, -1);
		//BotUtils.sysLogAppend("Found table, clicked", "white");
		BotUtils.waitForWindow("Herbalist Table");
		Window herbtable = gui.getwnd("Herbalist Table");
		for (Widget w = herbtable.lchild; w != null; w = w.prev) {
			if (w instanceof Inventory) {
				Inventory inv = (Inventory) w;
						List <WItem> items = BotUtils.getInventoryContents(inv);
					//	BotUtils.sysLogAppend("Inv size : "+items.size(),"white");
						for(WItem item : items)
							item.item.wdgmsg("transfer",Coord.z);
			}
		}
		herbtable.close();
		for(Gob gob : PBotAPI.getGobs()){
			if(gob == htable)
				if(gob.ols.size() == 0)
					tables.remove(htable);
		}
	//	BotUtils.sysLogAppend("Tables Size : "+tables.size(),"white");
		htable = null;
	}
if(BotUtils.invFreeSlots() == 0) {
	GItem pepperlol = gui.maininv.getItemPartial("Dried").item;
	java.util.List<WItem> pepper = gui.maininv.getIdenticalItems((pepperlol));
	if(section == 1) {
		lblProg.settext("Status - Going to Grind");
		gui.map.wdgmsg("click", hfire.sc, hfire.rc.floor(posres), 1, 0, 0, (int) hfire.id, hfire.rc.floor(posres), 0, -1);
		BotUtils.sleep(4000);
		gui.map.wdgmsg("click", grinder.sc, grinder.rc.floor(posres), 3, 0, 0, (int) grinder.id, grinder.rc.floor(posres), 0, -1);
		BotUtils.sleep(3000);
		BotUtils.sysLogAppend("Pepper size : "+pepper.size(),"white");
		int timeout = 0;
		while (gui.maininv.getIdenticalItems((pepperlol)).size() > 5) {
			timeout++;
			if(timeout > 5000) {
				gui.maininv.getItemPartial("Dried").item.wdgmsg("drop", Coord.z);
				timeout = 0;
			}
			while (gui.prog >= 0) {
				BotUtils.sleep(10);
				lblProg.settext("Status - Grinding");
			}
			if (PBotAPI.getStamina() > 50)
				((Button) craftall).click();
			else{
				lblProg.settext("Status - Drinking");
				new Thread(new BeltDrink(gui), "BeltDrink").start();
				BotUtils.sleep(5000);
			}
		}
		gui.map.wdgmsg("click", hfire.sc, hfire.rc.floor(posres), 1, 0, 0, (int) hfire.id, hfire.rc.floor(posres), 0, -1);
		BotUtils.sleep(2000);
	}
		else{
		lblProg.settext("Status - Going to Grind");
		gui.act("travel", "hearth");
		BotUtils.sleep(6000);
		gui.map.wdgmsg("click", grinder.sc, grinder.rc.floor(posres), 3, 0, 0, (int) grinder.id, grinder.rc.floor(posres), 0, -1);
		BotUtils.sleep(3000);
		BotUtils.sysLogAppend("Pepper size : "+pepper.size(),"white");
		int timeout = 0;
		while (gui.maininv.getIdenticalItems((pepperlol)).size() > 5) {
			timeout++;
			System.out.println("timeout : "+timeout);
			if(timeout > 10000) {
				gui.maininv.getItemPartial("Dried").item.wdgmsg("drop", Coord.z);
				timeout = 0;
			}
			while (gui.prog >= 0) {
				BotUtils.sleep(10);
				lblProg.settext("Status - Grinding");
			}
			if (PBotAPI.getStamina() > 50)
				((Button) craftall).click();
			else{
				lblProg.settext("Status - Drinking");
				new Thread(new BeltDrink(gui), "BeltDrink").start();
				BotUtils.sleep(5000);
			}
		}
		gui.map.wdgmsg("click", hfire.sc, hfire.rc.floor(posres), 1, 0, 0, (int) hfire.id, hfire.rc.floor(posres), 0, -1);
		BotUtils.sleep(2000);
	}
}
BotUtils.sysLogAppend("end of loop","white");
}
	// Update progression
		BotUtils.sysMsg("Grinder Bot finished!",Color.white);
		this.destroy();
	}



	public ArrayList<Gob> Crops(boolean checkStage) {
		// Initialises list of crops to harvest between selected coordinates
		ArrayList<Gob> gobs = new ArrayList<Gob>();
		double bigX = rc1.x > rc2.x ? rc1.x : rc2.x;
		double smallX = rc1.x < rc2.x ? rc1.x : rc2.x;
		double bigY = rc1.y > rc2.y ? rc1.y : rc2.y;
		double smallY = rc1.y < rc2.y ? rc1.y : rc2.y;
		synchronized (ui.sess.glob.oc) {
			for (Gob gob : ui.sess.glob.oc) {
				if (gob.rc.x <= bigX && gob.rc.x >= smallX && gob.getres() != null && gob.rc.y <= bigY
						&& gob.rc.y >= smallY && cropName.contains(gob.getres().name)) {
					// Add to list if its max stage
					if (checkStage) {
						int cropstgmaxval = 0;
						for (FastMesh.MeshRes layer : gob.getres().layers(FastMesh.MeshRes.class)) {
							int stg = layer.id / 10;
							if (stg > cropstgmaxval)
								cropstgmaxval = stg;
						}
						if (gob.getStage() == cropstgmaxval) {
							gobs.add(gob);
						}
					} else
						gobs.add(gob);
				}
			}
		}
		gobs.sort(new CoordSort());
		return gobs;
	}
	public ArrayList<Gob> Tables() {
		// Initialises list of crops to harvest between selected coordinates
		ArrayList<Gob> gobs = new ArrayList<Gob>();
		double bigX = rc1.x > rc2.x ? rc1.x : rc2.x;
		double smallX = rc1.x < rc2.x ? rc1.x : rc2.x;
		double bigY = rc1.y > rc2.y ? rc1.y : rc2.y;
		double smallY = rc1.y < rc2.y ? rc1.y : rc2.y;
		synchronized (ui.sess.glob.oc) {
			for (Gob gob : ui.sess.glob.oc) {
				if (gob.rc.x <= bigX && gob.rc.x >= smallX && gob.getres() != null && gob.rc.y <= bigY
						&& gob.rc.y >= smallY && gob.getres().basename().contains("htable")) {
						gobs.add(gob);
				}
			}
		}
		gobs.sort(new CoordSort());
		return gobs;
	}

	public ArrayList<Gob> Trellises() {
		// Initialises list of crops to harvest between selected coordinates
		ArrayList<Gob> gobs = new ArrayList<Gob>();
		double bigX = rc1.x > rc2.x ? rc1.x : rc2.x;
		double smallX = rc1.x < rc2.x ? rc1.x : rc2.x;
		double bigY = rc1.y > rc2.y ? rc1.y : rc2.y;
		double smallY = rc1.y < rc2.y ? rc1.y : rc2.y;
		synchronized (ui.sess.glob.oc) {
			for (Gob gob : ui.sess.glob.oc) {
				if (gob.rc.x <= bigX && gob.rc.x >= smallX && gob.getres() != null && gob.rc.y <= bigY
						&& gob.rc.y >= smallY && gob.getres().name.equals(trellis)) {
					gobs.add(gob);
				}
			}
		}
		gobs.sort(new CoordSort());
		return gobs;
	}

	@Override
	public void wdgmsg(Widget sender, String msg, Object... args) {
		if (sender == cbtn) {
			stop();
			reqdestroy();
		} else
			super.wdgmsg(sender, msg, args);
	}

	// Sorts coordinate array to efficient sequence
	class CoordSort implements Comparator<Gob> {
		public int compare(Gob a, Gob b) {

			if (a.rc.x == b.rc.x) {
				if (a.rc.x % 2 == 0)
					return (a.rc.y < b.rc.y) ? 1 : (a.rc.y > b.rc.y) ? -1 : 0;
				else
					return (a.rc.y < b.rc.y) ? -1 : (a.rc.y > b.rc.y) ? 1 : 0;
			} else
				return (a.rc.x < b.rc.x) ? -1 : (a.rc.x > b.rc.x) ? 1 : 0;
		}
	}

	public void stop() {
		// Stops thread
		BotUtils.sysMsg("Trellis Farmer stopped!", Color.white);
		gameui().map.wdgmsg("click", Coord.z, gameui().map.player().rc.floor(posres), 1, 0);
		if (gameui().map.pfthread != null) {
			gameui().map.pfthread.interrupt();
		}
		stopThread = true;
		this.destroy();
	}
}
