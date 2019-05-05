package haven.automation;

import haven.Button;
import haven.*;
import haven.Label;
import haven.Window;
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;

import java.awt.*;
import java.util.*;
import java.util.List;

import static haven.OCache.posres;

public class PepperGrinderRun extends Window implements Runnable {
	private Coord rc1, rc2;
	private ArrayList<Gob> crops = new ArrayList<Gob>();
	private ArrayList<Gob> tables = new ArrayList<Gob>();
	public ArrayList<Gob> blacklist = new ArrayList<Gob>();
	private boolean stopThread = false;
	private Label lblProg;
	private ArrayList<String> cropName = new ArrayList<String>();
	private ArrayList<String> seedName = new ArrayList<String>();
	private String trellis = "gfx/terobjs/plants/trellis";
	private boolean harvest = false;
	private boolean destroy = false;
	public Gob htable;
	private boolean replant = false;
	private static final int TIMEOUT = 1000;
	public Button stopBtn;
	public int x, y;
	private Gob chest, water, rowmarker, cauldron, barrel, hfire, grinder;
	private final int rowgap = 4200;
	private final int travel = 20000;

	private int section, direction;
	public Widget craftall;
	private Boolean boilmode = false;
	private Coord finalloc;
	private Thread t;

	public PepperGrinderRun(Coord rc1, Coord rc2, Gob grinder, int section, int direction) {
		super(new Coord(120, 45), "Pepper Grinder");
		this.grinder = grinder;
		this.rc1 = rc1;
		this.rc2 = rc2;
		this.direction = direction;
		this.section = section;

		// Initialise arraylists

		lblProg = new Label("Initialising...");
		add(lblProg, new Coord(0, 35));

		stopBtn = new Button(120, "Stop") {
			@Override
			public void click() {
				stopThread = true;
				stop();
			}
		};
		add(stopBtn, new Coord(0, 0));

	}

		public void run() {

			//section = 4;
			tables = Tables();
			PBotUtils.sysMsg("Pepper Grinder Bot started! Tables selected : "+tables.size(), Color.white);
			GameUI gui = gameui();

			gui.wdgmsg("act", "craft", "blackpepper");
			PBotUtils.waitForWindow("Crafting");

			if (stopThread) // Checks if aborted
				return;

			while (tables.size() > 0 && !stopThread) {
				// Check if stamina is under 30%, drink if needed
				gui = HavenPanel.lui.root.findchild(GameUI.class);
				IMeter.Meter stam = gui.getmeter("stam", 0);
				if (stam.a <= 60) {
					lblProg.settext("Drinking");
					PBotUtils.drink(true);
					PBotUtils.sleep(5000);
				}


				if (stopThread)
					return;

				int finishtimeout = 0;
				while (PBotUtils.invFreeSlots() > 2 && !stopThread) {
					if(stopThread)
						return;
					finishtimeout++;
					if (finishtimeout > 10000) {
						stopThread = true;
						return;
					}
					lblProg.settext("Status - Collecting");
					while (htable == null) {
						finishtimeout++;
						if (finishtimeout > 10000) {
							stopThread = true;
							return;
						}
						//BotUtils.sysLogAppend("while loop", "white");
						for (Gob tablelol : tables) {
							//BotUtils.sysLogAppend("table loop", "white");
							if (tablelol.ols.size() > 0)
								htable = tablelol;
							else
								blacklist.add(tablelol);
						}
					}
					tables.removeAll(blacklist);
					blacklist.clear();
					//BotUtils.sleep(500);
					//	BotUtils.sysLogAppend("Found table, clicking", "white");
					//PBotUtils.pfRightClick(htable, 0);
					//PBotAPI.gui.map.pfRightClick(htable,-1,3,0,null);
					PBotUtils.PathfinderRightClick(htable,0);
					PBotUtils.sleep(1000);
					int retrytimer = 0;
					int retrycount = 0;
					while (gui.getwnd("Herbalist Table") == null) {
						retrytimer++;
						if (retrytimer > 1000) {
							retrytimer = 0;
							retrycount++;
							if (retrycount > 1) {
								lblProg.settext("Unstucking");
								Gob player = gui.map.player();
								Coord location = player.rc.floor(posres);
								int x = location.x + + getrandom();
								int y = location.y + + getrandom();
								Coord finalloc = new Coord(x, y);
								gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
								retrycount = 0;
								PBotUtils.sleep(1000);
							}
						//	PBotUtils.pfRightClick(htable, 0);
							//PBotAPI.gui.map.pfRightClick(htable,-1,3,0,null);
							PBotUtils.PathfinderRightClick(htable,0);
						}
						PBotUtils.sleep(10);
					}
					PBotUtils.waitForWindow("Herbalist Table");
					Window herbtable = gui.getwnd("Herbalist Table");
					if (herbtable == null)
						continue;
					int freeslots;
					for (Widget w = herbtable.lchild; w != null; w = w.prev) {
						if (w instanceof Inventory) {
							Inventory inv = (Inventory) w;
							List<WItem> items = PBotUtils.getInventoryContents(inv);
							for (WItem item : items) {
								freeslots = PBotUtils.invFreeSlots();
								if (freeslots > 16) {
									System.out.println("Transferring pepper freeslots : " + freeslots);
									item.item.wdgmsg("transfer", Coord.z);
								} else if (freeslots > 2) {
									System.out.println("Transferring pepper freeslots : " + freeslots);
									item.item.wdgmsg("transfer", Coord.z);
									PBotUtils.sleep(300);
								} else
									break;
							}
						}
					}
					herbtable.close();
					//	BotUtils.sysLogAppend("Tables Size : "+tables.size(),"white");
					htable = null;
				}
				if (PBotUtils.invFreeSlots() <= 2) {
					GItem pepperlol = gui.maininv.getItemPartial("Dried").item;
					lblProg.settext("Status - Going to Grind");
				//	PBotUtils.pfRightClick(grinder, 0);
				//	PBotAPI.gui.map.pfRightClick(grinder,-1,3,0,null);
					PBotUtils.PathfinderRightClick(grinder,0);
					PBotUtils.sleep(6000); //sleep 6 seconds to walk to grinder
					int timeout = 0;
					int retrycount = 0;
					while (gui.maininv.getIdenticalItems((pepperlol)).size() > 5) {
						timeout++;
						if (timeout > 5000) {
							gui.maininv.getItemPartial("Dried").item.wdgmsg("drop", Coord.z);
							timeout = 0;
						}
						while (gui.prog >= 0) {
							PBotUtils.sleep(100);
							lblProg.settext("Status - Grinding");
						}
						if (PBotUtils.getStamina() > 50) {
							PBotUtils.craftItem("blackpepper",1);
							PBotUtils.sleep(2000);
							retrycount++;
							if (retrycount > 1) {
								lblProg.settext("Unstucking");
								Gob player = gui.map.player();
								Coord location = player.rc.floor(posres);
								int x = location.x + + getrandom();
								int y = location.y + + getrandom();
								Coord finalloc = new Coord(x, y);
								gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
								retrycount = 0;
								PBotUtils.sleep(1000);
								//PBotUtils.pfRightClick(grinder, 0);
							//	PBotAPI.gui.map.pfRightClick(grinder,-1,3,0,null);
								PBotUtils.PathfinderRightClick(grinder,0);
							}
						} else {
							lblProg.settext("Status - Drinking");
							PBotUtils.drink(true);
							PBotUtils.sleep(5000);
						}
					}
				}
				if(stopThread)
					return;
			}
			PBotUtils.sysMsg("Done",Color.white);
			stopThread = true;
		}

	public int getrandom(){
		Random r = new Random();
		int randomNumber = r.ints(1, -6000, 6000).findFirst().getAsInt();
		return randomNumber;
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
		gobs.sort(new CoordSortEW());
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
		if(direction==1 || direction == 2)
			gobs.sort(new CoordSortNS());
		else
			gobs.sort(new CoordSortEW());
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
	class CoordSortEW implements Comparator<Gob> { // sorts high Y to low Y along same X Axis
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
	class CoordSortNS implements Comparator<Gob> { // sorts high X to low X along the same Y Axis
		public int compare(Gob a, Gob b) {

			if (a.rc.y == b.rc.y) {
				if (a.rc.y % 2 == 0)
					return (a.rc.x < b.rc.x) ? 1 : (a.rc.x > b.rc.x) ? -1 : 0;
				else
					return (a.rc.x < b.rc.x) ? -1 : (a.rc.x > b.rc.x) ? 1 : 0;
			} else
				return (a.rc.y < b.rc.y) ? -1 : (a.rc.y > b.rc.y) ? 1 : 0;
		}
	}


	public void stop() {
		// Stops thread
		PBotUtils.sysMsg("Pepper Grinder Stopped!", Color.white);
		//gameui().map.wdgmsg("click", Coord.z, gameui().map.player().rc.floor(posres), 1, 0);
		if (gameui().map.pfthread != null) {
			gameui().map.pfthread.interrupt();
		}
		stopThread = true;
		this.destroy();
	}
}
