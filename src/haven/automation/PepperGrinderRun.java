package haven.automation;

import haven.Button;
import haven.*;
import haven.Label;
import haven.Window;
import haven.purus.BotUtils;
import haven.purus.pbot.PBotAPI;

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
			BotUtils.sysMsg("Pepper Grinder Bot started! Tables selected : "+tables.size(), Color.white);
			GameUI gui = gameui();

			gui.wdgmsg("act", "craft", "grindpepper");
			BotUtils.waitForWindow("Crafting");

			if (stopThread) // Checks if aborted
				return;

			while (tables.size() > 0 && !stopThread) {
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


	/*if (section == 2) {
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
	}*/
				int finishtimeout = 0;
				while (BotUtils.invFreeSlots() > 2 && !stopThread) {
					if(stopThread)
						return;
					finishtimeout++;
					if (finishtimeout > 10000) {
						stopThread = true;
						return;
					}
					lblProg.settext("Status - Collecting");
					//while (BotUtils.findObjectByNames(10, "gfx/terobjs/htable") == null)
					//BotUtils.sleep(10);
					//BotUtils.sysLogAppend("tablecoutn : "+tables.size(), "white");
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
					PBotAPI.pfRightClick(htable, 0);
					try {
						gui.map.pfthread.join();
					} catch (InterruptedException e) {
						return;
					}
					try {
						Thread.sleep(TIMEOUT);
					} catch (InterruptedException e) {
						return;
					}
					int retrytimer = 0;
					int retrycount = 0;
					while (gui.getwnd("Herbalist Table") == null) {
						retrytimer++;
						if (retrytimer > 1000) {
							retrycount++;
							if (retrycount > 1) {
								lblProg.settext("Unstucking");
								BotUtils.sysLogAppend("Moving char", "white");
								Gob player = gui.map.player();
								Coord location = player.rc.floor(posres);
								int x = location.x + + getrandom();
								int y = location.y + + getrandom();
								Coord finalloc = new Coord(x, y);
								gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
								retrycount = 0;
								BotUtils.sleep(1000);
							}
							PBotAPI.pfRightClick(htable, 0);
							try {
								gui.map.pfthread.join();
							} catch (InterruptedException e) {
								return;
							}
							try {
								Thread.sleep(TIMEOUT);
							} catch (InterruptedException e) {
								return;
							}
						}
						BotUtils.sleep(10);
					}
					BotUtils.waitForWindow("Herbalist Table");
					//gui.map.wdgmsg("click", htable.sc, htable.rc.floor(posres), 3, 0, 0, (int) htable.id, htable.rc.floor(posres), 0, -1);
					//BotUtils.sysLogAppend("Found table, clicked", "white");
					//BotUtils.waitForWindow("Herbalist Table");
					Window herbtable = gui.getwnd("Herbalist Table");
					if (herbtable == null)
						continue;
					int freeslots = BotUtils.invFreeSlots();
					for (Widget w = herbtable.lchild; w != null; w = w.prev) {
						if (w instanceof Inventory) {
							Inventory inv = (Inventory) w;
							List<WItem> items = BotUtils.getInventoryContents(inv);
							for (WItem item : items) {
								freeslots = BotUtils.invFreeSlots();
								if (freeslots > 16) {
									System.out.println("Transferring pepper freeslots : " + freeslots);
									item.item.wdgmsg("transfer", Coord.z);
								} else if (freeslots > 2) {
									System.out.println("Transferring pepper freeslots : " + freeslots);
									item.item.wdgmsg("transfer", Coord.z);
									BotUtils.sleep(300);
								} else
									break;
							}
						}
					}
					herbtable.close();
					//	BotUtils.sysLogAppend("Tables Size : "+tables.size(),"white");
					htable = null;
				}
				if (BotUtils.invFreeSlots() <= 2) {
					GItem pepperlol = gui.maininv.getItemPartial("Dried").item;
					java.util.List<WItem> pepper = gui.maininv.getIdenticalItems((pepperlol));
					lblProg.settext("Status - Going to Grind");
					BotUtils.pfRightClick(grinder, 0);
					try {
						gui.map.pfthread.join();
					} catch (InterruptedException e) {
						return;
					}
					try {
						Thread.sleep(TIMEOUT);
					} catch (InterruptedException e) {
						return;
					}
					BotUtils.sleep(6000); //sleep 6 seconds to walk to grinder
					int timeout = 0;
					int retrycount = 0;
					while (gui.maininv.getIdenticalItems((pepperlol)).size() > 5) {
						timeout++;
						if (timeout > 5000) {
							gui.maininv.getItemPartial("Dried").item.wdgmsg("drop", Coord.z);
							timeout = 0;
						}
						while (gui.prog >= 0) {
							BotUtils.sleep(10);
							lblProg.settext("Status - Grinding");
						}
						if (PBotAPI.getStamina() > 50) {
							ui.makewnd.wdgmsg("make",1);
							BotUtils.sleep(2000);
							retrycount++;
							if (retrycount > 1) {
								lblProg.settext("Unstucking");
								BotUtils.sysLogAppend("Moving char", "white");
								Gob player = gui.map.player();
								Coord location = player.rc.floor(posres);
								int x = location.x + + getrandom();
								int y = location.y + + getrandom();
								Coord finalloc = new Coord(x, y);
								gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
								retrycount = 0;
								BotUtils.sleep(1000);

								BotUtils.pfRightClick(grinder, 0);
								try {
									gui.map.pfthread.join();
								} catch (InterruptedException e) {
									return;
								}
								try {
									Thread.sleep(TIMEOUT);
								} catch (InterruptedException e) {
									return;
								}
							}
						} else {
							lblProg.settext("Status - Drinking");
							new Thread(new BeltDrink(gui), "BeltDrink").start();
							BotUtils.sleep(5000);
						}
					}
				//	gui.map.wdgmsg("click", hfire.sc, hfire.rc.floor(posres), 1, 0, 0, (int) hfire.id, hfire.rc.floor(posres), 0, -1);
					//BotUtils.sleep(2000);


				}
				BotUtils.sysLogAppend("end of loop", "white");
				if(stopThread)
					return;
			}
			BotUtils.sysMsg("Done",Color.white);
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
		BotUtils.sysMsg("Pepper Grinder Stopped!", Color.white);
		//gameui().map.wdgmsg("click", Coord.z, gameui().map.player().rc.floor(posres), 1, 0);
		if (gameui().map.pfthread != null) {
			gameui().map.pfthread.interrupt();
		}
		stopThread = true;
		this.destroy();
	}
}
