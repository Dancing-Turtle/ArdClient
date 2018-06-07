package haven.purus;

import haven.Button;
import haven.*;
import haven.Label;
import haven.Window;

import java.awt.*;
import java.util.ArrayList;
import java.util.Comparator;

import static haven.OCache.posres;

public class TrellisFarmer extends Window implements Runnable {

	private Coord rc1, rc2;
	
	private ArrayList<Gob> crops = new ArrayList<Gob>();

	private boolean stopThread = false;

	private Label lblProg;

	private ArrayList<String> cropName = new ArrayList<String>();
	private ArrayList<String> seedName = new ArrayList<String>();
	private String trellis = "gfx/terobjs/plants/trellis";

	private boolean harvest = false;
	private boolean destroy = false;
	private boolean replant = false;
	private Gob chest;
	public TrellisFarmer(Coord rc1, Coord rc2, boolean harvest, boolean destroy, boolean replant, Gob chest) {
		super(new Coord(120, 65), "Trellis Farmer");
		this.rc1 = rc1;
		this.rc2 = rc2;
		this.harvest = harvest;
		this.destroy = destroy;
		this.replant = replant;
		this.chest = chest;

		// Initialise arraylists
		seedName.add("gfx/invobjs/peapod");
		seedName.add("gfx/invobjs/peppercorn");
		seedName.add("gfx/invobjs/seed-cucumber");
		seedName.add("gfx/invobjs/seed-grape");
		seedName.add("gfx/invobjs/hopcones");

		cropName.add("gfx/terobjs/plants/pepper");
		cropName.add("gfx/terobjs/plants/peas");
		cropName.add("gfx/terobjs/plants/hops");
		cropName.add("gfx/terobjs/plants/cucumber");
		cropName.add("gfx/terobjs/plants/wine");

		Label lblstxt = new Label("Progress:");
		add(lblstxt, new Coord(15, 35));
		lblProg = new Label("Initialising...");
		add(lblProg, new Coord(65, 35));

		Button stopBtn = new Button(120, "Stop") {
			@Override
			public void click() {
				stop();
			}
		};
		add(stopBtn, new Coord(0, 0));
	}

	public void run() {
		BotUtils.sysMsg("Trellis Farmer started!", Color.white);
		if (harvest) {

			// Initialise crop list
			crops = Crops(true);

			// Initialize progression label on window
			int totalCrops = crops.size();
			int cropsHarvested = 0;
			lblProg.settext(cropsHarvested + "/" + totalCrops);

			for (Gob g : crops) {
				if (stopThread) // Checks if aborted
					return;

				// Check if stamina is under 30%, drink if needed
				GameUI gui = HavenPanel.lui.root.findchild(GameUI.class);
				IMeter.Meter stam = gui.getmeter("stam", 0);
				if (stam.a <= 30) {
					BotUtils.drink();
				}


				if (stopThread)
					return;

				int stageBefore = g.getStage();


				// Right click the crop
				BotUtils.doClick(g, 3, 0);

				// Wait for harvest menu to appear
				while (ui.root.findchild(FlowerMenu.class) == null) {
					BotUtils.sleep(10);
					if (stopThread)
						return;
				}

				// Select the harvest option
				FlowerMenu menu = ui.root.findchild(FlowerMenu.class);
				if (menu != null) {
					for (FlowerMenu.Petal opt : menu.opts) {
						if (opt.name.equals("Harvest")) {
							menu.choose(opt);
							menu.destroy();
						}
					}
				}

				// Wait until stage has changed = harvested
				while (true) {
					if (BotUtils.findObjectById(g.id) == null
							|| BotUtils.findObjectById(g.id).getStage() != stageBefore)
						break;
					else
						BotUtils.sleep(20);
					if (stopThread)
						return;
				}
				//BotUtils.dropItem(0);
				GItem dropitem;
				for (Widget w = BotUtils.playerInventory().child; w != null; w = w.next) {
					if (w instanceof GItem && ((GItem) w).resource().name.contains("grape")) {
						dropitem = (GItem) w;
						try {
							dropitem.wdgmsg("drop", Coord.z);
						} catch (Exception e) { }
					}
				}

					if (BotUtils.invFreeSlots() < 4 && chest != null) {
						BotUtils.pfRightClick(chest, 0);
						try {
							while (gui.getwnd("Exquisite Chest") == null) {
								try {
									Thread.sleep(10);
								} catch (InterruptedException iqp) {
								}
							}
						} catch (NullPointerException ipo) {
						}
						BotUtils.waitForWindow("Exquisite Chest");
						for (Widget w = BotUtils.playerInventory().child; w != null; w = w.next) {
							if (w instanceof GItem && ((GItem) w).res.get().name.contains("pepper")) {
								GItem item = (GItem) w;
								try {
									item.wdgmsg("transfer", Coord.z);

								} catch (NullPointerException qip) {
									BotUtils.sysMsg("Null Pointer on line 142", Color.white);
								}
							}
						}
						if(BotUtils.invFreeSlots() < 20)
							for (Widget w = BotUtils.playerInventory().child; w != null; w = w.next) {
								if (w instanceof GItem && ((GItem) w).res.get().name.contains("pepper")) {
									GItem item = (GItem) w;
									try {
										item.wdgmsg("drop", Coord.z);

									} catch (NullPointerException qip) {
										BotUtils.sysMsg("Null Pointer on line 142", Color.white);
									}
								}
							}
					}
					// Update progression
					cropsHarvested++;
					lblProg.settext(cropsHarvested + "/" + totalCrops);
				}
			}


		if (destroy) {
			crops = Crops(false);

			// Initialize progression label on window
			int totalCrops = crops.size();
			int cropsHarvested = 0;
			lblProg.settext(cropsHarvested + "/" + totalCrops);

			for (Gob g : crops) {
				if (stopThread) // Checks if aborted
					return;

				// Check if stamina is under 30%, drink if needed
				GameUI gui = HavenPanel.lui.root.findchild(GameUI.class);
				IMeter.Meter stam = gui.getmeter("stam", 0);
				if (stam.a <= 30) {
					BotUtils.drink();
				}

				if (stopThread)
					return;

				// Click destroy on gob
				BotUtils.destroyGob(g);

				// Wait until the gob is gone = destroyed
				while (BotUtils.findObjectById(g.id) != null) {
					BotUtils.sleep(10);
					if (stopThread)
						return;
				}

				// Update progression
				cropsHarvested++;
				lblProg.settext(cropsHarvested + "/" + totalCrops);
			}
		} // End of destroy

		if (replant) {
			crops = Trellises(); // in this case crops = trellis
			// Initialise progression label on window
			int totalCrops = crops.size();
			int cropsHarvested = 0;
			lblProg.settext(cropsHarvested + "/" + totalCrops);

			for (Gob g : crops) {

				// Take a seed from inventory to hand
				GItem item = null;
				while (BotUtils.getItemAtHand() == null) {
					Inventory inv = BotUtils.playerInventory();
					for (Widget w = inv.child; w != null; w = w.next) {
						if (w instanceof GItem && seedName.contains(((GItem) w).resource().name)) {
							item = (GItem) w;
							break;
						}
					}
					if (item != null)
						BotUtils.takeItem(item);
				}

				if (stopThread)
					return;

				// Right click trellis with the seed
				BotUtils.itemClick(g, 0);

				// Wait until item is gone from hand = Planted
				int retry = 0; // IF no success for 10 seconds skip
				while (BotUtils.getItemAtHand() != null) {
					BotUtils.sleep(10);
					if (stopThread)
						return;
					retry++;
					if (retry > 1000)
						break;
				}

				// Update progression
				cropsHarvested++;
				lblProg.settext(cropsHarvested + "/" + totalCrops);
			}
		}

		BotUtils.sysMsg("Trellis Farmer finished!", Color.white);
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