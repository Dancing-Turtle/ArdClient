package haven.purus;

import static haven.OCache.posres;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;

import haven.Button;
import haven.Coord;
import haven.FlowerMenu;
import haven.GItem;
import haven.GameUI;
import haven.Gob;
import haven.HavenPanel;
import haven.IMeter;
import haven.Inventory;
import haven.Label;
import haven.Widget;
import haven.Window;
import haven.automation.BeltDrink;
import haven.purus.pbot.PBotAPI;

public class SeedCropFarmer extends Window implements Runnable {


	private Coord rc1, rc2;

	private ArrayList<Gob> crops = new ArrayList<Gob>();

	private boolean stopThread = false;

	private Label lblProg;

	private int stage;
	private String cropName;
	private String seedName;
	private boolean replant;
	private boolean replantcontainer; // True = Only Container, False = Only replant
	private boolean containeronly;
	private Gob barrel;

	public SeedCropFarmer(Coord rc1, Coord rc2, String cropName, String seedName, int stage,boolean replant, boolean containeronly, boolean replantcontainer, Gob barrel) {
		super(new Coord(120, 65), cropName.substring(cropName.lastIndexOf("/") + 1).substring(0, 1).toUpperCase()
				+ cropName.substring(cropName.lastIndexOf("/") + 1).substring(1) + " Farmer");
		this.rc1 = rc1;
		this.rc2 = rc2;
		this.cropName = cropName;
		this.stage = stage;
		this.seedName = seedName;
		this.replantcontainer = replantcontainer;
		this.replant = replant;
		this.containeronly = containeronly;
		this.barrel = barrel;

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
		// Initialise crop list
		crops = Crops();
		int totalCrops = crops.size();
		int cropsHarvested = 0;
		lblProg.settext(cropsHarvested + "/" + totalCrops);
		for (Gob g : crops) {
			if (stopThread)
				return;
			// Check if stamina is under 30%, drink if so
			GameUI gui = HavenPanel.lui.root.findchild(GameUI.class);
			IMeter.Meter stam = gui.getmeter("stam", 0);
			if (stam.a <= 30) {
				BotUtils.drink();
			}

			if (stopThread)
				return;

			// Right click the crop
			BotUtils.doClick(g, 1, 0);
			BotUtils.gui.map.wdgmsg("click", Coord.z, g.rc.floor(posres), 1, 0);
			while (BotUtils.player().rc.x != g.rc.x || BotUtils.player().rc.y != g.rc.y) {
				BotUtils.sleep(10);
			}
			BotUtils.pfRightClick(g, 0);

			// Wait for harvest menu to appear and harvest the crop
			while (ui.root.findchild(FlowerMenu.class) == null) {
				BotUtils.sleep(10);
			}

			if (stopThread)
				return;

			FlowerMenu menu = ui.root.findchild(FlowerMenu.class);
			if (menu != null) {
				for (FlowerMenu.Petal opt : menu.opts) {
					if (opt.name.equals("Harvest")) {
						menu.choose(opt);
						menu.destroy();
					}
				}
			}
			while (BotUtils.findObjectById(g.id) != null) {
				BotUtils.sleep(10);
			}

			if (stopThread)
				return;
			// Replant
			if (replant) {
				try {
					GItem item = null;
					while (BotUtils.getItemAtHand() == null) {
						Inventory inv = BotUtils.playerInventory();
						for (Widget w = inv.child; w != null; w = w.next) {
							if (w instanceof GItem && ((GItem) w).resource().name.equals(seedName) && (!seedName.contains("seed") || BotUtils.getAmount((GItem) w) >= 5)) {
								while ((GItem) w == null) {
									BotUtils.sleep(10);
								}
								item = (GItem) w;
								break;
							}
						}
						if (item != null)
							BotUtils.takeItem(item);
					}

					while (BotUtils.getItemAtHand() == null)
						BotUtils.sleep(10);

					// Plant the seed from hand
					int amount = 0;
					if (seedName.contains("seed"))
						BotUtils.getAmount(BotUtils.getItemAtHand());
					BotUtils.mapInteractClick(0);
					while (BotUtils.findNearestStageCrop(5, 0, cropName) == null || (BotUtils.getItemAtHand() != null && (seedName.contains("seed") && amount == BotUtils.getAmount(BotUtils.getItemAtHand())))) {
						BotUtils.sleep(10);
					}
					BotUtils.dropItem(0);
					for (Widget w = BotUtils.playerInventory().child; w != null; w = w.next) {
						if (w instanceof GItem && ((GItem) w).resource().name.equals(seedName)) {
							item = (GItem) w;
							try {
								item.wdgmsg("drop", Coord.z);
							} catch (Exception e) {
							}
						}
					}
				}catch(NullPointerException q){}
			} else if (replantcontainer) {
				try {
					GItem item = null;
					while (BotUtils.getItemAtHand() == null) {
						Inventory inv = BotUtils.playerInventory();
						for (Widget w = inv.child; w != null; w = w.next) {
							if (w instanceof GItem && ((GItem) w).resource().name.equals(seedName) && (!seedName.contains("seed") || BotUtils.getAmount((GItem) w) >= 5)) {
								while ((GItem) w == null) {
									BotUtils.sleep(10);
								}
								item = (GItem) w;
								break;
							}
						}
						if (item != null)
							BotUtils.takeItem(item);

					}
					while (BotUtils.getItemAtHand() == null)
						BotUtils.sleep(10);

					// Plant the seed from hand
					int amount = 0;
					if (seedName.contains("seed"))
						BotUtils.getAmount(BotUtils.getItemAtHand());
					BotUtils.mapInteractClick(0);
					while (BotUtils.findNearestStageCrop(5, 0, cropName) == null || (BotUtils.getItemAtHand() != null && (seedName.contains("seed") && amount == BotUtils.getAmount(BotUtils.getItemAtHand())))) {
						BotUtils.sleep(10);
					}


					// Merge seed from hand into inventory or put it in inventory
					for (Widget w = BotUtils.playerInventory().child; w != null; w = w.next) {
						if (w instanceof GItem && ((GItem) w).resource().name.equals(seedName)) {
							item = (GItem) w;
							if (BotUtils.getItemAtHand() != null && BotUtils.getAmount(item) < 50) {
								int handAmount = BotUtils.getAmount(BotUtils.getItemAtHand());
								try {
									item.wdgmsg("itemact", 0);
								} catch (Exception e) {
								}
								while (BotUtils.getItemAtHand() != null && BotUtils.getAmount(BotUtils.getItemAtHand()) == handAmount)
									BotUtils.sleep(50);
							}
						}
					}
					if (BotUtils.getItemAtHand() != null) {
						Coord slot = BotUtils.getFreeInvSlot(BotUtils.playerInventory());
						if (slot != null) {
							int freeSlots = BotUtils.invFreeSlots();
							BotUtils.dropItemToInventory(slot, BotUtils.playerInventory());
							while (BotUtils.getItemAtHand() != null)
								BotUtils.sleep(50);
						}
					}
					if (BotUtils.invFreeSlots() == 0) {
						BotUtils.pfRightClick(barrel, 0);
						BotUtils.waitForWindow("Barrel");
						if (BotUtils.getItemAtHand() != null) {
							gameui().map.wdgmsg("itemact", Coord.z, barrel.rc.floor(posres), 0, 0, (int) barrel.id,
									barrel.rc.floor(posres), 0, -1);
							int i = 0;
							while (BotUtils.getItemAtHand() != null) {
								if (i == 60000)
									break;
								BotUtils.sleep(10);
								i++;
							}
						}
						while (BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedName)).size() != 0) {
							if (stopThread)
								break;
							item = BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedName)).get(0).item;
							BotUtils.takeItem(item);

							gameui().map.wdgmsg("itemact", Coord.z, barrel.rc.floor(posres), 0, 0, (int) barrel.id,
									barrel.rc.floor(posres), 0, -1);
							int i = 0;
							while (BotUtils.getItemAtHand() != null) {
								if (i == 60000)
									break;
								BotUtils.sleep(10);
								i++;
							}
						}
					}
				} catch (NullPointerException x) {
				}
			}
		else {
				try {
					if (containeronly) { // Put items into container if inventory is full
						GItem item;
						if (BotUtils.invFreeSlots() == 0) {
							BotUtils.pfRightClick(barrel, 0);
							BotUtils.waitForWindow("Barrel");
							if (BotUtils.getItemAtHand() != null) {
								gameui().map.wdgmsg("itemact", Coord.z, barrel.rc.floor(posres), 0, 0, (int) barrel.id,
										barrel.rc.floor(posres), 0, -1);
								int i = 0;
								while (BotUtils.getItemAtHand() != null) {
									if (i == 60000)
										break;
									BotUtils.sleep(10);
									i++;
								}
							}
							while (BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedName)).size() != 0) {
								if (stopThread)
									break;
								item = BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedName)).get(0).item;
								BotUtils.takeItem(item);

								gameui().map.wdgmsg("itemact", Coord.z, barrel.rc.floor(posres), 0, 0, (int) barrel.id,
										barrel.rc.floor(posres), 0, -1);
								int i = 0;
								while (BotUtils.getItemAtHand() != null) {
									if (i == 60000)
										break;
									BotUtils.sleep(10);
									i++;
								}
							}
						}
					}
				} catch (NullPointerException p) { }
			}



			cropsHarvested++;
			lblProg.settext(cropsHarvested + "/" + totalCrops);
		}
		if(replantcontainer) {
			if (BotUtils.getItemAtHand() != null)
				BotUtils.dropItem(0);
			BotUtils.pfRightClick(barrel, 0);
			BotUtils.waitForWindow("Barrel");
	
			while (BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedName)).size() != 0) {
				if (stopThread)
					break;
				GItem item = BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedName)).get(0).item;
				BotUtils.takeItem(item);
	
				gameui().map.wdgmsg("itemact", Coord.z, barrel.rc.floor(posres), 0, 0, (int) barrel.id,
						barrel.rc.floor(posres), 0, -1);
				int i = 0;
				while (BotUtils.getItemAtHand() != null) {
					if (i == 60000)
						break;
					BotUtils.sleep(10);
					i++;
				}
			}
		}
		BotUtils.sysMsg(cropName.substring(cropName.lastIndexOf("/") + 1).substring(0, 1).toUpperCase()
						+ cropName.substring(cropName.lastIndexOf("/") + 1).substring(1)
						+ " Farmer finished!", Color.white);
		this.destroy();
	}

	public ArrayList<Gob> Crops() {
		// Initialises list of crops to harvest between the selected coordinates
		ArrayList<Gob> gobs = new ArrayList<Gob>();
		double bigX = rc1.x > rc2.x ? rc1.x : rc2.x;
		double smallX = rc1.x < rc2.x ? rc1.x : rc2.x;
		double bigY = rc1.y > rc2.y ? rc1.y : rc2.y;
		double smallY = rc1.y < rc2.y ? rc1.y : rc2.y;
		synchronized (ui.sess.glob.oc) {
			for (Gob gob : ui.sess.glob.oc) {
				if (gob.rc.x <= bigX && gob.rc.x >= smallX && gob.getres() != null && gob.rc.y <= bigY
						&& gob.rc.y >= smallY && gob.getres().name.contains(cropName) && gob.getStage() == stage) {
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
			if (a.rc.floor().x == b.rc.floor().x) {
				if (a.rc.floor().x % 2 == 0)
					return (a.rc.floor().y < b.rc.floor().y) ? 1 : (a.rc.floor().y > b.rc.floor().y) ? -1 : 0;
				else
					return (a.rc.floor().y < b.rc.floor().y) ? -1 : (a.rc.floor().y > b.rc.floor().y) ? 1 : 0;
			} else
				return (a.rc.floor().x < b.rc.floor().x) ? -1 : (a.rc.floor().x > b.rc.floor().x) ? 1 : 0;
		}
	}

	public void stop() {
		// Stops thread
		BotUtils.sysMsg(cropName.substring(cropName.lastIndexOf("/") + 1).substring(0, 1).toUpperCase()
						+ cropName.substring(cropName.lastIndexOf("/") + 1).substring(1)
						+ " Farmer stopped!", Color.white);
		gameui().map.wdgmsg("click", Coord.z, gameui().map.player().rc.floor(posres), 1, 0);
		if (gameui().map.pfthread != null) {
			gameui().map.pfthread.interrupt();
		}
		stopThread = true;
		this.destroy();
	}
}