package haven.purus;

import static haven.OCache.posres;

import java.awt.Color;
import java.util.*;

import haven.*;
import haven.automation.BeltDrink;
import haven.purus.pbot.PBotAPI;
import haven.res.ui.tt.q.qbuff.QBuff;

public class SeedCropFarmer extends Window implements Runnable {


	private Coord rc1, rc2;

	private ArrayList<Gob> crops = new ArrayList<Gob>();

	private boolean stopThread = false;

	private Label lblProg, lblProg2;

	private int stage;
	private String cropName;
	private String seedName;
	private boolean replant;
	private boolean replantcontainer; // True = Only Container, False = Only replant
	private boolean containeronly;
	private ArrayList<Gob> containers = new ArrayList<>();
	//private Gob barrel;

	public SeedCropFarmer(Coord rc1, Coord rc2, String cropName, String seedName, int stage,boolean replant, boolean containeronly, boolean replantcontainer, ArrayList<Gob> containers) {
		super(new Coord(120, 65), cropName.substring(cropName.lastIndexOf("/") + 1).substring(0, 1).toUpperCase()
				+ cropName.substring(cropName.lastIndexOf("/") + 1).substring(1) + " Farmer");
		this.rc1 = rc1;
		this.rc2 = rc2;
		this.cropName = cropName;
		this.stage = stage;
		this.containers = containers;
		this.seedName = seedName;
		this.replantcontainer = replantcontainer;
		this.replant = replant;
		this.containeronly = containeronly;
	//	this.barrel = barrel;

		Label lblstxt = new Label("Progress:");
		add(lblstxt, new Coord(15, 35));
		lblProg = new Label("Initialising...");
		add(lblProg, new Coord(67, 35));
		lblProg2 = new Label("Initialising...");
		add(lblProg2, new Coord(15,55));

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
		BotUtils.gui.map.unregisterGobSelect();
		crops = Crops();
		int totalCrops = crops.size();
		int cropsHarvested = 0;
		lblProg.settext(cropsHarvested + "/" + totalCrops);
		lblProg2.settext("Starting");
		for (Gob g : crops) {
			if (stopThread)
				return;
			// Check if stamina is under 30%, drink if so
			GameUI gui = HavenPanel.lui.root.findchild(GameUI.class);
			IMeter.Meter stam = gui.getmeter("stam", 0);
			if (stam.a <= 30) {
				new Thread(new BeltDrink(gui), "BeltDrink").start();
				BotUtils.sleep(5000);
			}

			if (stopThread)
				return;

			// Right click the crop
			lblProg2.settext("Harvesting");
			try {
				BotUtils.doClick(g, 1, 0);
			}catch(NullPointerException ii){continue;}
			BotUtils.gui.map.wdgmsg("click", Coord.z, g.rc.floor(posres), 1, 0);
			while (BotUtils.player().rc.x != g.rc.x || BotUtils.player().rc.y != g.rc.y)
				BotUtils.sleep(10);

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
						lblProg2.settext("Grabbing seeds");
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
					lblProg2.settext("Planting");
					BotUtils.mapInteractClick(0);
					while (BotUtils.findNearestStageCrop(5, 0, cropName) == null || (BotUtils.getItemAtHand() != null && (seedName.contains("seed") && amount == BotUtils.getAmount(BotUtils.getItemAtHand())))) {
						BotUtils.sleep(10);
					}
					BotUtils.dropItem(0);
					lblProg2.settext("Dropping seeds");
					for (Widget w = BotUtils.playerInventory().child; w != null; w = w.next) {
						if (w instanceof GItem && ((GItem) w).resource().name.equals(seedName)) {
							item = (GItem) w;
							try {
								item.wdgmsg("drop", Coord.z);
							} catch (Exception e) {
							}
						}
					}
				}catch(NullPointerException | Loading | Sprite.ResourceException q){}
			} else if (replantcontainer) {
				try {
					while (BotUtils.getItemAtHand() == null) { // loops until successfully picked up seeds
						lblProg2.settext("Grabbing seeds");
						while(gui.maininv.getItemPartial("seed") == null)
							BotUtils.sleep(10);
						WItem flax = gui.maininv.getItemPartial("seed");
						GItem flax2 = flax.item;
						java.util.List<WItem> items = gui.maininv.getIdenticalItems((flax2)); // acquires all seed stacks in inventory
						sort(items); // sorts by quality
						for (WItem seeds : items) {
							GItem item = seeds.item;
							if (BotUtils.getAmount(item) >= 5) {
								BotUtils.takeItem(item);
								break;
							}
						}
					}
						while (BotUtils.getItemAtHand() == null) // just a double verification that we have successfully picked up seeds, should account for lag
							BotUtils.sleep(10);

						// Plant the seed from hand
						int amount = 0;
						if (seedName.contains("seed"))
							BotUtils.getAmount(BotUtils.getItemAtHand()); // logs the seed count in your hand so it can use the count to verify it successfully planted
					lblProg2.settext("Planting");
						BotUtils.mapInteractClick(0);
						while (BotUtils.findNearestStageCrop(5, 0, cropName) == null || (BotUtils.getItemAtHand() != null && (seedName.contains("seed") && amount == BotUtils.getAmount(BotUtils.getItemAtHand())))) {
							BotUtils.sleep(10);
						}
						// Merge seed from hand into inventory or put it in inventory
							for (Widget w = BotUtils.playerInventory().child; w != null; w = w.next) {
								if (w instanceof GItem && ((GItem) w).resource().name.equals(seedName)) {
									GItem item = (GItem) w;
									if (BotUtils.getItemAtHand() != null && BotUtils.getAmount(item) < 50) {//finds other seeds in inventory with less than 50 count
										lblProg2.settext("Merging stacks");
										int handAmount = BotUtils.getAmount(BotUtils.getItemAtHand());
										try {
											item.wdgmsg("itemact", 0);//merges
										} catch (Exception e) {
										}
										while (BotUtils.getItemAtHand() != null && BotUtils.getAmount(BotUtils.getItemAtHand()) == handAmount)//waits until the count changes to account for lag
											BotUtils.sleep(50);
									}
								}
							}
						if (BotUtils.getItemAtHand() != null) {//still have seeds on cursor, dropping them in an empty inventory slot
							lblProg2.settext("Dropping to inv");
							Coord slot = BotUtils.getFreeInvSlot(BotUtils.playerInventory());
							if (slot != null) {
								BotUtils.dropItemToInventory(slot, BotUtils.playerInventory());
								while (BotUtils.getItemAtHand() != null)
									BotUtils.sleep(50);
							}
						}
						if (BotUtils.invFreeSlots() == 0) {//inv full, time to barrel
							lblProg2.settext("Barreling");
							if (BotUtils.getItemAtHand() != null)
								BotUtils.dropItem(0);
							BotUtils.pfRightClick(containers.get(0), 0);
							if (containers.get(0).getres().basename().contains("barrel"))
								BotUtils.waitForWindow("Barrel");
							else
								BotUtils.waitForWindow("Trough");
							/*if (BotUtils.getItemAtHand() != null) {
								gameui().map.wdgmsg("itemact", Coord.z, containers.get(0).rc.floor(posres), 0, 0, (int) containers.get(0).id,
										containers.get(0).rc.floor(posres), 0, -1);
								int i = 0;
								while (BotUtils.getItemAtHand() != null) {
									if (i == 60000)
										break;
									if(containers.size() == 1 && i > 250){
										BotUtils.sysMsg("Only container in list appears to be full, stopping.",Color.white);
										stopThread = true;
										stop();
										break;
									}else if(i > 250){
										BotUtils.sysMsg("Container appears to be full, removing.",Color.white);
										Coord slot = BotUtils.getFreeInvSlot(BotUtils.playerInventory());
										BotUtils.dropItemToInventory(slot,BotUtils.playerInventory());
										BotUtils.sleep(250);
										containers.remove(0);
										BotUtils.pfRightClick(containers.get(0), 0);
										if (containers.get(0).getres().basename().contains("barrel"))
											BotUtils.waitForWindow("Barrel");
										else
											BotUtils.waitForWindow("Trough");
										break;
									}
									BotUtils.sleep(10);
									i++;
								}
							}*/
							GItem item = BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedName)).get(0).item;
							BotUtils.takeItem(item);
							while(BotUtils.getInventoryItemsByName(BotUtils.playerInventory(),seedName).size() > 0){
								System.out.println("Looping");
								if(BotUtils.getItemAtHand() == null){
									System.out.println("Hand null, breaking");
									break;
								}
								List<WItem> list = BotUtils.getInventoryItemsByName(BotUtils.playerInventory(),seedName);
								gameui().map.wdgmsg("itemact", Coord.z, containers.get(0).rc.floor(posres), 1, 0, (int) containers.get(0).id, containers.get(0).rc.floor(posres), 0, -1);
								int i = 0;
								while(BotUtils.getInventoryItemsByName(BotUtils.playerInventory(),seedName).size() == list.size()) {
									if(containers.size() == 1 && i > 250){
										BotUtils.sysMsg("Only container in list appears to be full, stopping.",Color.white);
										stopThread = true;
										stop();
										break;
									}else if(i > 250){
										BotUtils.sysMsg("Container appears to be full, removing.",Color.white);
										Coord slot = BotUtils.getFreeInvSlot(BotUtils.playerInventory());
										BotUtils.dropItemToInventory(slot,BotUtils.playerInventory());
										BotUtils.sleep(250);
										containers.remove(0);
										BotUtils.pfRightClick(containers.get(0), 0);
										if (containers.get(0).getres().basename().contains("barrel"))
											BotUtils.waitForWindow("Barrel");
										else
											BotUtils.waitForWindow("Trough");
										item = BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedName)).get(0).item;
										BotUtils.takeItem(item);
										break;
									}
									BotUtils.sleep(10);
									i++;
								}
							}
							BotUtils.sleep(250);
							if (BotUtils.getItemAtHand() != null) {//still have seeds on cursor, dropping them in an empty inventory slot
								gameui().map.wdgmsg("itemact",Coord.z, containers.get(0).rc.floor(posres), 0, 0, (int) containers.get(0).id, containers.get(0).rc.floor(posres), 0, -1);
							}
							if (BotUtils.getItemAtHand() != null) {//still have seeds on cursor, dropping them in an empty inventory slot
								lblProg2.settext("Dropping to inv");
								Coord slot = BotUtils.getFreeInvSlot(BotUtils.playerInventory());
								if (slot != null) {
									BotUtils.dropItemToInventory(slot, BotUtils.playerInventory());
									while (BotUtils.getItemAtHand() != null)
										BotUtils.sleep(10);
								}
							}
							/*while (BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedName)).size() != 0) {
								if (stopThread)
									break;
								GItem item = BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedName)).get(0).item;
								BotUtils.takeItem(item);

								gameui().map.wdgmsg("itemact", Coord.z, containers.get(0).rc.floor(posres), 0, 0, (int) containers.get(0).id,
										containers.get(0).rc.floor(posres), 0, -1);
								int i = 0;
								while (BotUtils.getItemAtHand() != null) {
									if (i == 60000)
										break;
									if(containers.size() == 1 && i > 250){
										BotUtils.sysMsg("Only container in list appears to be full, stopping.",Color.white);
										stopThread = true;
										stop();
										break;
									}else if(i > 250){
										BotUtils.sysMsg("Container appears to be full, removing.",Color.white);
										Coord slot = BotUtils.getFreeInvSlot(BotUtils.playerInventory());
										BotUtils.dropItemToInventory(slot,BotUtils.playerInventory());
										BotUtils.sleep(250);
										containers.remove(0);
										BotUtils.pfRightClick(containers.get(0), 0);
										if (containers.get(0).getres().basename().contains("barrel"))
											BotUtils.waitForWindow("Barrel");
										else
											BotUtils.waitForWindow("Trough");
										break;
									}
									BotUtils.sleep(10);
									i++;
								}
							}*/
						}
				} catch (NullPointerException | Loading | Resource.LoadException x) { }
			}
		else {
				try {
					if (containeronly) { // Put items into container if inventory is full
						GItem item;
						if (BotUtils.invFreeSlots() == 0) {
							lblProg2.settext("Barreling");
							BotUtils.pfRightClick(containers.get(0), 0);
							BotUtils.waitForWindow("Barrel");
							item = BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedName)).get(0).item;
							BotUtils.takeItem(item);
							while(BotUtils.getInventoryItemsByName(BotUtils.playerInventory(),seedName).size() > 0){
								System.out.println("Looping");
								if(BotUtils.getItemAtHand() == null){
									System.out.println("Hand null, breaking");
									break;
								}
								List<WItem> list = BotUtils.getInventoryItemsByName(BotUtils.playerInventory(),seedName);
								gameui().map.wdgmsg("itemact", Coord.z, containers.get(0).rc.floor(posres), 1, 0, (int) containers.get(0).id, containers.get(0).rc.floor(posres), 0, -1);
								while(BotUtils.getInventoryItemsByName(BotUtils.playerInventory(),seedName).size() == list.size()) {
									int i = 0;
									while (BotUtils.getInventoryItemsByName(BotUtils.playerInventory(), seedName).size() == list.size()) {
										if (containers.size() == 1 && i > 250) {
											BotUtils.sysMsg("Only container in list appears to be full, stopping.", Color.white);
											stopThread = true;
											stop();
											break;
										} else if (i > 250) {
											BotUtils.sysMsg("Container appears to be full, removing.", Color.white);
											Coord slot = BotUtils.getFreeInvSlot(BotUtils.playerInventory());
											BotUtils.dropItemToInventory(slot, BotUtils.playerInventory());
											BotUtils.sleep(250);
											containers.remove(0);
											BotUtils.pfRightClick(containers.get(0), 0);
											if (containers.get(0).getres().basename().contains("barrel"))
												BotUtils.waitForWindow("Barrel");
											else
												BotUtils.waitForWindow("Trough");
											item = BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedName)).get(0).item;
											BotUtils.takeItem(item);
											break;
										}
										BotUtils.sleep(10);
										i++;
									}
								}
							}
							BotUtils.sleep(250);
							if (BotUtils.getItemAtHand() != null) {//still have seeds on cursor, dropping them in an empty inventory slot
								gameui().map.wdgmsg("itemact",Coord.z, containers.get(0).rc.floor(posres), 0, 0, (int) containers.get(0).id, containers.get(0).rc.floor(posres), 0, -1);
							}
							if (BotUtils.getItemAtHand() != null) {//still have seeds on cursor, dropping them in an empty inventory slot
								lblProg2.settext("Dropping to inv");
								Coord slot = BotUtils.getFreeInvSlot(BotUtils.playerInventory());
								if (slot != null) {
									BotUtils.dropItemToInventory(slot, BotUtils.playerInventory());
									while (BotUtils.getItemAtHand() != null)
										BotUtils.sleep(10);
								}
							}
							/*if (BotUtils.getItemAtHand() != null) {
								gameui().map.wdgmsg("itemact", Coord.z, containers.get(0).rc.floor(posres), 0, 0, (int) containers.get(0).id,
										containers.get(0).rc.floor(posres), 0, -1);
								int i = 0;
								while (BotUtils.getItemAtHand() != null) {
									if (i == 60000)
										break;
									BotUtils.sleep(10);
									i++;
								}
							}*/
							/*while (BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedName)).size() != 0) {
								if (stopThread)
									break;
								item = BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedName)).get(0).item;
								BotUtils.takeItem(item);

								gameui().map.wdgmsg("itemact", Coord.z, containers.get(0).rc.floor(posres), 0, 0, (int) containers.get(0).id,
										containers.get(0).rc.floor(posres), 0, -1);
								int i = 0;
								while (BotUtils.getItemAtHand() != null) {
									if (i == 60000)
										break;
									BotUtils.sleep(10);
									i++;
								}
							}*/
						}
					}
						} catch(NullPointerException | Loading | Resource.LoadException p){
						}
					}
			cropsHarvested++;
			lblProg.settext(cropsHarvested + "/" + totalCrops);
		}
		if(replantcontainer) {
			lblProg2.settext("Barreling");
			if (BotUtils.getItemAtHand() != null)
				BotUtils.dropItem(0);
			BotUtils.pfRightClick(containers.get(0), 0);
			if(containers.get(0).getres().basename().contains("barrel"))
			BotUtils.waitForWindow("Barrel");
			else
				BotUtils.waitForWindow("Trough");
	
			while (BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedName)).size() != 0) {
				if (stopThread)
					break;
				GItem item = BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedName)).get(0).item;
				BotUtils.takeItem(item);
	
				gameui().map.wdgmsg("itemact", Coord.z, containers.get(0).rc.floor(posres), 0, 0, (int) containers.get(0).id,
						containers.get(0).rc.floor(posres), 0, -1);
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

	public void sort (List< WItem > items) {
		Collections.sort(items, (a, b) -> {
			QBuff aq = a.item.quality();
			QBuff bq = b.item.quality();
			if (aq == null || bq == null)
				return 0;
			else if (aq.q == bq.q)
				return 0;
			else if (aq.q > bq.q)
				return -1;
			else
				return 1;
		});
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