package haven.automation;

import haven.Button;
import haven.*;
import haven.Label;
import haven.Window;
import haven.purus.BotUtils;
import haven.purus.pbot.PBotAPI;

import java.awt.*;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import static haven.OCache.posres;

public class PepperBotRun extends Window implements Runnable {
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
	private boolean replant = false;
	private Gob chest, water, rowmarker, cauldron, barrel;
	private final int rowgap = 4200;
	private final int northtravel = 20000;
	private int section;
	public Widget craftall;
	private Boolean boilmode = false;
	private Coord finalloc;

	public PepperBotRun(Coord rc1, Coord rc2, boolean harvest, boolean destroy, boolean replant, Gob barrel, Gob water, Gob rowmarker, Gob cauldron, int section) {
		super(new Coord(120, 65), "Trellis Farmer");
		this.rc1 = rc1;
		this.rc2 = rc2;
		this.harvest = harvest;
		this.destroy = destroy;
		this.replant = replant;
		this.water = water;
		this.rowmarker = rowmarker;
		this.cauldron = cauldron;
		this.barrel = barrel;
		this.section = section;
		//this.chest = chest;

		// Initialise arraylists
		cropName.add("gfx/terobjs/plants/pepper");

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
		Window crafting = gameui().getwnd("Crafting");
		if (crafting == null) {
			BotUtils.sysMsg("Craft Window not open, open and retry.", Color.white);
			stop();
			return;
		}
		for (Widget a = crafting.lchild; a != null; a = a.prev) {
			for (Widget aa = a.lchild; aa != null; aa = aa.prev) {
				if (aa instanceof Button) {
					if (((Button) aa).text.text == "Craft All") {
						craftall = aa;
					}
				}
			}
		}
		if (harvest) {
			// Initialise crop list
			crops = Crops(true);
			tables = Tables();
			BotUtils.sysLogAppend("Crops : " + crops.size() + " Htables : " + tables.size(), "white");
			// Initialize progression label on window
			int totalCrops = crops.size();
			int cropsHarvested = 0;
			lblProg.settext(cropsHarvested + "/" + totalCrops);
			crops = Crops(true);
			for (Gob g : crops) {
				if (stopThread) // Checks if aborted
					return;


				// Check if stamina is under 30%, drink if needed
				GameUI gui = HavenPanel.lui.root.findchild(GameUI.class);
				IMeter.Meter stam = gui.getmeter("stam", 0);
				if (stam.a <= 60) {
					new Thread(new BeltDrink(gui), "BeltDrink").start();
					BotUtils.sleep(5000);
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

				if (BotUtils.invFreeSlots() < 4 && !stopThread) {
					boilmode = true;
					gui.act("travel", "hearth");
					BotUtils.sleep(6000);
					while (BotUtils.invFreeSlots() < 4 && !stopThread) {
						List<WItem> pepperlist = gameui().maininv.getItemsPartial("Peppercorn");
						//BotUtils.sysLogAppend("Peppercorns : "+pepperlist.size(),"white");
						if (pepperlist.size() == 0) //put pepper on tables
						{
							gui.act("travel", "hearth");
							BotUtils.sleep(6000);
							if (section == 2) {
								Gob player = gui.map.player();
								Coord location = player.rc.floor(posres);
								int x = location.x;
								int y = location.y - rowgap;
								finalloc = new Coord(x, y);
								gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
								BotUtils.sleep(2000);
							}else if (section != 1 && section != 2){
								Gob player = gui.map.player();
								Coord location = player.rc.floor(posres);
								int x = location.x;
								int y = location.y - ((rowgap*section)-rowgap);
								finalloc = new Coord(x, y);
								gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
								BotUtils.sleep(6000);
							}
							for (Gob htable : tables) {
								//BotUtils.sysLogAppend("In the htable array","white");
								pepperlist = gameui().maininv.getItemsPartial("Drupe");
								if (pepperlist.size() > 0) {
									//BotUtils.sysLogAppend("pepper still in inv, overlay of current table is : "+htable.ols.size(),"white");
									if (htable.ols.size() != 2) {
									//	BotUtils.sysLogAppend("empty table found","white");
										gui.map.wdgmsg("click", htable.sc, htable.rc.floor(posres), 3, 0, 0, (int) htable.id, htable.rc.floor(posres), 0, -1);
										BotUtils.waitForWindow("Herbalist Table");
										BotUtils.sleep(1000);
										for (Widget w = BotUtils.playerInventory().child; w != null; w = w.next) {
											//BotUtils.sysLogAppend("checking inv","white");
											if (w instanceof GItem && ((GItem) w).getname().contains("Pepper")) {
											//	BotUtils.sysLogAppend("Pepper Found","white");
												GItem item = (GItem) w;
												try {
												//	BotUtils.sysLogAppend("trying to transfer pepper","white");
													item.wdgmsg("transfer", Coord.z);
												} catch (NullPointerException qip) {BotUtils.sysLogAppend("Null pointer during pepper transfer.","white");}
											}
										}
									}
								}
							}
						}
						if(BotUtils.invFreeSlots() > 4)
							break;
						gui.map.wdgmsg("click", cauldron.sc, cauldron.rc.floor(posres), 3, 0, 0, (int) cauldron.id, cauldron.rc.floor(posres), 0, -1);
						BotUtils.waitForWindow("Cauldron");
						Window cwnd = gameui().getwnd("Cauldron");
						VMeter vm = cwnd.getchild(VMeter.class);
						IMeter vm2 = cwnd.getchild(IMeter.class);
						((Button) craftall).click();
						BotUtils.sleep(2000);
						if (vm.amount < 30) {
							List<Gob> allgobs = PBotAPI.getGobs();
							for (Gob gobz : allgobs){
								if (gobz.id == barrel.id){
									barrel = gobz;
									break;
								}
							}
							BotUtils.sysLogAppend("filling cauldron, barrel is : "+barrel.ols.size(),"white");
							BotUtils.sleep(600);
							Coord retain = barrel.rc.floor(posres);
							if (barrel.ols.size() == 0 && water != null) {
								//BotUtils.sysLogAppend("Barrel is empty refilling from cistern/well overlay size is : "+barrel.ols.size(),"white");
								PBotAPI.liftGob(barrel);
								BotUtils.sleep(1000);
								gui.map.wdgmsg("click", water.sc, water.rc.floor(posres), 3, 0, 0, (int) water.id, water.rc.floor(posres), 0, -1);
								BotUtils.sleep(3500);
								gui.map.wdgmsg("click", cauldron.sc, cauldron.rc.floor(posres), 3, 0, 0, (int) cauldron.id, cauldron.rc.floor(posres), 0, -1);
								BotUtils.sleep(1000);
								gui.map.wdgmsg("click", Coord.z, retain, 3, 0);
								BotUtils.sleep(1000);
								gui.map.wdgmsg("click", cauldron.sc, cauldron.rc.floor(posres), 3, 0, 0, (int) cauldron.id, cauldron.rc.floor(posres), 0, -1);
								BotUtils.sleep(1000);
								((Button) craftall).click();
								BotUtils.sleep(1000);
							} else {
							//	BotUtils.sysLogAppend("Barrel is not empty refilling from barrel overlay size is : "+barrel.ols.size(),"white");
								PBotAPI.liftGob(barrel);
								BotUtils.sleep(1000);
								gui.map.wdgmsg("click", cauldron.sc, cauldron.rc.floor(posres), 3, 0, 0, (int) cauldron.id, cauldron.rc.floor(posres), 0, -1);
								BotUtils.sleep(1000);
								gui.map.wdgmsg("click", Coord.z, retain, 3, 0);
								BotUtils.sleep(1000);
								gui.map.wdgmsg("click", cauldron.sc, cauldron.rc.floor(posres), 3, 0, 0, (int) cauldron.id, cauldron.rc.floor(posres), 0, -1);
								BotUtils.sleep(1000);
								((Button) craftall).click();
								BotUtils.sleep(1000);
							}
						}
						while (gui.prog >= 0) {
							BotUtils.sleep(10);
						}
						if (stam.a > 50)
							((Button) craftall).click();
					}
				}
				if(boilmode) {
					boilmode = false;
					gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
					BotUtils.sleep(2000);
					Gob player = gui.map.player();
					Coord location = player.rc.floor(posres);
					int x = location.x + northtravel;
					int y = location.y;
					finalloc = new Coord(x, y);
					gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
					BotUtils.sleep(5000);
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
