package haven.automation;

import haven.Button;
import haven.*;
import haven.Label;
import haven.Window;
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;

import java.awt.*;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.ConcurrentModificationException;
import java.util.List;

import static haven.OCache.posres;

public class PepperBotProRun extends Window implements Runnable {
	private Coord rc1, rc2;
	private ArrayList<Gob> crops, crops1, crops2, crops3, crops4 = new ArrayList<Gob>();
	private ArrayList<Gob> tables, tables1, tables2, tables3, tables4 = new ArrayList<Gob>();
	private ArrayList<Gob> tablesblacklist = new ArrayList<Gob>();
	private boolean stopThread = false;
	private Label lblProg, lblProg2;
	private ArrayList<String> cropName = new ArrayList<String>();
	private boolean harvest = false;
	private Gob htable;
	private Window cwnd;
	public int x,y;
	private Button stopBtn;
	private Gob water,cauldron, barrel, hfire;
	private final int rowgap = 4200;
	private final int travel = 20000;
	private int section, direction;
	private Coord retain;
	private Boolean boilmode = false;
	private Coord finalloc;


	public PepperBotProRun(ArrayList crops1, ArrayList crops2, ArrayList crops3, ArrayList crops4, ArrayList tables1, ArrayList tables2, ArrayList tables3,
                           ArrayList tables4, boolean harvest, Gob barrel, Gob water, Gob cauldron, int section, Gob hfire, int direction) {
		super(new Coord(140, 55), "Trellis Farmer");
		this.harvest = harvest;
		this.water = water;
		this.direction = direction;
		this.hfire = hfire;
		this.cauldron = cauldron;
		this.barrel = barrel;
		this.section = section;
		this.crops1 = crops1;
		this.crops2 = crops2;
		this.crops3 = crops3;
		this.crops4 = crops4;
		this.tables1 = tables1;
		this.tables2 = tables2;
		this.tables3 = tables3;
		this.tables4 = tables4;


		// Initialise arraylists
		cropName.add("gfx/terobjs/plants/pepper");

		Label lblstxt = new Label("Progress:");
		add(lblstxt, new Coord(15, 35));
		lblProg = new Label("Initialising...");
		add(lblProg, new Coord(70, 35));

		Label lblstxt2 = new Label("Status: ");
		add(lblstxt2, new Coord(15, 45));
		lblProg2 = new Label("Initialising...");
		add(lblProg2, new Coord(70, 45));

		stopBtn = new Button(120, "Stop") {
			@Override
			public void click() {
				stop();
			}
		};
		add(stopBtn, new Coord(10, 0));
	}

	public void run() {
		PBotUtils.sysMsg("Pepper Bot started!", Color.white);
		GameUI gui = gameui();

		gui.wdgmsg("act", "craft", "boiledpepper");
		PBotUtils.waitForWindow("Crafting");

while(true) {
	if (harvest) {
		retain = barrel.rc.floor(posres);
		// Initialise crop list
		if(section == 1){
			crops = crops1;
			tables = tables1;
		}else if(section == 2){
			crops = crops2;
			tables = tables2;
		}else if(section == 3){
			crops = crops3;
			tables = tables3;
		}else if(section == 4){
			crops = crops4;
			tables = tables4;
		}

		if (tables.size() == 0) {
			PBotUtils.sysMsg("No tables selected, stopping.", Color.white);
			stopThread = true;
			stop();
			return;
		}
		// Initialize progression label on window
		int totalCrops = crops.size();
		int cropsHarvested = 0;
		lblProg.settext(cropsHarvested + "/" + totalCrops);
		for (Gob g : crops) {
			if (stopThread) // Checks if aborted
				break;
try {
	if (PBotUtils.getEnergy() < 50) {
		List<WItem> porridge = PBotUtils.getInventoryItemsByName(PBotAPI.gui.maininv, "gfx/invobjs/porridge");
		if (porridge.size() > 0) {
			porridge.get(0).item.wdgmsg("iact", Coord.z, -1);
			FlowerMenu.setNextSelection("Eat");
			PBotUtils.sleep(2000);
		} else {
			if (PBotUtils.getEnergy() < 21) {
				PBotUtils.sysMsg("Starving and no porridge detected, stopping bot and logging out.", Color.white);
				stopThread = true;
				stop();
				gui.logoutChar();
				PBotUtils.sleep(1000);
				return;
			}
		}
	}
}catch(NullPointerException qqq){
	//probably null pointer for a reason, stop bot.
	PBotUtils.sysMsg("Null pointer exception trying to find food to eat, stopping bot for safety. Please tell Ardennes about this.",Color.white);
	stopThread = true;
	stop();
	return;
}

			// Check if stamina is under 30%, drink if needed
			gui = HavenPanel.lui.root.findchild(GameUI.class);
			IMeter.Meter stam = gui.getmeter("stam", 0);
			if (stam.a <= 60) {
				if (stopThread)
					break;
				lblProg2.settext("Drinking");
				PBotUtils.drink(true);
			}


			if (stopThread)
				break;

			int stageBefore = g.getStage();


			// Right click the crop
			try {
				if (stopThread)
					break;
				lblProg2.settext("Harvesting");
				PBotUtils.doClick(g, 3, 0);
			} catch (NullPointerException qq) {
				PBotUtils.sysMsg("Null pointer when harvesting, ping related?", Color.white);
			}

			int retryharvest = 0;

			// Wait for harvest menu to appear
			while (ui.root.findchild(FlowerMenu.class) == null) {
				if (stopThread)
					break;
				retryharvest++;
				PBotUtils.sleep(10);
				if (retryharvest >= 500) {
					PBotUtils.sysLogAppend("Retrying harvest", "white");
					lblProg2.settext("Retry Harvest");
					PBotUtils.doClick(g, 3, 0);
					retryharvest = 0;
				}


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
				if (stopThread)
					break;
				retryharvest++;
				if (retryharvest >= 500) {
					PBotUtils.sysLogAppend("Retrying harvest", "white");
					lblProg2.settext("Retry Harvest");
					PBotUtils.doClick(g, 3, 0);
					retryharvest = 0;
				}
				if (PBotUtils.findObjectById(g.id) == null
						|| PBotUtils.findObjectById(g.id).getStage() != stageBefore)
					break;
				else
					PBotUtils.sleep(20);
			}

			if (PBotUtils.invFreeSlots() < 4 && !stopThread) {
				List<Gob> goblist = PBotUtils.getGobs(); //needed to ensure that the table overlay is correct for referencing later
				boilmode = true;
				gui.act("travel", "hearth");
				PBotUtils.sleep(6000);
				while (PBotUtils.invFreeSlots() < 4 && !stopThread) {
					if (stopThread) // Checks if aborted
						break;
					List<WItem> pepperlist = gameui().maininv.getItemsPartial("Peppercorn");
					if (pepperlist.size() == 0) {
						lblProg2.settext("Tables");
						PBotUtils.sleep(1000);
						gui.map.wdgmsg("click", hfire.sc, hfire.rc.floor(posres), 1, 0, 0, (int) hfire.id, hfire.rc.floor(posres), 0, -1);
						PBotUtils.sleep(2000);
						if (section == 2) {
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
							PBotUtils.sleep(2000);
						} else if (section != 1 && section != 2) {
							//	System.out.println("section 3/4");
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

							PBotUtils.sleep(6000);
						}
						while (gui.maininv.getItemPartialCount("Drupe") > 0) {
							if (stopThread) // Checks if aborted
								break;
							lblProg2.settext("Tables");
							while (htable == null) {
								if (tables.size() == 0) {
									PBotUtils.sysMsg("Tables is now empty for some reason, all tables full?", Color.white);
									stopBtn.click();
									break;
								}

								for (Gob tablelol : tables) {
									for (Gob idklol : goblist)
										if (idklol.id == tablelol.id)
											tablelol = idklol;
									if (tablelol.ols.size() != 2) {
										//	System.out.println("Found not full table, id : "+tablelol.id);
										htable = tablelol;
										break;
									} else {
										tablesblacklist.add(tablelol);
										//   System.out.println("Blacklisting table : "+tablelol.id);
									}
								}
								tables.removeAll(tablesblacklist);
								tablesblacklist.clear();
							}

							//	System.out.println("clicking table, tables size : "+tables.size()+" blacklist size "+tablesblacklist.size()+" gob id : "+htable.id);
							PBotUtils.doClick(htable,3,0);
							//BotUtils.pfRightClick(htable, 0);
							int retry = 0;
							while (gui.getwnd("Herbalist Table") == null) {
								retry++;
								if (retry > 500) {
									retry = 0;
									//	System.out.println("retrying table");
									PBotUtils.doClick(htable, 3,0);
								//	BotUtils.pfRightClick(htable, 0);
								}
								PBotUtils.sleep(10);
							}
							PBotUtils.sleep(100);
							cwnd = gui.getwnd("Herbalist Table");
							// System.out.println("Getting pepper from inv");
							PBotUtils.sleep(2000);
							for (Widget w = PBotAPI.gui.maininv.child; w != null; w = w.next) {
								if (w instanceof GItem && ((GItem) w).getname().contains("Pepper")) {
									GItem item = (GItem) w;
									try {
										item.wdgmsg("transfer", Coord.z);
									} catch (NullPointerException qip) {
									}
								}
							}
							PBotUtils.sleep(1500);
							PBotUtils.doClick(htable, 3, 0);
							PBotUtils.waitForWindow("Herbalist Table");
							if (gui.getwnd("Herbalist Table") != null) {
								cwnd = gui.getwnd("Herbalist Table");
								for (Widget w = cwnd.lchild; w != null; w = w.prev) {
									if (w instanceof Inventory) {
										int drupes = PBotUtils.getInventoryContents((Inventory) w).size();
										//	System.out.println("Pepper on table : "+drupes);
										if (drupes == 16) {
											tables.remove(htable);
											//  System.out.println("Table full, removing : "+htable.id);
											break;
										}
									}
								}
							}
							htable = null;
							cwnd = null;
						}
					}

					gui = HavenPanel.lui.root.findchild(GameUI.class);
					stam = gui.getmeter("stam", 0);
					if (stam.a <= 60) {
						if (stopThread)
							break;
						lblProg2.settext("Drinking");
						PBotUtils.drink(true);
					}

					if (PBotUtils.invFreeSlots() > 4)
						break;
					pepperlist.clear();
					lblProg2.settext("Boiling");
					PBotUtils.pfRightClick(cauldron, 0);
					//gui.map.wdgmsg("click", cauldron.sc, cauldron.rc.floor(posres), 3, 0, 0, (int) cauldron.id, cauldron.rc.floor(posres), 0, -1);
					FlowerMenu.setNextSelection("Open");
					int tryagaintimer = 0;
					gui = gameui();
					while (gameui().getwnd("Cauldron") == null) {
						if (stopThread) // Checks if aborted
							break;
						PBotUtils.sleep(10);
						try {
							Thread.sleep(10);
							tryagaintimer++;
							if (tryagaintimer >= 500) {
								tryagaintimer = 0;
								PBotUtils.sysLogAppend("Retrying cauldron open", "white");
								PBotUtils.pfRightClick(cauldron, 0);
								//	gui.map.wdgmsg("click", cauldron.sc, cauldron.rc.floor(posres), 3, 0, 0, (int) cauldron.id, cauldron.rc.floor(posres), 0, -1);
								FlowerMenu.setNextSelection("Open");
							}
						} catch (InterruptedException idk) {
						}
					}
					PBotUtils.sleep(500);
					//System.out.println("after cauldron window");
					//BotUtils.waitForWindow("Cauldron");
					cwnd = gameui().getwnd("Cauldron");
					PBotUtils.sleep(200);
					VMeter vm = cwnd.getchild(VMeter.class);
					//	System.out.println("Clicking craft");
					PBotUtils.craftItem("boiledpepper",1);
					//System.out.println("after Clicking craft");
					PBotUtils.sleep(2000);

					if (vm.amount < 30)
						RefillCauldron(gameui());

					while (gui.prog >= 0) {
						if (stopThread) // Checks if aborted
							break;
						lblProg2.settext("Boiling");
						PBotUtils.sleep(10);
					}
					if (stam.a > 50) {
						PBotUtils.craftItem("boiledpepper",1);
					}

				}
			}
			if (boilmode) {
				if (stopThread) // Checks if aborted
					break;
				lblProg2.settext("Moving to harvest");
				boilmode = false;
				gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
				PBotUtils.sleep(2000);
				Gob player = gui.map.player();
				Coord location = player.rc.floor(posres);
				if (direction == 1) {
					x = location.x + travel;
					y = location.y;
				}
				if (direction == 2) {
					x = location.x - travel;
					y = location.y;
				}
				if (direction == 3) {
					x = location.x;
					y = location.y + travel;
				}
				if (direction == 4) {
					x = location.x;
					y = location.y - travel;
				}
				finalloc = new Coord(x, y);
				gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
				PBotUtils.sleep(5000);
			}
			// Update progression
			cropsHarvested++;
			lblProg.settext(cropsHarvested + "/" + totalCrops);
		}
	}
	PBotUtils.sysMsg("Section finished!", Color.white);
	gui.act("travel", "hearth");
	PBotUtils.sleep(6000);
	lblProg2.settext("Moving to harvest");
	if(section ==1) {
		section = 2;
		crops = null;
		tables = null;
	}
	else if(section ==2) {
		section = 3;
		crops = null;
		tables = null;
	}
	else if(section == 3) {
		section = 4;
		crops = null;
		tables = null;
	}
	else{
		stopThread = true;
		stop();
		return;
	}
	if (section == 2) {
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
		PBotUtils.sleep(2000);
		player = gui.map.player();
		location = player.rc.floor(posres);
		if (direction == 1) {
			x = location.x + travel;
			y = location.y;
		}
		if (direction == 2) {
			x = location.x - travel;
			y = location.y;
		}
		if (direction == 3) {
			x = location.x;
			y = location.y + travel;
		}
		if (direction == 4) {
			x = location.x;
			y = location.y - travel;
		}
		finalloc = new Coord(x, y);
		gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
		PBotUtils.sleep(5000);
	} else if (section != 1 && section != 2) {
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
		PBotUtils.sleep(6000);
		player = gui.map.player();
		location = player.rc.floor(posres);
		if (direction == 1) {
			x = location.x + travel;
			y = location.y;
		}
		if (direction == 2) {
			x = location.x - travel;
			y = location.y;
		}
		if (direction == 3) {
			x = location.x;
			y = location.y + travel;
		}
		if (direction == 4) {
			x = location.x;
			y = location.y - travel;
		}
		finalloc = new Coord(x, y);
		gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
		PBotUtils.sleep(5000);
	}
}
	}

	@Override
	public void wdgmsg(Widget sender, String msg, Object... args) {
		if (sender == cbtn) {
			stop();
			reqdestroy();
		} else
			super.wdgmsg(sender, msg, args);
	}

	private void RefillCauldron(GameUI gui){
		try {
			List<Gob> allgobs = PBotUtils.getGobs();
			for (Gob gobz : allgobs) {
				if (gobz.id == barrel.id) {
					barrel = gobz;
					break;
				}
			}
		} catch (ConcurrentModificationException idklolok) {
		}
		//	BotUtils.sysLogAppend("filling cauldron, barrel is : "+barrel.ols.size(),"white");
		PBotUtils.sleep(600);
		if (barrel.ols.size() == 0 && water != null) {
			lblProg2.settext("Refill Barrel");
			//	System.out.println("Refill Barrel");
			//BotUtils.sysLogAppend("Barrel is empty refilling from cistern/well overlay size is : "+barrel.ols.size(),"white");
			PBotUtils.liftGob(barrel);
			PBotUtils.sleep(2000);
			gui.map.wdgmsg("click", water.sc, water.rc.floor(posres), 3, 0, 0, (int) water.id, water.rc.floor(posres), 0, -1);
			PBotUtils.sleep(3500);
			gui.map.wdgmsg("click", cauldron.sc, cauldron.rc.floor(posres), 3, 0, 0, (int) cauldron.id, cauldron.rc.floor(posres), 0, -1);
			FlowerMenu.setNextSelection("Open");
			PBotUtils.sleep(2000);
			gui.map.wdgmsg("click", Coord.z, retain, 3, 0);
			PBotUtils.sleep(2000);
			gui.map.wdgmsg("click", cauldron.sc, cauldron.rc.floor(posres), 3, 0, 0, (int) cauldron.id, cauldron.rc.floor(posres), 0, -1);
			FlowerMenu.setNextSelection("Open");
			PBotUtils.sleep(2000);
			PBotUtils.craftItem("boiledpepper",1);
			PBotUtils.sleep(2000);
		} else {
			lblProg2.settext("Refill Cauldron");
			//	BotUtils.sysLogAppend("Barrel is not empty refilling from barrel overlay size is : "+barrel.ols.size(),"white");
			//	System.out.println("Refill Cauldron");
			PBotUtils.liftGob(barrel);
			PBotUtils.sleep(2000);
			gui.map.wdgmsg("click", cauldron.sc, cauldron.rc.floor(posres), 3, 0, 0, (int) cauldron.id, cauldron.rc.floor(posres), 0, -1);
			//FlowerMenu.setNextSelection("Open");
			PBotUtils.sleep(2000);
			gui.map.wdgmsg("click", Coord.z, retain, 3, 0);
			PBotUtils.sleep(2000);
			gui.map.wdgmsg("click", cauldron.sc, cauldron.rc.floor(posres), 3, 0, 0, (int) cauldron.id, cauldron.rc.floor(posres), 0, -1);
			FlowerMenu.setNextSelection("Open");
			PBotUtils.sleep(2000);
			PBotUtils.craftItem("boiledpepper",1);
			PBotUtils.sleep(2000);
		}
	}

	public void stop() {
		// Stops thread
		PBotUtils.sysMsg("Trellis Farmer stopped!", Color.white);
		gameui().map.wdgmsg("click", Coord.z, gameui().map.player().rc.floor(posres), 1, 0);
		if (gameui().map.pfthread != null) {
			gameui().map.pfthread.interrupt();
		}
		stopThread = true;
		harvest = false;
		section = 4;
		this.destroy();
	}
}
