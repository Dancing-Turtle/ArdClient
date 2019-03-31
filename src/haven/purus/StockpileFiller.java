package haven.purus;

import static haven.OCache.posres;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collections;

import haven.*;
import haven.automation.GobSelectCallback;
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;

public class StockpileFiller extends Window implements GobSelectCallback, ItemClickCallback {

	private ArrayList<Gob> stockpiles = new ArrayList<Gob>();
	private String invobj, terobj;

	private boolean stop = false;
	private boolean terobjCallback = false;
	private Button clearBtn,startBtn;

	public StockpileFiller() {
		super(new Coord(270, 200), "Stockpile Filler");

		Widget inf = add(new Widget(new Coord(245, 30)) {
			public void draw(GOut g) {
				g.chcolor(0, 0, 0, 128);
				g.frect(Coord.z, sz);
				g.chcolor();
				super.draw(g);
			}

		}, new Coord(10, 10).add(wbox.btloff()));
		Frame.around(this, Collections.singletonList(inf));
		Label infolbl = inf.add(new Label("Alt + Click to select stockpiles"), new Coord(5, 0));
		int y = 40;
		Button invobjBtn = new Button(140, "Choose item from inventory") {
			@Override
			public void click() {
				PBotUtils.sysMsg("Click the stockpile item in inventory", Color.GREEN);
				registerItemCallback();
			}
		};
		add(invobjBtn, new Coord(20, y));
		y += 35;
		Button terobjBtn = new Button(140, "Choose item from ground") {
			@Override
			public void click() {
				terobjCallback = true;
				PBotUtils.sysMsg("Alt + Click to select ground item", Color.GREEN);
			}
		};
		add(terobjBtn, new Coord(20, y));
		y += 35;
		clearBtn = new Button(140, "Clear/Stop") {
			@Override
			public void click() {
				startBtn.show();
				stop = true;
				if (t!=null)
					t.interrupt();
				stockpiles.clear();
				PBotUtils.sysMsg("Cleared the list of selected stockpiles", Color.GREEN);
			}
		};
		add(clearBtn, new Coord(20, y));
		y += 35;
		startBtn = new Button(140, "Start") {
			@Override
			public void click() {
				this.hide();
				stop = false;
				if(stockpiles.isEmpty()) {
					PBotUtils.sysMsg("No stockpiles chosen!", Color.GREEN);
				} else if(terobj==null) {
					PBotUtils.sysMsg("No ground item chosen!", Color.GREEN);
				} else if(invobj==null) {
					PBotUtils.sysMsg("No inventory item chosen!", Color.GREEN);
				} else {
				 t.start();
				}
			}
		};
		add(startBtn, new Coord(20, y));
		y += 35;
	}

	Thread t = new Thread(new Runnable() {
		public void run() {
			main:
			try {
				while (PBotUtils.findObjectByNames(5000, terobj) != null) {
					System.out.println("In main loop");
					if (stop)
						break;
					while(PBotUtils.getItemAtHand() == null){
						if (stop)
							break;
						if (PBotUtils.findObjectByNames(5000, terobj) == null) {
							PBotUtils.sysLogAppend("Out of items to stockpile, finishing.", "white");
							stop = true;
							break;
						}
						PBotUtils.sysLogAppend("Grabbing stuff.", "white");
						Gob g = PBotUtils.findObjectByNames(5000, terobj);
						gameui().map.wdgmsg("click", g.sc, g.rc.floor(posres), 3, 1, 0, (int) g.id, g.rc.floor(posres), 0, -1);
						PBotUtils.sleep(1000);

						while(PBotUtils.getItemAtHand() == null & PBotUtils.findObjectByNames(5000,terobj)!=null && PBotUtils.isMoving()) {
						System.out.println("waiting for item on  hand");
							PBotUtils.sleep(10);
						}
						System.out.println("inv free slots : "+PBotUtils.invFreeSlots());
					}

					PBotUtils.sysLogAppend("Done Grabbing stuff.", "white");
					if (stop)
						break;
					while (PBotUtils.getInventoryItemsByName(PBotAPI.gui.maininv, invobj).size() != 0 && !stop) {
						if (stop)
							break;
						System.out.println("In stockpile loop");
						PBotUtils.sleep(1000);
						if (PBotUtils.getItemAtHand() != null)
							PBotUtils.dropItem(0);
						if (stockpiles.isEmpty()) {
							System.out.println("Stockpiles empty");
							PBotUtils.sysMsg("All chosen stockpiles full!", Color.GREEN);
							stop = true;
							break;
						}

						if (PBotUtils.stockpileIsFull(PBotUtils.findObjectById(stockpiles.get(0).id))) {
							System.out.println("Stockpile full");
							stockpiles.remove(0);
							continue;
						}
						if (stop)
							break;
						if(stockpiles.size() == 0){
							PBotUtils.sysMsg("Stockpile list now empty, stopping.",Color.white);
							stop = true;
							stop();
						}
						PBotUtils.pfRightClick(stockpiles.get(0), 0);
						int retry = 0;
						while (gameui().getwnd("Stockpile") == null) {
							if(!PBotUtils.isMoving())
							retry++;
							if (retry > 100) {
								if (stop)
									break;
								retry = 0;
								System.out.println("Retry : "+retry);
								PBotUtils.sysLogAppend("Retrying stockpile interaction", "white");
								PBotUtils.dropItem(0);
								PBotUtils.pfRightClick(stockpiles.get(0), 0);
							}
							PBotUtils.sleep(10);
						}
						PBotUtils.sleep(1000);
						System.out.println("clicking stockpile");
						try {
							while (PBotUtils.getItemAtHand() == null)
								PBotUtils.takeItem(PBotUtils.getInventoryItemsByName(PBotAPI.gui.maininv, invobj).get(0).item);
						}catch(NullPointerException q){
							//break on null pointer here, bot is prob done
							stop = true;
							break main;
						}
						int cnt = PBotUtils.invFreeSlots();
						try {
							PBotAPI.gui.map.wdgmsg("itemact", Coord.z, stockpiles.get(0).rc.floor(posres), 1, 0, (int) stockpiles.get(0).id, stockpiles.get(0).rc.floor(posres), 0, -1);
						}catch(IndexOutOfBoundsException lolindexes){
							PBotUtils.sysMsg("Critical error in stockpile list, stopping thread to prevent crash.",Color.white);
							stop = true;
							stop();
						}
						while (PBotUtils.invFreeSlots() == cnt) {
							System.out.println("waiting for inv update");
							PBotUtils.sleep(100);
						}
					}
					if (PBotUtils.findObjectByNames(5000, terobj) == null)
						break;
				}
				PBotUtils.sysMsg("Stockpile Filler finished!", Color.GREEN);
				startBtn.show();
				reqdestroy();
			}catch(Loading | NullPointerException q){}
		}
	});
	
	private void registerItemCallback() {
		synchronized (GobSelectCallback.class) {
			PBotAPI.gui.registerItemCallback(this);
    	}
	}

	@Override
	public void gobselect(Gob gob) {
		if(terobjCallback) {
			terobjCallback = false;
			terobj = gob.getres().name;
			PBotUtils.sysMsg("Ground item chosen!", Color.GREEN);
		} else if (gob.getres().basename().contains("stockpile")) {
			stockpiles.add(gob);
			PBotUtils.sysMsg("Stockpile added to list! Total stockpiles chosen: " + stockpiles.size(), Color.GREEN);
		}
		synchronized (GobSelectCallback.class) {
			PBotAPI.gui.map.registerGobSelect(this);
    	}
	}
	
	@Override
	public void itemClick(WItem item) {
		invobj = item.item.getres().name;
		PBotUtils.sysMsg("Inventory item to put in the stockpiles chosen!", Color.GREEN);
		synchronized (ItemClickCallback.class) {
			PBotAPI.gui.unregisterItemCallback();
    	}
	}

	@Override
	public void wdgmsg(Widget sender, String msg, Object... args) {
		if (msg == "close") {
			stop();
			reqdestroy();
		} else
			super.wdgmsg(sender, msg, args);
	}

	public void stop() {
		t.interrupt();
		stop = true;
		synchronized (ItemClickCallback.class) {
    		PBotAPI.gui.unregisterItemCallback();
    	}
		synchronized (ItemClickCallback.class) {
    		PBotAPI.gui.unregisterItemCallback();
    	}
		reqdestroy();
		//gameui().map.wdgmsg("click", Coord.z, new Coord((int)BotUtils.player().rc.x, (int)BotUtils.player().rc.y), 1, 0);
	}
}