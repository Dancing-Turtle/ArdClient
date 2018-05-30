package haven.purus;

import static haven.OCache.posres;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collections;

import haven.Button;
import haven.Coord;
import haven.Frame;
import haven.GOut;
import haven.Gob;
import haven.Label;
import haven.WItem;
import haven.Widget;
import haven.Window;
import haven.automation.GobSelectCallback;

public class StockpileFiller extends Window implements GobSelectCallback, ItemClickCallback {

	private ArrayList<Gob> stockpiles = new ArrayList<Gob>();
	private String invobj, terobj;

	private boolean stop = false;
	private boolean terobjCallback = false;

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
				BotUtils.sysMsg("Click the stockpile item in inventory", Color.GREEN);
				registerItemCallback();
			}
		};
		add(invobjBtn, new Coord(20, y));
		y += 35;
		Button terobjBtn = new Button(140, "Choose item from ground") {
			@Override
			public void click() {
				terobjCallback = true;
				BotUtils.sysMsg("Alt + Click to select ground item", Color.GREEN);
			}
		};
		add(terobjBtn, new Coord(20, y));
		y += 35;
		Button clearBtn = new Button(140, "Clear") {
			@Override
			public void click() {
				stockpiles.clear();
				BotUtils.sysMsg("Cleared the list of selected stockpiles", Color.GREEN);
			}
		};
		add(clearBtn, new Coord(20, y));
		y += 35;
		Button startBtn = new Button(140, "Start") {
			@Override
			public void click() {
				if(stockpiles.isEmpty()) {
					BotUtils.sysMsg("No stockpiles chosen!", Color.GREEN);
				} else if(terobj==null) {
					BotUtils.sysMsg("No ground item chosen!", Color.GREEN);
				} else if(invobj==null) {
					BotUtils.sysMsg("No inventory item chosen!", Color.GREEN);
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
			while(true) {
				Gob gob = BotUtils.findObjectByNames(1000, terobj);
				if(gob == null) {
					BotUtils.sysMsg("No more items on ground found!", Color.GREEN);
					break;
				}
				while(gob != null && BotUtils.getItemAtHand() == null) {
					if(stop)
						break;
					BotUtils.doClick(gob, 3, 1);
					while(BotUtils.findObjectById(gob.id) != null && BotUtils.getItemAtHand() == null) {
						BotUtils.sleep(100);
					}
					gob = BotUtils.findObjectByNames(1000, terobj);
				}
				
				if(BotUtils.getItemAtHand()==null && BotUtils.getInventoryItemsByName(BotUtils.playerInventory(), invobj).size()>0) {
					BotUtils.takeItem(BotUtils.getInventoryItemsByName(BotUtils.playerInventory(), invobj).get(0).item);
				}
				while(BotUtils.getItemAtHand() != null && !stop) {
					if(stockpiles.isEmpty()) {
						BotUtils.sysMsg("All chosen stockpiles full!", Color.GREEN);
						stop = true;
						break;
					}
					
					if(BotUtils.stockpileIsFull(BotUtils.findObjectById(stockpiles.get(0).id))) {
						stockpiles.remove(0);
						continue;
					}
					if(stop)
						break;
					BotUtils.gui.map.wdgmsg("itemact", Coord.z, stockpiles.get(0).rc.floor(posres), 1, 0, (int) stockpiles.get(0).id, stockpiles.get(0).rc.floor(posres), 0, -1);
					int cnt = BotUtils.invFreeSlots();
					while(BotUtils.invFreeSlots() == cnt) {
						BotUtils.sleep(100);
					}
					if(BotUtils.getItemAtHand()==null && BotUtils.getInventoryItemsByName(BotUtils.playerInventory(), invobj).size()>0) {
						BotUtils.takeItem(BotUtils.getInventoryItemsByName(BotUtils.playerInventory(), invobj).get(0).item);
					}
				}
				
			}
			BotUtils.sysMsg("Stockpile Filler finished!", Color.GREEN);
			reqdestroy();
		}
	});
	
	private void registerItemCallback() {
		synchronized (GobSelectCallback.class) {
    		BotUtils.gui.registerItemCallback(this);
    	}
	}

	@Override
	public void gobselect(Gob gob) {
		if(terobjCallback) {
			terobjCallback = false;
			terobj = gob.getres().name;
			BotUtils.sysMsg("Ground item chosen!", Color.GREEN);
		} else if (gob.getres().basename().contains("stockpile")) {
			stockpiles.add(gob);
			BotUtils.sysMsg("Stockpile added to list! Total stockpiles chosen: " + stockpiles.size(), Color.GREEN);
		}
		synchronized (GobSelectCallback.class) {
    		BotUtils.gui.map.registerGobSelect(this);
    	}
	}
	
	@Override
	public void itemClick(WItem item) {
		invobj = item.item.getres().name;
		BotUtils.sysMsg("Inventory item to put in the stockpiles chosen!", Color.GREEN);
		synchronized (ItemClickCallback.class) {
    		BotUtils.gui.unregisterItemCallback();
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
		stop = true;
		synchronized (ItemClickCallback.class) {
    		BotUtils.gui.unregisterItemCallback();
    	}
		synchronized (ItemClickCallback.class) {
    		BotUtils.gui.unregisterItemCallback();
    	}
		reqdestroy();
		//gameui().map.wdgmsg("click", Coord.z, new Coord((int)BotUtils.player().rc.x, (int)BotUtils.player().rc.y), 1, 0);
	}
}