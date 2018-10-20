package haven.purus;

import static haven.OCache.posres;

import java.awt.Color;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import haven.*;
import haven.automation.GobSelectCallback;

public class TroughFiller extends Window implements GobSelectCallback {

	private Gob trough;

	private List<String> invobjs = Arrays.asList("gfx/invobjs/carrot", "gfx/invobjs/beet", "gfx/invobjs/beetleaves", "gfx/invobjs/pumpkin");
	private String[] terobjs = { "gfx/terobjs/items/carrot", "gfx/terobjs/items/beet", "gfx/terobjs/items/beetleaves","gfx/terobjs/items/pumpkin" };

	private boolean stop = false;

	private Button stopBtn;

	public TroughFiller() {
		super(new Coord(270, 100), "Trough Filler");

		Widget inf = add(new Widget(new Coord(245, 30)) {
			public void draw(GOut g) {
				g.chcolor(0, 0, 0, 128);
				g.frect(Coord.z, sz);
				g.chcolor();
				super.draw(g);
			}

		}, new Coord(10, 10).add(wbox.btloff()));
		Frame.around(this, Collections.singletonList(inf));
		Label infolbl = inf.add(new Label("Alt + Click to select trough"), new Coord(5, 0));
		stopBtn = new Button(120, "Stop") {
			@Override
			public void click() {
				stop();
			}
		};
		add(stopBtn, new Coord(75, 65));
	}

	Thread t = new Thread(new Runnable() {
		public void run() {
			try {
				main:
				while (true) {
					int timeout = 0;
					while(BotUtils.findObjectByNames(1000,terobjs) == null) {
						//System.out.println("While loop : " + timeout);
						timeout++;
						if(stop)
							break main;
						if (timeout >= 100) {
							timeout = 0;
							stop();
							break;
						}
						BotUtils.sleep(50);
					}
					if (BotUtils.findObjectByNames(1000, terobjs) == null
							&& BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), invobjs).size() == 0)
						break;
					while(BotUtils.getItemAtHand() == null){
					//while (BotUtils.invFreeSlots() > 0) {
						if (stop)
							break main;


						if (BotUtils.findObjectByNames(1000, terobjs) == null)
							break;

						Gob g = BotUtils.findObjectByNames(1000, terobjs);
						gameui().map.wdgmsg("click", g.sc, g.rc.floor(posres),3,1,0,(int)g.id,g.rc.floor(posres),0,-1);
						BotUtils.sleep(1000);
					//	gui.map.wdgmsg("click", cistern.sc, cistern.rc.floor(posres), 3, 0, 0, (int) cistern.id, cistern.rc.floor(posres), 0, -1);
						//BotUtils.pfRightClick(g, 0);
						while(BotUtils.getItemAtHand() == null & BotUtils.findObjectByNames(1000,terobjs)!=null && BotUtils.isMoving())
							BotUtils.sleep(10);
						/*int i = 0;
						while (BotUtils.findObjectById(g.id) != null) {
							if (i == 100)
								break;
							BotUtils.sleep(50);
							i++;
						}*/
					}

					if (stop)
						break main;

					if (BotUtils.getItemAtHand() != null)
						BotUtils.dropItem(0);

					BotUtils.pfRightClick(trough, 0);
					int retry = 0;
					while(gameui().getwnd("Trough")==null){
						retry++;
						if(retry > 500){
							retry = 0;
							BotUtils.sysLogAppend("Retrying trough interaction","white");
							BotUtils.dropItem(0);
							BotUtils.pfRightClick(trough,0);
						}
						BotUtils.sleep(10);
					}
					//BotUtils.waitForWindow("Trough");

					while (BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), invobjs).size() != 0 && !stop) {
						if (stop)
							break main;
						GItem item = BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), invobjs).get(0).item;
						BotUtils.takeItem(item);

						gameui().map.wdgmsg("itemact", Coord.z, trough.rc.floor(posres), 0, 0, (int) trough.id,
								trough.rc.floor(posres), 0, -1);
						int i = 0;
						while (BotUtils.getItemAtHand() != null) {
							if (i == 60000)
								break main;
							BotUtils.sleep(10);
							i++;
						}
					}

				}
			}catch(Loading | Resource.LoadException | NullPointerException idklol) {BotUtils.sysLogAppend("Error captured in main thread.","white");}
			BotUtils.sysMsg("Trough Filler finished", Color.WHITE);
			reqdestroy();
		}
	});

	@Override
	public void gobselect(Gob gob) {
		if (gob.getres().basename().contains("trough")) {
			trough = gob;
			t.start();
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
		BotUtils.sysMsg("Stopping Trough Filler",Color.white);
		this.destroy();
	//	gameui().map.wdgmsg("click", Coord.z, new Coord((int)BotUtils.player().rc.x, (int)BotUtils.player().rc.y), 1, 0);
	}
}