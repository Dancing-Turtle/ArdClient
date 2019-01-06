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
		Label infolbl = inf.add(new Label("Alt + Click to select trough/compost bin"), new Coord(5, 0));
		stopBtn = new Button(120, "Stop") {
			@Override
			public void click() { stop(); }
		};
		add(stopBtn, new Coord(75, 65));
	}

	Thread t = new Thread(new Runnable() {
		public void run() {

				main:
				while (!stop) {
					try {

						if (stop) {
							System.out.println("stop detected ,breaking");
							return;
						}
					System.out.println("Main loop start");
					int timeout = 0;
					while(BotUtils.findObjectByNames(5000,terobjs) == null) {
						System.out.println("Waiting to detect objects");
						timeout++;
						if(stop) {
							stop = true;
							break main;
						}
						if (timeout >= 100) {
							System.out.println("Timeout expired, stopping");
							stop();
							break;
						}
						BotUtils.sleep(50);
					}
					if (BotUtils.findObjectByNames(5000, terobjs) == null && BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), invobjs).size() == 0) {
						System.out.println("No objects on ground or in inv, breaking.");
						break;
					}
					while(BotUtils.getItemAtHand() == null){
						System.out.println("Waiting for item on cursor");
					//while (BotUtils.invFreeSlots() > 0) {
						if (stop) {
							System.out.println("Stop detected, breaking.");
							stop = true;
							break main;
						}


						if (BotUtils.findObjectByNames(5000, terobjs) == null) {
							System.out.println("No objects on ground, breaking.");
							break;
						}

						Gob g = BotUtils.findObjectByNames(5000, terobjs);
						gameui().map.wdgmsg("click", g.sc, g.rc.floor(posres),3,1,0,(int)g.id,g.rc.floor(posres),0,-1);
						BotUtils.sleep(1000);
					//	gui.map.wdgmsg("click", cistern.sc, cistern.rc.floor(posres), 3, 0, 0, (int) cistern.id, cistern.rc.floor(posres), 0, -1);
						//BotUtils.pfRightClick(g, 0);
						while(BotUtils.getItemAtHand() == null & BotUtils.findObjectByNames(5000,terobjs)!=null && BotUtils.isMoving())
							BotUtils.sleep(10);
						/*int i = 0;
						while (BotUtils.findObjectById(g.id) != null) {
							if (i == 100)
								break;
							BotUtils.sleep(50);
							i++;
						}*/
					}

					if (stop) {
						System.out.println("stop detected ,breaking");
						return;
					}

					BotUtils.sleep(500);

					if (BotUtils.getItemAtHand() != null)
						BotUtils.dropItem(0);

					BotUtils.pfRightClick(trough, 0);
					int retry = 0;
					while(gameui().getwnd("Trough") == null && gameui().getwnd("Compost Bin") == null){
						retry++;
						if(retry > 500){
							retry = 0;
							BotUtils.sysLogAppend("Retrying trough/Compost Bin interaction","white");
							BotUtils.dropItem(0);
							BotUtils.pfRightClick(trough,0);
						}
						BotUtils.sleep(10);
					}
					//BotUtils.waitForWindow("Trough");

				/*	while (BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), invobjs).size() != 0 && !stop) {
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
					}*/

					while(BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), invobjs).size() > 0 && !stop){

						if (stop) {
							System.out.println("stop detected ,breaking");
							return;
						}
						if(BotUtils.getItemAtHand() == null){
							GItem item = BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), invobjs).get(0).item;
							BotUtils.takeItem(item);
							BotUtils.sleep(100);
						}
						if(BotUtils.getItemAtHand() == null){
							System.out.println("Hand null, breaking");
							break;
						}
						List<WItem> list = BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(),invobjs);
						gameui().map.wdgmsg("itemact", Coord.z, trough.rc.floor(posres), 1, 0, (int) trough.id, trough.rc.floor(posres), 0, -1);
						int i = 0;
						while(BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), invobjs).size() == list.size()) {
							if(i > 500){
								System.out.println("Trough appears to be full, breaking.");
								break;
							}
							BotUtils.sleep(10);
							i++;
						}
					}
					BotUtils.sleep(250);
					if(BotUtils.getItemAtHand() != null)
						gameui().map.wdgmsg("itemact", Coord.z, trough.rc.floor(posres), 0, 0, (int) trough.id, trough.rc.floor(posres), 0, -1);
				}catch(Loading | Resource.LoadException | NullPointerException idklol) {
						BotUtils.sysLogAppend("Error captured in main thread.","white");
					}
			}
			BotUtils.sysMsg("Trough Filler finished", Color.WHITE);
			stop = true;
			stop();
		}
	});

	@Override
	public void gobselect(Gob gob) {
		if (gob.getres().basename().contains("trough") || gob.getres().basename().contains("compostbin")) {
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