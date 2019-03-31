package haven.purus;

import static haven.OCache.posres;

import java.awt.Color;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import haven.*;
import haven.automation.GobSelectCallback;
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;

public class TroughFiller extends Window implements GobSelectCallback {

	private Gob trough;

	private List<String> invobjs = Arrays.asList("gfx/invobjs/carrot", "gfx/invobjs/beet", "gfx/invobjs/beetleaves", "gfx/invobjs/pumpkin","gfx/invobjs/turnip");
	private String[] terobjs = { "gfx/terobjs/items/carrot", "gfx/terobjs/items/beet", "gfx/terobjs/items/beetleaves","gfx/terobjs/items/pumpkin","gfx/terobjs/items/turnip" };


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
					while(PBotUtils.findObjectByNames(5000,terobjs) == null) {
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
						PBotUtils.sleep(50);
					}
					if (PBotUtils.findObjectByNames(5000, terobjs) == null && PBotUtils.getInventoryItemsByNames(PBotAPI.gui.maininv, invobjs).size() == 0) {
						System.out.println("No objects on ground or in inv, breaking.");
						break;
					}
					while(PBotUtils.getItemAtHand() == null){
						System.out.println("Waiting for item on cursor");
						if (stop) {
							System.out.println("Stop detected, breaking.");
							stop = true;
							break main;
						}


						if (PBotUtils.findObjectByNames(5000, terobjs) == null) {
							System.out.println("No objects on ground, breaking.");
							break;
						}

						Gob g = PBotUtils.findObjectByNames(5000, terobjs);
						gameui().map.wdgmsg("click", g.sc, g.rc.floor(posres),3,1,0,(int)g.id,g.rc.floor(posres),0,-1);
						PBotUtils.sleep(1000);
						while(PBotUtils.getItemAtHand() == null & PBotUtils.findObjectByNames(5000,terobjs)!=null && PBotUtils.isMoving())
							PBotUtils.sleep(10);
					}

					if (stop) {
						System.out.println("stop detected ,breaking");
						return;
					}

						PBotUtils.sleep(500);

					if (PBotUtils.getItemAtHand() != null)
						PBotUtils.dropItem(0);

						PBotUtils.pfRightClick(trough, 0);
					int retry = 0;
					while(gameui().getwnd("Trough") == null && gameui().getwnd("Compost Bin") == null){
						retry++;
						if(retry > 500){
							retry = 0;
							PBotUtils.sysLogAppend("Retrying trough/Compost Bin interaction","white");
							PBotUtils.dropItem(0);
							PBotUtils.pfRightClick(trough,0);
						}
						PBotUtils.sleep(10);
					}
					while(PBotUtils.getInventoryItemsByNames(PBotAPI.gui.maininv, invobjs).size() > 0 && !stop){

						if (stop) {
							System.out.println("stop detected ,breaking");
							return;
						}
						if(PBotUtils.getItemAtHand() == null){
							GItem item = PBotUtils.getInventoryItemsByNames(PBotAPI.gui.maininv, invobjs).get(0).item;
							PBotUtils.takeItem(item);
							PBotUtils.sleep(100);
						}
						if(PBotUtils.getItemAtHand() == null){
							System.out.println("Hand null, breaking");
							break;
						}
						List<WItem> list = PBotUtils.getInventoryItemsByNames(PBotAPI.gui.maininv,invobjs);
						gameui().map.wdgmsg("itemact", Coord.z, trough.rc.floor(posres), 1, 0, (int) trough.id, trough.rc.floor(posres), 0, -1);
						int i = 0;
						while(PBotUtils.getInventoryItemsByNames(PBotAPI.gui.maininv, invobjs).size() == list.size()) {
							if(i > 500){
								System.out.println("Trough appears to be full, breaking.");
								break;
							}
							PBotUtils.sleep(10);
							i++;
						}
					}
						PBotUtils.sleep(250);
					if(PBotUtils.getItemAtHand() != null)
						gameui().map.wdgmsg("itemact", Coord.z, trough.rc.floor(posres), 0, 0, (int) trough.id, trough.rc.floor(posres), 0, -1);
				}catch(Loading | Resource.LoadException | NullPointerException idklol) {
						PBotUtils.sysLogAppend("Error captured in main thread.","white");
					}
			}
			PBotUtils.sysMsg("Trough Filler finished", Color.WHITE);
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
		PBotUtils.sysMsg("Stopping Trough Filler",Color.white);
		this.destroy();
	}
}