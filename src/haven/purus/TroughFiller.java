package haven.purus;

import static haven.OCache.posres;

import java.awt.Color;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import haven.Coord;
import haven.Frame;
import haven.GItem;
import haven.GOut;
import haven.Gob;
import haven.Inventory;
import haven.Label;
import haven.UI;
import haven.Widget;
import haven.Window;
import haven.automation.GobSelectCallback;

public class TroughFiller extends Window implements GobSelectCallback {

	private Gob trough;

	private List<String> invobjs = Arrays.asList("gfx/invobjs/carrot", "gfx/invobjs/beet", "gfx/invobjs/beetleaves");
	private String[] terobjs = { "gfx/terobjs/items/carrot", "gfx/terobjs/items/beet", "gfx/terobjs/items/beetleaves" };

	private boolean stop = false;

	public TroughFiller() {
		super(new Coord(270, 50), "Trough Filler");

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
	}

	Thread t = new Thread(new Runnable() {
		public void run() {
			main: while (true) {
				if (BotUtils.findObjectByNames(1000, terobjs) == null
						&& BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), invobjs).size() == 0)
					break;
				while (BotUtils.invFreeSlots() > 0) {
					if (stop)
						break main;
					if (BotUtils.findObjectByNames(1000, terobjs) == null)
						break;

					Gob g = BotUtils.findObjectByNames(1000, terobjs);
					BotUtils.pfRightClick(g, 0);
					int i = 0;
					while (BotUtils.findObjectById(g.id) != null) {
						if (i == 100)
							break;
						BotUtils.sleep(100);
						i++;
					}
				}

				if (stop)
					break main;
				if (BotUtils.getItemAtHand() != null)
					BotUtils.dropItem(0);
				BotUtils.pfRightClick(trough, 0);
				BotUtils.waitForWindow("Trough");

				while (BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), invobjs).size() != 0) {
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
		stop = true;
		reqdestroy();
		gameui().map.wdgmsg("click", Coord.z, new Coord((int)BotUtils.player().rc.x, (int)BotUtils.player().rc.y), 1, 0);
	}
}