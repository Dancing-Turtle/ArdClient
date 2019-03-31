package haven.purus;

import haven.*;
import haven.purus.pbot.PBotGobAPI;

import java.util.Set;
import java.util.TreeSet;

import static haven.OCache.posres;

public class TransferToObject implements Runnable {

	private GameUI gui;
	private boolean shouldStop;

	public TransferToObject(GameUI gui) {
		this.gui = gui;
	}

	@Override
	public void run() {
		Set<Integer> windowsNamesBefore = new TreeSet<>();
		for(Widget w = gui.lchild; w != null; w = w.prev) {
			if(w instanceof Window) {
				windowsNamesBefore.add(w.wdgid());
			}
		}
		Gob gob = getClosestGob();
		if(gob == null)
			return;
		String wndName = PBotGobAPI.gobWindowMap.get(gob.getres().name);
		gui.map.wdgmsg("click", Coord.z, gob.rc.floor(posres), 3, 0, 0, (int) gob.id, gob.rc.floor(posres), 0, -1);
		int retries = 0;
		while(retries < 50) {
			if(shouldStop) {
				return;
			}
			for(Widget w = gui.lchild; w != null; w = w.prev) {
				if(w instanceof Window && !windowsNamesBefore.contains(w.wdgid()) && ((Window) w).cap.text.equals(wndName)) {
					Window wnd = (Window) w;
					for(Widget wdg = wnd.lchild; wdg != null; wdg = wdg.prev) {
						if(wdg instanceof Inventory) {
							Inventory inv = (Inventory) wdg;
							int fs = inv.getFreeSpace();
							for(int i=0; i<fs; i++) {
								gui.maininv.wdgmsg("invxf", inv.wdgid(), 1);
								if(sleep(10))
									return;
							}
						}
					}
					return;
				}
			}
			retries++;
			if(sleep(100))
				return;
			}
	}

	public void stop() {
		shouldStop = true;
	}

	// True if should stop
	private boolean sleep(int timeInMs) {
		try {
			Thread.sleep(timeInMs);
		} catch(InterruptedException ie) {
			return true;
		}
		return false;
	}

	private Gob getClosestGob() {
		Gob player = gui.map.player();
		Gob ret = null;
		double minDist = 110; // Only look for objects not farther than 10 tiles
		synchronized(gui.ui.sess.glob.oc) {
			for(Gob gob:gui.ui.sess.glob.oc) {
				try {
					if(gob == player || gob.getres() == null || !PBotGobAPI.gobWindowMap.containsKey(gob.getres().name))
						continue;
				} catch(Loading l) {

				}
				double dist = gob.rc.dist(player.rc);
				if(dist < minDist) {
					minDist = dist;
					ret = gob;
				}
			}
		}
		return ret;
	}
}
