package haven.automation;


import haven.GameUI;
import haven.Gob;
import haven.Loading;
import haven.Resource;

import static haven.OCache.posres;

public class ClayPicker implements Runnable {
    private GameUI gui;
    private Gob initClay;
    private static final int TIMEOUT_INITIAL = 8000;
    private static final int TIMEOUT = 5000;

    public ClayPicker(GameUI gui, Gob initClay) {
        this.gui = gui;
        this.initClay = initClay;
    }

    @Override
    public void run() {
        long s = System.currentTimeMillis();
        while (!Thread.currentThread().isInterrupted()) {
            try {
                Thread.sleep(500);
                if (gui.map.glob.oc.getgob(initClay.id) == null)
                    break;
                else if (System.currentTimeMillis() - s > TIMEOUT_INITIAL)
                    return;
            } catch (InterruptedException e) {
                return;
            }
        }

        while (!Thread.currentThread().isInterrupted()) {
            Gob closestsClay = null;

            synchronized (gui.map.glob.oc) {
                for (Gob gob : gui.map.glob.oc) {
                    try {
                        Resource res = gob.getres();
                        if (res != null && res.name.equals("gfx/terobjs/herbs/clay-gray")) {
                            if (closestsClay == null || gob.rc.dist(initClay.rc) < closestsClay.rc.dist(initClay.rc))
                                closestsClay = gob;
                        }
                    } catch (Loading l) {
                    }
                }
            }

            if (closestsClay == null || closestsClay.rc.dist(initClay.rc) > 11 * 5)
                return;

            gui.map.wdgmsg("click", closestsClay.sc, closestsClay.rc.floor(posres), 3, 0, 0, (int) closestsClay.id, closestsClay.rc.floor(posres), 0, -1);

            s = System.currentTimeMillis();
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    Thread.sleep(500);
                    if (gui.map.glob.oc.getgob(closestsClay.id) == null)
                        break;
                    else if (System.currentTimeMillis() - s > TIMEOUT)
                        return;
                } catch (InterruptedException e) {
                    return;
                }
            }
        }
    }
}
