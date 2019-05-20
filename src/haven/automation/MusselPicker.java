package haven.automation;


import static haven.OCache.posres;

import haven.GameUI;
import haven.Gob;
import haven.Loading;
import haven.Resource;

public class MusselPicker implements Runnable {
    private GameUI gui;
    private Gob initMussel;
    private static final int TIMEOUT_INITIAL = 8000;
    private static final int TIMEOUT = 5000;

    public MusselPicker(GameUI gui, Gob initMussel) {
        this.gui = gui;
        this.initMussel = initMussel;
    }

    @Override
    public void run() {
        long s = System.currentTimeMillis();
        while (!Thread.currentThread().isInterrupted()) {
            try {
                Thread.sleep(500);
                if (gui.map.glob.oc.getgob(initMussel.id) == null)
                    break;
                else if (System.currentTimeMillis() - s > TIMEOUT_INITIAL)
                    return;
            } catch (InterruptedException e) {
                return;
            }
        }

        while (!Thread.currentThread().isInterrupted()) {
            Gob closestsMussel = null;

            synchronized (gui.map.glob.oc) {
                for (Gob gob : gui.map.glob.oc) {
                    try {
                        Resource res = gob.getres();
                        if (res != null && (res.name.equals("gfx/terobjs/herbs/mussels") ||
                                res.name.equals("gfx/terobjs/herbs/clay-gray") ||
                                res.name.equals("gfx/terobjs/herbs/oyster") ||
                                res.name.equals("gfx/terobjs/herbs/goosebarnacle") ||
                                res.name.equals("gfx/terobjs/herbs/cattail") ||
                                res.name.equals("gfx/kritter/jellyfish/jellyfish") ||
                                res.name.equals("gfx/terobjs/herbs/lampstalk"))) {
                            if (closestsMussel == null || gob.rc.dist(initMussel.rc) < closestsMussel.rc.dist(initMussel.rc))
                                closestsMussel = gob;
                        }
                    } catch (Loading l) {
                    }
                }
            }

            if (closestsMussel == null || closestsMussel.rc.dist(initMussel.rc) > 11 * 5)
                return;

            gui.map.wdgmsg("click", closestsMussel.sc, closestsMussel.rc.floor(posres), 3, 0, 0, (int) closestsMussel.id, closestsMussel.rc.floor(posres), 0, -1);

            s = System.currentTimeMillis();
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    Thread.sleep(500);
                    if (gui.map.glob.oc.getgob(closestsMussel.id) == null)
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
