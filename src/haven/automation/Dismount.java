package haven.automation;


import static haven.OCache.posres;

import haven.Coord2d;
import haven.FlowerMenu;
import haven.GameUI;
import haven.Gob;
import haven.Loading;
import haven.Resource;

public class Dismount implements Runnable {
    private GameUI gui;

    public Dismount(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        Gob animal = null;
        synchronized (gui.map.glob.oc) {
            for (Gob gob : gui.map.glob.oc) {
                try {
                    Resource res = gob.getres();
                    if (res != null && (res.name.startsWith("gfx/kritter/horse"))) {
                        Coord2d plc = gui.map.player().rc;
                        if ((animal == null || gob.rc.dist(plc) < animal.rc.dist(plc)) && !gob.isDead())
                            animal = gob;
                    }
                } catch (Loading l) {
                }
            }
        }

        if (animal == null)
            return;

        FlowerMenu.setNextSelection("Dismount rider");
        gui.map.wdgmsg("click", animal.sc, animal.rc.floor(posres), 3, 0, 0, (int) animal.id, animal.rc.floor(posres), 0, -1);
    }
}
