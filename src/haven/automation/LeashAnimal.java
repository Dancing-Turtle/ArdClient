package haven.automation;


import static haven.OCache.posres;

import haven.Coord;
import haven.GItem;
import haven.GameUI;
import haven.Gob;
import haven.Loading;
import haven.Resource;
import haven.WItem;

public class LeashAnimal implements Runnable {
    private GameUI gui;
    private Gob animal;
    private static final int TIMEOUT = 2000;
    private static final int HAND_DELAY = 8;

    public LeashAnimal(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        synchronized (gui.map.glob.oc) {
            for (Gob gob : gui.map.glob.oc) {
                Resource res = null;
                try {
                    res = gob.getres();
                } catch (Loading l) {
                }
                if (res != null && (res.name.startsWith("gfx/kritter/horse/") ||
                        res.name.equals("gfx/kritter/sheep/sheep") ||
                        res.name.equals("gfx/kritter/cattle/cattle") ||
                        res.name.equals("gfx/kritter/boar/boar") || 
                        res.name.equals("gfx/kritter/goat/wildgoat"))) {
                    if (animal == null)
                        animal = gob;
                    else if (gob.rc.dist(gui.map.player().rc) < animal.rc.dist(gui.map.player().rc))
                        animal = gob;
                }
            }
        }

        if (animal == null) {
            gui.error("No horse/auroch/mouflon/boar/goat found");
            return;
        }

        WItem ropew = gui.maininv.getItemPartial("Rope");
        if (ropew == null) {
            gui.error("No rope found in the inventory");
            return;
        }
        GItem rope = ropew.item;

        rope.wdgmsg("take", new Coord(rope.sz.x / 2, rope.sz.y / 2));
        int timeout = 0;
        while (gui.hand.isEmpty() || gui.vhand == null) {
            timeout += HAND_DELAY;
            if (timeout >= TIMEOUT) {
                gui.error("No rope found in the inventory");
                return;
            }
            try {
                Thread.sleep(HAND_DELAY);
            } catch (InterruptedException e) {
                return;
            }
        }

        gui.map.wdgmsg("itemact", Coord.z, animal.rc.floor(posres), 0, 0, (int) animal.id, animal.rc.floor(posres), 0, -1);
    }
}
