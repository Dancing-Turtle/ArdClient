package haven.automation;


import static haven.OCache.posres;

import haven.CheckListboxItem;
import haven.Config;
import haven.GameUI;
import haven.Gob;
import haven.Loading;
import haven.Resource;

public class PickForageable implements Runnable {
    private GameUI gui;

    public PickForageable(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        Gob herb = null;
        synchronized (gui.map.glob.oc) {
            for (Gob gob : gui.map.glob.oc) {
                Resource res = null;
                try {
                    res = gob.getres();
                } catch (Loading l) {
                }
                if (res != null) {
                    CheckListboxItem itm = Config.icons.get(res.basename());
                    Boolean hidden = Boolean.FALSE;
                    if (itm == null)
                        hidden = null;
                    else if (itm.selected)
                        hidden = Boolean.TRUE;

                    if (hidden == null && res.name.startsWith("gfx/terobjs/herbs") ||
                            hidden == Boolean.FALSE && !res.name.startsWith("gfx/terobjs/vehicle")) {
                        double distFromPlayer = gob.rc.dist(gui.map.player().rc);
                        if (distFromPlayer <= 20 * 11 && (herb == null || distFromPlayer < herb.rc.dist(gui.map.player().rc)))
                            herb = gob;
                    }
                }
            }
        }
        if (herb == null)
            return;

        gui.map.wdgmsg("click", herb.sc, herb.rc.floor(posres), 3, 0, 0, (int) herb.id, herb.rc.floor(posres), 0, -1);
        if (Config.autopickmussels)
            gui.map.startMusselsPicker(herb);
    }
}
