package haven.automation;


import static haven.OCache.posres;

import haven.CheckListboxItem;
import haven.Config;
import haven.GameUI;
import haven.Gob;
import haven.Loading;
import haven.Resource;
import haven.Utils;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;

public class PickForageable implements Runnable {
    private GameUI gui;
    public static final HashSet<String> gates = new HashSet(Arrays.asList("brickwallgate", "drystonewallgate", "drystonewallbiggate", "palisadegate", "palisadebiggate", "polegate", "polebiggate"));
    public PickForageable(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        Gob herb = null;
        synchronized (gui.map.glob.oc) {
            for (Gob gob : gui.map.glob.oc) {
                Resource res = null;
                boolean gate;
                try {
                    res = gob.getres();
                } catch (Loading l) {
                }
                if (res != null) {
                    CheckListboxItem itm = Config.icons.get(res.basename());
                    Boolean hidden = Boolean.FALSE;
                    gate = gates.contains(res.basename());
                    if (itm == null)
                        hidden = null;
                    else if (itm.selected)
                        hidden = Boolean.TRUE;

                    try {
                        if (gate) {
                            for(Gob.Overlay ol : gob.ols){
                                String resname = (this.gui.map.glob.sess.getres(Utils.uint16d(ol.sdt.rbuf, 0)).get()).basename();
                                if (resname.equals("visflag")) {
                                    gate = false;
                                }
                            }
                        }
                    } catch (NullPointerException fucknulls) { System.out.println("Null on gate open/close"); }

                    if (hidden == null && res.name.startsWith("gfx/terobjs/herbs") || (hidden == Boolean.FALSE && !res.name.startsWith("gfx/terobjs/vehicle")) || gate) {
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
        if (herb.getres().basename().contains("mussel") && Config.autopickmussels)
            gui.map.startMusselsPicker(herb);
        if(herb.getres().basename().contains("clay-gray") && Config.autopickclay)
            gui.map.startClayPicker(herb);
    }
}
