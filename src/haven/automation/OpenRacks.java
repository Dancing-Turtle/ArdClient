package haven.automation;


import haven.*;

import static haven.OCache.posres;

import haven.Window;
import haven.purus.BotUtils;
import haven.purus.pbot.PBotAPI;
import haven.res.ui.tt.q.qbuff.QBuff;
import haven.Inventory;

import java.awt.*;
import java.util.*;
import java.util.List;

public class OpenRacks implements Runnable {
    private GameUI gui;
    private Gob rack;
    private int count;
    private static final int TIMEOUT = 2000;
    private static final int HAND_DELAY = 8;
    private Inventory inv;
    //private Gob Racks[] = new Gob[7];


    public OpenRacks(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {

        List<Gob> Racks = new ArrayList<>();
        List<Gob> allGobs = new ArrayList<Gob>();
        synchronized (gui.ui.sess.glob.oc) {
            for (Gob gob : gui.ui.sess.glob.oc)
                allGobs.add(gob);

                try {
                    for (int i = 0; i < allGobs.size(); i++) {
                        double distFromPlayer = allGobs.get(i).rc.dist(gui.map.player().rc);
                        if (distFromPlayer < 14 && allGobs.get(i).getres().name.contains("cheeserack")) {
                            Racks.add(allGobs.get(i));
                        }
                    }

                } catch (NullPointerException e) {
                }

        }

if (Racks.size() == 0){
            return;
}
        try {
            for (int i = 0; i < 7; i++) {
                gui.map.wdgmsg("click", Coord.z, Racks.get(i).rc.floor(posres), 3, 0, 0, (int) Racks.get(i).id, Racks.get(i).rc.floor(posres), 0, -1);// timeout = 0;
                //gui.map.wdgmsg("click", herb.sc, herb.rc.floor(posres), 3, 0, 0, (int) herb.id, herb.rc.floor(posres), 0, -1);
               // BotUtils.sleep(120);
            }
        } catch (IndexOutOfBoundsException f) {
        }

        // List<WItem> items = items(Inventory inv)
        // for (WItem item : items)
        // item.item.wdgmsg("transfer", Coord.z);
Racks = null;
        allGobs = null;

    }
}










