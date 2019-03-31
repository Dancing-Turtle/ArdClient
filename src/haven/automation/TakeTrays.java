package haven.automation;


import haven.*;
import haven.Window;
import haven.purus.pbot.PBotUtils;

import java.awt.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


public class TakeTrays implements Runnable {
    private GameUI gui;
    public TakeTrays(GameUI gui) {
        this.gui = gui;
    }
    @Override
    public void run() {
        Thread t = new Thread(new OpenRacks(gui), "OpenRacks");
        t.start();
       // while (t.isAlive()) {
        PBotUtils.sleep(500);
       // }
        List<WItem> trays;
        synchronized (gui.ui.root.lchild) {
            try {
                for (Widget q = gui.ui.root.lchild; q != null; q = q.rnext()) {
                    if (q instanceof Inventory && q != gui.maininv) {
                        trays = (((Inventory) q).getItemsPartial("Tray"));
                        System.out.println("trays2 size : "+trays.size());
                        if (trays.size()>0) {
                            for(WItem item : trays)
                                item.item.wdgmsg("transfer", new Coord(item.item.sz.x / 2, item.item.sz.y / 2), -1);
                            trays.clear();
                        }
                    }

                }
            } catch (NullPointerException q) {
            }
        }
    }
}
