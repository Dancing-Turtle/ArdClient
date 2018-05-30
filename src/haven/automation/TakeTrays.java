package haven.automation;


import haven.*;
import haven.Window;
import haven.purus.BotUtils;

import java.awt.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


public class TakeTrays implements Runnable {
    private GameUI gui;
    private UI ui;
    private static final int TIMEOUT = 2000;

    public TakeTrays(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        Thread t = new Thread(new OpenRacks(gui), "OpenRacks");
        t.start();
       // while (t.isAlive()) {
            BotUtils.sleep(400);
        //}
        WItem trays = null;
        WItem belt = null;
        List<WItem> trays2 = new ArrayList<>();
        int i;
        List<Widget> children = new ArrayList<>();
        synchronized (gui.ui.root.lchild) {
            try {
                for (Widget q = gui.ui.root.lchild; q != null; q = q.rnext()) {
                    if (q instanceof Inventory) {
                        trays = getTrays((Inventory) q);
                        belt = getBelt((Inventory) q);
                        if (trays != null && belt == null) {
                            children.add(q);
                            trays2.add(getTrays((Inventory) q));
                        }
                    }

                }

            } catch (NullPointerException q) {
            }
        }
       // BotUtils.sysMsg("size is : "+children.size(),Color.white);
        //BotUtils.sysMsg("trays is : "+trays2.size(),Color.white);

    try {
            for (Widget x : children) {
                trays = getTrays((Inventory) x);
                GItem drinkItem = trays.item;
               // BotUtils.sysMsg("idk"+drinkItem.getcontents().toString(),Color.white);
                drinkItem.wdgmsg("transfer", new Coord(drinkItem.sz.x / 2, drinkItem.sz.y / 2), -1);
                //x.destroy();
               // BotUtils.sleep(100);

            }

    }catch(NullPointerException | IndexOutOfBoundsException m){}


//            for (i = 0; i < 8; i++) {
//                Window racks = gui.getwnd("C. Rack");
//                if (racks == null)
//                    break;
//
//                for (Widget w = racks.lchild; w != null; w = w.prev) {
//                    if (w instanceof Inventory) {
//                        trays = getTrays((Inventory) w);
//                        if(trays == null)
//                            break;
//
//                    }
//                }
//
//                if (trays == null) {
//                    //BotUtils.sysMsg("trays null i is :"+i,Color.white);
//                    return;
//                }
//                GItem drinkItem = trays.item;
//                if (drinkItem != null)
//                {
//                    drinkItem.wdgmsg("transfer", new Coord(drinkItem.sz.x / 2, drinkItem.sz.y / 2), -1);
//                    //racks.wdgmsg("close");
//                    racks.destroy();
//                    BotUtils.sleep(200);
//                }
//
//            }
    }

    private WItem getTrays (Inventory inv){
        WItem trays = inv.getItemPartialTrays("Tray");
        // BotUtils.sysMsg("trying to find trays", Color.WHITE);
        if(trays == null)
            return null;
        return trays;
    }
    private WItem getBelt (Inventory inv){
        WItem trays = inv.getItemPartial("Belt");
        // BotUtils.sysMsg("trying to find trays", Color.WHITE);
        if(trays == null)
            return null;
        return trays;
    }
}
