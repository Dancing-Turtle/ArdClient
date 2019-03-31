package haven.automation;


import haven.*;
import haven.Window;
import haven.purus.pbot.PBotUtils;

import java.awt.*;
import java.util.List;


public class PlaceTrays implements Runnable {
    private GameUI gui;
    private static final int TIMEOUT = 2000;

    public PlaceTrays(GameUI gui) {
        this.gui = gui;
    }
    @Override
    public void run() {
        Thread t = new Thread(new OpenRacks(gui), "OpenRacks");
        t.start();
        while (t.isAlive()) {
            PBotUtils.sleep(10);
        }

        WItem trays = null;
            try {
                if ((trays = Utils.findItemByPrefixInInv(gui.maininv, "gfx/invobjs/cheesetray")) != null) {
                    for (Widget w = gui.lchild; w != null; w = w.prev) {
                        if (!(w instanceof Window))
                            continue;
                        for (Widget wdg = w.lchild; wdg != null; wdg = wdg.prev) {
                            if (!(wdg instanceof Inventory))
                                continue;
                            if (trays == null)
                                break;
                        }
                    }
                    List<WItem> items = gui.maininv.getIdenticalItems(trays.item);
                    for (WItem item : items)
                        item.item.wdgmsg("transfer", Coord.z);
                    // trays.item.wdgmsg("transfer", Coord.z);
                }


            } catch (NullPointerException fa) {
            }

        }


    }

