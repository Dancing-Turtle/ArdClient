package haven.automation;


import haven.*;
import haven.Window;
import haven.purus.pbot.PBotUtils;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;

public class SplitLogs implements Runnable {
    private GameUI gui;
    private static final int TIMEOUT = 2000;

    public SplitLogs(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        WItem tray = null;
        List<WItem> trays = new ArrayList<>();
        List<WItem> trays2 = new ArrayList<>();
        List<Widget> children = new ArrayList<>();
        Window cupboard = null;
        synchronized (gui.ui.root.lchild) {
            try {
                for (Widget q = gui.ui.root.lchild; q != null; q = q.rnext()) {
                    if (q instanceof Inventory) {
                        tray = getTrays2((Inventory) q);
                        if (tray != null) {
                            children.add(q);
                            trays = getTrays((Inventory) q);
                        }
                    }

                }

            } catch (NullPointerException q) {
            }
        }

             //  trays2.addAll(trays);


        for (int i = 0; i < trays.size(); i++) {
                FlowerMenu.setNextSelection("Split");
                trays.get(i).item.wdgmsg("iact", Coord.z, -1);
            PBotUtils.sleep(900);
        }
        PBotUtils.sysMsg("Done",Color.white);
    }
    private List<WItem> getTrays (Inventory inv){
        List<WItem> trays = inv.getItemsPartial("Block");
        // BotUtils.sysMsg("trying to find trays", Color.WHITE);
        if(trays == null)
            return null;
        return trays;
    }

    private WItem getTrays2 (Inventory inv){
        WItem trays = inv.getItemPartialTrays("Block");
        // BotUtils.sysMsg("trying to find trays", Color.WHITE);
        if(trays == null)
            return null;
        return trays;
    }
}

