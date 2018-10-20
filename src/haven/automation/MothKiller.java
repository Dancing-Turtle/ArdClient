package haven.automation;


import haven.*;
import haven.Window;
import haven.purus.BotUtils;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;

public class MothKiller implements Runnable {
    private GameUI gui;

    public MothKiller(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        WItem moth = null;
        List<WItem> moths = new ArrayList<>();
        List<Widget> children = new ArrayList<>();
        synchronized (gui.ui.root.lchild) {
            try {
                for (Widget q = gui.ui.root.lchild; q != null; q = q.rnext()) {
                    if (q instanceof Inventory) {
                        moth = getMoths2((Inventory) q);
                        if (moth != null) {
                            children.add(q);
                            moths = getMoths((Inventory) q);
                        }
                    }

                }

            } catch (NullPointerException q) {
            }
        }

             //  trays2.addAll(trays);
        if(moths!=null)
        if(moths.size() > 0)
        BotUtils.sysMsg("Found "+moths.size()+" to kill.",Color.white);
        for (WItem item : moths) {
                FlowerMenu.setNextSelection("Kill");
                item.item.wdgmsg("iact", Coord.z, -1);
                BotUtils.sleep(1000);
        }
        BotUtils.sysMsg("Done",Color.white);
    }
    private List<WItem> getMoths (Inventory inv){
        List<WItem> moths = inv.getItemsPartial("Cocoon");
        // BotUtils.sysMsg("trying to find trays", Color.WHITE);
        if(moths == null)
            return null;
        return moths;
    }

    private WItem getMoths2 (Inventory inv){
        WItem moths = inv.getItemPartialTrays("Cocoon");
        // BotUtils.sysMsg("trying to find trays", Color.WHITE);
        if(moths == null)
            return null;
        return moths;
    }
}

