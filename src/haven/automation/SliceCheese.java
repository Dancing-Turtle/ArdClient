package haven.automation;


import haven.*;
import haven.Window;
import haven.purus.pbot.PBotUtils;
import haven.resutil.WaterTile;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;

public class SliceCheese implements Runnable {
    private GameUI gui;
    private static final int TIMEOUT = 2000;

    public SliceCheese(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        WItem tray = null;
        List<WItem> trays = new ArrayList<>();
        List<WItem> trays2 = new ArrayList<>();
        Window cupboard = null;
        synchronized (gui.ui.root.lchild) {
            try {
                for (Widget q = gui.ui.root.lchild; q != null; q = q.rnext()) {
                    if (q instanceof Inventory) {
                        tray = getTrays2((Inventory) q);
                        if (tray != null) {
                            trays = getTrays((Inventory) q);
                        }
                    }

                }

            } catch (NullPointerException q) {
            }
        }
        //BotUtils.sysMsg("inv found is : "+children.size(),Color.white);
       // BotUtils.sysMsg("trays found is : "+trays.size(),Color.white);

        for (int l = 0 ; l < trays.size(); l++){
            if(trays.get(l).item.getcontents() != null){
                trays2.add(trays.get(l));
            }
        }
        //BotUtils.sysMsg("Number of Cheese trays found is : "+trays2.size(),Color.white);
        gui.error("Number of Cheese trays found is : "+trays2.size());
            for (int i = 0; i < trays2.size(); i++) {
                    if (trays2.get(i).item.getcontents() != null) {
                        FlowerMenu.setNextSelection("Slice up");
                        trays2.get(i).item.wdgmsg("iact", Coord.z, -1);
                        int timeout = 0;
                        while(trays2.get(i).item.getcontents() != null) {
                            timeout++;
                            if(timeout > 500)
                            {
                                PBotUtils.sysMsg("Cheese Slicer interrupted, exited.",Color.white);
                                return;
                            }
                            PBotUtils.sleep(10);
                        }
                    }
            }
        PBotUtils.sysMsg("Done",Color.white);
    }
    private List<WItem> getTrays (Inventory inv){
        List<WItem> trays = inv.getItemsPartial("Cheese Tray");
        // BotUtils.sysMsg("trying to find trays", Color.WHITE);
        if(trays == null)
            return null;
        return trays;
    }

    private WItem getTrays2 (Inventory inv){
        WItem trays = inv.getItemPartialTrays("Tray");
        // BotUtils.sysMsg("trying to find trays", Color.WHITE);
        if(trays == null)
            return null;
        return trays;
    }
}
