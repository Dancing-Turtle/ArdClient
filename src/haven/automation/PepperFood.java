package haven.automation;


import haven.*;
import haven.Window;
import haven.purus.BotUtils;
import haven.WItem;
import haven.resutil.FoodInfo;

import java.awt.*;
import java.util.List;

public class PepperFood implements Runnable {
    private GameUI gui;
    private static final int TIMEOUT = 2000;

    public PepperFood(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        List<WItem> pepper;
        WItem tray2 = null;
        pepper = gui.maininv.getItemsPartial("pepper");
        List<WItem> tray = null;
        List<WItem> food = null;
        int pepperiterator = 0;
        for (Widget w = gui.lchild; w != null && tray == null; w = w.prev) {
            if (!(w instanceof Window))
                continue;
            for (Widget inv = w.lchild; inv != null; inv = inv.prev) {
                if (!(inv instanceof Inventory))
                    continue;
                tray = gui.maininv.getItemsPartial("");
                if (tray != null)
                    break;
            }
        }


    pepper.get(0).item.wdgmsg("take", Coord.z);


        BotUtils.sleep(500);


        try {
            for (int i = 0; i < tray.size(); i++) {
                //  gui.syslog.append("looping",Color.white);
                // tray.get(i).item.wdgmsg("itemact",0);
                if (BotUtils.getItemAtHand() == null) {
                    BotUtils.sysMsg("Out of pepper, getting next", Color.white);
                    pepperiterator = pepperiterator +1;
                    pepper.get(pepperiterator).item.wdgmsg("take", Coord.z);
                    BotUtils.sleep(500);
                    if (BotUtils.getItemAtHand() == null){
                        BotUtils.sysMsg("Fully out of pepper",Color.white);
                        break;
                    }
                }
                for (ItemInfo info : tray.get(i).item.info()) {
                    if (info instanceof FoodInfo) {
                        tray.get(i).item.wdgmsg("itemact", 0);
                    }
                }


                // BotUtils.sysMsg("test : "+tray.get(i).item.info().get(5).toString(),Color.white);
                // for (ItemInfo info : tray.get(i).item.info()) {
                //if (info instanceof ItemInfo.AdHoc) {
                //if (((ItemInfo.AdHoc) info).str.text != "Peppered") {
                // tray.get(i).wdgmsg("iact",-1);
                // }


                //}
            }

            //}
        } catch (NullPointerException | IndexOutOfBoundsException q) {
        }


        if (BotUtils.getItemAtHand() != null) {
            Coord slot = BotUtils.getFreeInvSlot(BotUtils.playerInventory());
            if (slot != null) {
                int freeSlots = BotUtils.invFreeSlots();
                BotUtils.dropItemToInventory(slot, BotUtils.playerInventory());
                while (BotUtils.getItemAtHand() != null)
                    BotUtils.sleep(50);
            }
        }
    }
    }

