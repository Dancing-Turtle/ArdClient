package haven.automation;


import haven.*;
import haven.Window;
import haven.purus.BotUtils;
import haven.WItem;
import haven.purus.pbot.PBotAPI;
import haven.resutil.FoodInfo;

import java.awt.*;
import java.util.List;

public class PepperFood implements Runnable {
    private GameUI gui;
    private static final int TIMEOUT = 2000;
    private static int iterations;
    List<WItem> foods;

    public PepperFood(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
 iterations = 0;
        while (iterations < 2) {
            WItem pepper;
            pepper = gui.maininv.getItemPartial("pepper");
            try{
                Window cupboard = gui.getwnd("Cupboard");
                Inventory inv = PBotAPI.getInventory(cupboard);
                foods = getFood(inv);
            }catch(NullPointerException q){BotUtils.sysLogAppend("Null pointer at window grab.","white");}

try {
    pepper.item.wdgmsg("take", Coord.z);
}catch(NullPointerException qq){BotUtils.sysLogAppend("No pepper left.","white");}

            BotUtils.sleep(500);


            try {
                for (int i = 0; i < foods.size(); i++) {
                    for (ItemInfo info : foods.get(i).item.info()) {
                        if (info instanceof FoodInfo) {
                            foods.get(i).item.wdgmsg("itemact", 0);
                        }
                    }
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
            iterations++;
        }
    }
    private List<WItem> getFood (Inventory inv){
        List<WItem> food = inv.getItemsPartial("");
        // BotUtils.sysMsg("trying to find trays", Color.WHITE);
        if(food == null)
            return null;
        return food;
    }
}


