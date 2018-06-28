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
            try{
                Window cupboard = gui.getwnd("Cupboard");
                Inventory inv = PBotAPI.getInventory(cupboard);
                foods = getFood(inv);
            }catch(NullPointerException q){BotUtils.sysLogAppend("Null pointer at window grab.","white");}

            BotUtils.sleep(500);

                Equipory f = gui.getequipory();
                WItem l = f.quickslots[6];
                WItem r = f.quickslots[7];

                boolean nolbucket = true;
                boolean norbucket = true;

                if (l != null) {
                    String lname = l.item.getname();
                    if (lname.contains("Bucket"))
                        nolbucket = false;
                }
                if (r != null) {
                    String rname = r.item.getname();
                    if (rname.contains("Bucket"))
                        norbucket = false;
                }

                if (!nolbucket || !norbucket) {
                    WItem x = f.quickslots[nolbucket ? 7 : 6];
                    x.mousedown(new Coord(x.sz.x / 2, x.sz.y / 2), 1);
                    while(BotUtils.getItemAtHand() == null)
                        BotUtils.sleep(10);

                    try {
                        Thread.sleep(100);
                    } catch (InterruptedException ie) {
                        f.wdgmsg("drop", nolbucket ? 7 : 6);
                        return;
                    }
                    for (WItem fooditem : foods){
                        GItem fooditemlol = fooditem.item;
                        fooditemlol.wdgmsg("itemact",0);
                    }
                    f.wdgmsg("drop", nolbucket ? 7 : 6);
                }
                BotUtils.sysMsg("Food found and Peppered : "+foods.size(),Color.white);
            }
    private List<WItem> getFood (Inventory inv){
        List<WItem> food = inv.getItemsPartial("");
        // BotUtils.sysMsg("trying to find trays", Color.WHITE);
        if(food == null)
            return null;
        return food;
    }
}


