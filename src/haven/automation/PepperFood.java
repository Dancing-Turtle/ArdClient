package haven.automation;


import haven.*;
import haven.Window;
import haven.WItem;
import haven.purus.pbot.PBotUtils;
import haven.resutil.FoodInfo;

import java.awt.*;
import java.util.List;

public class PepperFood implements Runnable {
    private GameUI gui;
    private static final int TIMEOUT = 2000;
    private static int iterations;


    public PepperFood(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
                Equipory f = gui.getequipory();
                WItem l = f.quickslots[6];
                WItem r = f.quickslots[7];

                boolean nolbucket = true;
                boolean norbucket = true;
                List<WItem> foods = null;

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
                    try{
                    WItem x = f.quickslots[nolbucket ? 7 : 6];
                    x.mousedown(new Coord(x.sz.x / 2, x.sz.y / 2), 1);
                }catch(NullPointerException lo){}
                    while(PBotUtils.getItemAtHand() == null)
                        PBotUtils.sleep(10);
                    try {
                        Thread.sleep(100);
                    } catch (InterruptedException ie) {
                        f.wdgmsg("drop", nolbucket ? 7 : 6);
                        return;
                    }
                }
                    synchronized (gui.ui.root.lchild) {
                            for (Widget q = gui.ui.root.lchild; q != null; q = q.rnext()) {
                                if (q instanceof Inventory) {
                                    List<WItem> invfoods = getFood((Inventory) q);
                                    for (WItem fooditem : invfoods){
                                        GItem fooditemlol = fooditem.item;
                                        fooditemlol.wdgmsg("itemact",0);
                                    }
                                    invfoods.clear();
                                }

                            }
                    }
                    f.wdgmsg("drop", nolbucket ? 7 : 6);
                if(nolbucket && norbucket) {
                    PBotUtils.sysMsg("No equipped Bucket Found", Color.white);
                }
                else
                    PBotUtils.sysMsg("Done",Color.white);
            }

    private List<WItem> getFood (Inventory inv){
        List<WItem> food = inv.getItemsPartial("");
       // BotUtils.sysMsg("Food size : "+food.size(),Color.white);
        // BotUtils.sysMsg("trying to find trays", Color.WHITE);
        if(food == null)
            return null;
        return food;
    }
}


