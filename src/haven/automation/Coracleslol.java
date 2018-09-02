package haven.automation;


import haven.*;
import haven.purus.BotUtils;

import java.awt.*;
import java.util.List;

public class Coracleslol implements Runnable {
    public GameUI gui;
    public boolean havecoracle;
    public WItem coracle;
    public Gob coraclegob;
    public Coracleslol(GameUI gui) {
        this.gui = gui;
    }

    public void run() {
        try {
            coracle = gui.maininv.getItemPartial("Coracle");
        }catch(NullPointerException e){
            havecoracle = false;
        }
        if(coracle != null)
            havecoracle = true;
        if(!havecoracle){
            try {
                coraclegob = BotUtils.findObjectByNames(10, "gfx/terobjs/vehicle/coracle");
            }catch(NullPointerException ip){}
            if (coraclegob == null){
                BotUtils.sysMsg("Coracle not found.",Color.white);
                return;
            }
            else{
                FlowerMenu.setNextSelection("Pick up");
                BotUtils.doClick(coraclegob,3,1);
                BotUtils.sleep(250);
                  // coracle = gui.maininv.getItemPartial("Coracle");
                   List<Coord> slots = BotUtils.getFreeInvSlots(BotUtils.playerInventory());
                        for (Coord i : slots) {
                            BotUtils.dropItemToInventory(i, BotUtils.playerInventory());
                        }

            }
        }
        else {
            coracle.item.wdgmsg("drop", Coord.z);
            while(gui.maininv.getItemPartial("Coracle")!=null){}
            Gob coraclegob = BotUtils.findObjectByNames(10, "gfx/terobjs/vehicle/coracle");
            if (coraclegob == null) {
                BotUtils.sysMsg("Coracle not found, breaking.", Color.white);
                return;
            } else {//Into the blue yonder!
                FlowerMenu.setNextSelection("Into the blue yonder!");
                BotUtils.doClick(coraclegob, 3, 1);
                while (gui.ui.root.findchild(FlowerMenu.class) == null) {}
            }
        }
    }
}


