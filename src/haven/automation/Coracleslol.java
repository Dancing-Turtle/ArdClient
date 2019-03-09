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
    public Boolean invcoracle = false, equipcoracle = false;

    public Coracleslol(GameUI gui) {
        this.gui = gui;
    }

    public void run() {
        try {
            if (gui.maininv.getItemPartial("Coracle") != null) {
                coracle = gui.maininv.getItemPartial("Coracle");
                invcoracle = true;
                equipcoracle = false;
            }
            else if (gui.getequipory().quickslots[11] != null) {
                if (gui.getequipory().quickslots[11].item.getname().contains("Coracle")) {
                    coracle = gui.getequipory().quickslots[11];
                    equipcoracle = true;
                    invcoracle = false;
                }
                else
                    havecoracle = false;
            } else havecoracle = false;
        } catch (NullPointerException q) {
            BotUtils.sysMsg("Prevented null pointer crash, something went wrong finding coracle.", Color.white);
        }
            if (coracle != null)
                havecoracle = true;
            if (!havecoracle) {
                try {
                    if(invcoracle)
                    while (BotUtils.findObjectByNames(10, "gfx/terobjs/vehicle/coracle") == null)
                        BotUtils.sleep(10);
                    coraclegob = BotUtils.findObjectByNames(10, "gfx/terobjs/vehicle/coracle");

                    if (coraclegob == null) {
                        BotUtils.sysMsg("Coracle not found.", Color.white);
                        return;
                    } else {
                        FlowerMenu.setNextSelection("Pick up");
                        BotUtils.doClick(coraclegob, 3, 1);
                        BotUtils.sleep(250);
                        // coracle = gui.maininv.getItemPartial("Coracle");
                        List<Coord> slots = BotUtils.getFreeInvSlots(BotUtils.playerInventory());
                        for (Coord i : slots) {
                            BotUtils.dropItemToInventory(i, BotUtils.playerInventory());
                            BotUtils.sleep(10);
                        }
                    }
                } catch (NullPointerException ip) {
                    BotUtils.sysMsg("Prevented null pointer crash, something went wrong finding coracle to pick up.", Color.white);
                }
            } else {
                try {
                    coracle.item.wdgmsg("drop", Coord.z);
                    if(invcoracle) {
                        while (gui.maininv.getItemPartial("Coracle") != null)
                            BotUtils.sleep(10);
                    }else if(equipcoracle) {
                        while (gui.getequipory().quickslots[11] != null)
                            BotUtils.sleep(10);
                    }else{
                        BotUtils.sysMsg("Somehow I don't know if the coracle came from inv or equipory, breaking.",Color.white);
                        return;
                    }
                    while (BotUtils.findObjectByNames(10, "gfx/terobjs/vehicle/coracle") == null)
                        BotUtils.sleep(10);
                    Gob coraclegob = BotUtils.findObjectByNames(10, "gfx/terobjs/vehicle/coracle");
                    if (coraclegob == null) {
                        BotUtils.sysMsg("Coracle not found, breaking.", Color.white);
                        return;
                    } else {//Into the blue yonder!
                        FlowerMenu.setNextSelection("Into the blue yonder!");
                        BotUtils.doClick(coraclegob, 3, 1);
                        while (gui.ui.root.findchild(FlowerMenu.class) == null) {
                        }
                    }
                } catch (NullPointerException qqq) {
                    BotUtils.sysMsg("Prevented null pointer crash, something went wrong dropping and mounting coracle.", Color.white);
                }
            }
        }
    }



