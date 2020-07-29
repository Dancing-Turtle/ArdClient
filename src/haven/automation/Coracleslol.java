package haven.automation;


import haven.Coord;
import haven.GameUI;
import haven.Gob;
import haven.MCache;
import haven.Tiler;
import haven.WItem;
import haven.purus.pbot.PBotUtils;
import haven.resutil.WaterTile;

import java.awt.Color;
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
            } else if (gui.getequipory().quickslots[11] != null) {
                if (gui.getequipory().quickslots[11].item.getname().contains("Coracle")) {
                    coracle = gui.getequipory().quickslots[11];
                    equipcoracle = true;
                    invcoracle = false;
                } else
                    havecoracle = false;
            } else havecoracle = false;

            if (coracle != null)
                havecoracle = true;
            if (!havecoracle) {
                if (invcoracle)
                    while (PBotUtils.findObjectByNames(gui.ui, 10, "gfx/terobjs/vehicle/coracle") == null)
                        PBotUtils.sleep(10);

                coraclegob = PBotUtils.findObjectByNames(gui.ui, 10, "gfx/terobjs/vehicle/coracle");

                if (coraclegob == null) {
                    PBotUtils.sysMsg(gui.ui, "Coracle not found.", Color.white);
                    return;
                } else {
                    //FlowerMenu.setNextSelection("Pick up");
                    PBotUtils.doClick(gui.ui, coraclegob, 3, 1);
                    //       PBotUtils.sleep(250);
                    gui.ui.wdgmsg(gui.ui.next_predicted_id, "cl", 0, 0);
                    List<Coord> slots = PBotUtils.getFreeInvSlots(gui.maininv);
                    for (Coord i : slots) {
                        PBotUtils.dropItemToInventory(i, gui.maininv);
                        PBotUtils.sleep(10);
                    }
                }
            } else {
                Tiler tl = gui.ui.sess.glob.map.tiler(gui.ui.sess.glob.map.gettile_safe(gui.map.player().rc.floor(MCache.tilesz)));
                int timeout = 0;
                while (tl != null && !(tl instanceof WaterTile)) {
                    tl = gui.ui.sess.glob.map.tiler(gui.ui.sess.glob.map.gettile_safe(gui.map.player().rc.floor(MCache.tilesz)));
                    timeout++;
                    if (timeout > 250) {
                        PBotUtils.sysMsg(gui.ui, "Timed out waiting for water tile to drop coracle on.", Color.white);
                        return;
                    }
                    PBotUtils.sleep(10);
                }
                coracle.item.wdgmsg("drop", Coord.z);
                //  ui.gui.map.wdgmsg("gk", 27); //sends escape key to mapview to stop movement so you dont walk away from the coracle
                gui.ui.root.wdgmsg("gk", 27);
                if (invcoracle) {
                    while (gui.maininv.getItemPartial("Coracle") != null)
                        PBotUtils.sleep(10);
                } else if (equipcoracle) {
                    while (gui.getequipory().quickslots[11] != null)
                        PBotUtils.sleep(10);
                } else {
                    PBotUtils.sysMsg(gui.ui, "Somehow I don't know if the coracle came from inv or equipory, breaking.", Color.white);
                    return;
                }
                while (PBotUtils.findObjectByNames(gui.ui, 10, "gfx/terobjs/vehicle/coracle") == null)
                    PBotUtils.sleep(10);
                Gob coraclegob = PBotUtils.findObjectByNames(gui.ui, 10, "gfx/terobjs/vehicle/coracle");
                if (coraclegob == null) {
                    PBotUtils.sysMsg(gui.ui, "Coracle not found, breaking.", Color.white);
                    return;
                } else {//Into the blue yonder!
                    //  FlowerMenu.setNextSelection("Into the blue yonder!");
                    PBotUtils.doClick(gui.ui, coraclegob, 3, 1);
                    gui.ui.wdgmsg(gui.ui.next_predicted_id, "cl", 0, 0);
                    // while (gui.ui.root.findchild(FlowerMenu.class) == null) { }
                }
            }
        } catch (Exception e) {
            PBotUtils.sysMsg(gui.ui, "Prevented crash, something went wrong dropping and mounting coracle.", Color.white);
            e.printStackTrace();
        }
    }
}



