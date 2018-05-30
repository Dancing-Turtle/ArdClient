package haven.automation;


import haven.Coord;
import haven.GameUI;
import haven.Inventory;
import haven.WItem;
import haven.Widget;
import haven.Window;

public class FillCheeseTray implements Runnable {
    private GameUI gui;
    private static final int TIMEOUT = 2000;

    public FillCheeseTray(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        WItem curd;
        while ((curd = Utils.findItemByPrefixInInv(gui.maininv, "gfx/invobjs/curd")) != null) {
            WItem tray = null;
            for (Widget w = gui.lchild; w != null && tray == null; w = w.prev) {
                if (!(w instanceof Window))
                    continue;
                for (Widget inv = w.lchild; inv != null; inv = inv.prev) {
                    if (!(inv instanceof Inventory))
                        continue;
                    tray = Utils.findItemInInv((Inventory) inv, "gfx/invobjs/cheesetray");
                    if (tray != null)
                        break;
                }
            }

            if (tray == null)
                return;

            curd.item.wdgmsg("take", Coord.z);

            try {
                if (!Utils.waitForOccupiedHand(gui, TIMEOUT, "waitForOccupiedHand timed-out"))
                    return;

                tray.item.wdgmsg("itemact", 0);

                if (!Utils.waitForEmptyHand(gui, TIMEOUT, "waitForEmptyHand timed-out"))
                    return;
            } catch (InterruptedException e) {
                return;
            }
        }
    }
}
