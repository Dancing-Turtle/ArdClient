package haven.automation;


import haven.BeltWnd;
import haven.Config;
import haven.Coord;
import haven.Equipory;
import haven.GItem;
import haven.GameUI;
import haven.Inventory;
import haven.InventoryBelt;
import haven.WItem;
import haven.Widget;
import haven.Window;

public class EquipWeapon implements Runnable {
    private GameUI gui;
    private static final int TIMEOUT = 2000;

    public EquipWeapon(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        WItem weapon = getWeapon(gui.maininv);

        Inventory beltInv = null;
        InventoryBelt quickBeltInv = null;

        if (weapon == null) {
            if (Config.quickbelt) {
                Widget belt = null;
                for (Widget w = gui.lchild; w != null; w = w.prev) {
                    if (w instanceof BeltWnd) {
                        belt = w;
                        break;
                    }
                }

                if (belt == null)
                    return;

                for (Widget w = belt.lchild; w != null; w = w.prev) {
                    if (w instanceof InventoryBelt) {
                        weapon = getWeaponQuickBelt((InventoryBelt) w);
                        quickBeltInv = (InventoryBelt) w;
                        break;
                    }
                }
            } else {
                Window belt = gui.getwnd("Belt");
                if (belt == null)
                    return;

                for (Widget w = belt.lchild; w != null; w = w.prev) {
                    if (w instanceof Inventory) {
                        weapon = getWeapon((Inventory) w);
                        beltInv = (Inventory) w;
                        break;
                    }
                }
            }
        }

        if (weapon == null)
            return;

        GItem weaponItem = weapon.item;

        weaponItem.wdgmsg("take", new Coord(weaponItem.sz.x / 2, weaponItem.sz.y / 2));

        try {
            if (!Utils.waitForOccupiedHand(gui, TIMEOUT, "waitForOccupiedHand timed-out"))
                return;
        } catch (InterruptedException ie) {
            return;
        }

        Equipory e = gui.getequipory();
        if (e == null)
            return;
        e.wdgmsg("drop", 6);

        try {
            if (!Utils.waitForOccupiedHand(gui, TIMEOUT, "waitForOccupiedHand2 timed-out"))
                return;
        } catch (InterruptedException ie) {
            return;
        }

        WItem hand = gui.vhand;
        if (hand != null) {
            if (beltInv != null)
                beltInv.wdgmsg("drop", weaponItem.c.add(Inventory.sqsz.div(2)).div(Inventory.invsq.sz()));
            else if (quickBeltInv != null)
                quickBeltInv.wdgmsg("drop", weapon.c.add(Inventory.sqsz.div(2)).div(Inventory.invsq.sz()));
            else
                gui.maininv.wdgmsg("drop", weapon.c.add(Inventory.sqsz.div(2)).div(Inventory.invsq.sz()));
        }
    }

    private WItem getWeapon(Inventory inv) {
        WItem weapon = inv.getItemPartial("Sword");
        if (weapon == null)
            weapon = inv.getItemPartial("Battleaxe");
        if (weapon == null)
            weapon = inv.getItemPartial("Axe");
        return weapon;
    }

    private WItem getWeaponQuickBelt(InventoryBelt inv) {
        WItem weapon = inv.getItemPartial("Sword");
        if (weapon == null)
            weapon = inv.getItemPartial("Battleaxe");
        if (weapon == null)
            weapon = inv.getItemPartial("Axe");
        return weapon;
    }
}
