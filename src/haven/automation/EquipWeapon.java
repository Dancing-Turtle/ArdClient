package haven.automation;


import haven.*;
import haven.Window;
import haven.purus.BotUtils;

import java.awt.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;

public class EquipWeapon implements Runnable {
    private GameUI gui;
    private static final int TIMEOUT = 2000;

    public EquipWeapon(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        try{//start giant try catch to prevent any loading/null crashes when using this.
        Inventory beltInv = null;
        InventoryBelt quickBeltInv = null;
        WItem lhand = gui.getequipory().quickslots[6];
        WItem rhand = gui.getequipory().quickslots[7];
        HashMap<WItem, Integer> wepmap = new HashMap<>();
        wepmap.putAll(getWeapon(gui.maininv));

        if (lhand != null) {
            if (lhand.name.get().contains("Sword") || lhand.name.get().contains("Battleaxe")) {
                BotUtils.sysMsg("Already found weapon in left hand, canceling.", Color.white);
                return;
            }
        }

        if (rhand != null) {
            if (rhand.name.get().contains("Sword") || rhand.name.get().contains("Battleaxe")) {
                BotUtils.sysMsg("Already found weapon in right hand, canceling.", Color.white);
                return;
            }
        }

        if (Config.quickbelt) {
            Widget belt = null;
            for (Widget w = gui.lchild; w != null; w = w.prev) {
                if (w instanceof AltBeltWnd) {
                    belt = w;
                    break;
                }
            }

            if (belt == null)
                return;

            for (Widget w = belt.lchild; w != null; w = w.prev) {
                if (w instanceof InventoryBelt) {
                    wepmap.putAll(getWeaponQuickBelt((InventoryBelt) w));
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
                    wepmap.putAll(getWeapon((Inventory) w));
                    beltInv = (Inventory) w;
                    break;
                }
            }
        }


        if (wepmap.size() == 0) {
            BotUtils.sysMsg("No weapons found",Color.white);
            return;
        }

        GItem weaponItem;

        List<WItem> weapons = new ArrayList<>(wepmap.keySet());
        Collections.sort(weapons, new Comparator<WItem>() {
            @Override
            public int compare(WItem s1, WItem s2) {
                Integer popularity1 = wepmap.get(s1);
                Integer popularity2 = wepmap.get(s2);
                return popularity1.compareTo(popularity2);
            }
        });
        weaponItem = weapons.get(weapons.size() - 1).item;


        weaponItem.wdgmsg("take", new Coord(weaponItem.sz.x / 2, weaponItem.sz.y / 2));

        try {
            if (!Utils.waitForOccupiedHand(gui, TIMEOUT, "waitForOccupiedHand timed-out"))
                return;
        } catch (InterruptedException ie) {
            return;
        }

        Equipory e = gui.getequipory();
            if (e == null)//equipory is somehow null, break
                return;

        if(lhand == null) //try to find an empty hand first, otherwise drop it in left hand
            e.wdgmsg("drop", 6);
        else if(rhand == null)
            e.wdgmsg("drop", 7);
        else
            e.wdgmsg("drop", 6);

        try {
            if (!Utils.waitForOccupiedHand(gui, TIMEOUT, "waitForOccupiedHand2 timed-out"))
                return;
        } catch (InterruptedException ie) {
            return;
        }

        WItem hand = gui.vhand;
        if (hand != null) { //try to empty hand into belt
            if (beltInv != null) {
                List<Coord> slots = BotUtils.getFreeInvSlots(beltInv);
                for (Coord i : slots) {
                    BotUtils.dropItemToInventory(i, beltInv);
                    BotUtils.sleep(10);
                }
            } else if (quickBeltInv != null) {
                List<Coord> slots = BotUtils.getFreeInvSlotsAlt(quickBeltInv);
                for (Coord i : slots) {
                    BotUtils.dropItemToInventory(i, quickBeltInv);
                    BotUtils.sleep(10);
                }
            }
        }
        if (gui.vhand != null) { //hand still not empty, dump into main inventory
            List<Coord> slots = BotUtils.getFreeInvSlots(BotUtils.playerInventory());
            for (Coord i : slots) {
                BotUtils.dropItemToInventory(i, BotUtils.playerInventory());
                BotUtils.sleep(10);
            }
        }
        BotUtils.sysLogAppend("Finished equipWeapon script, if a weapon did not equip check to ensure you had inventory space to remove a bindle/sack.","white");
    }catch(Exception e){
           BotUtils.sysMsg("Exception occured in EquipWeapon script, ignored.",Color.white);
           e.printStackTrace();
        }//ignore all exceptions, this script will likely be used in a combat situation and crashes are unacceptable
    }

    private HashMap<WItem, Integer> getWeapon(Inventory inv) {
        HashMap<WItem, Integer> map = new HashMap<>();
        int priority;
        WItem weapon = inv.getItemPartial("Sword");
        priority = 3;
        if (weapon == null) {
            weapon = inv.getItemPartial("Battleaxe");
            priority = 2;
        }
        if (weapon == null) {
            weapon = inv.getItemPartial("Axe");
            priority = 1;
        }
        if(weapon!=null)
            map.put(weapon, priority);
        return map;
    }

    private HashMap<WItem, Integer> getWeaponQuickBelt(InventoryBelt inv) {
        HashMap<WItem, Integer> map = new HashMap<>();
        int priority = 0;
        WItem weapon = inv.getItemPartial("Sword");
        priority = 3;
        if (weapon == null) {
            weapon = inv.getItemPartial("Battleaxe");
            priority = 2;
        }
        if (weapon == null) {
            weapon = inv.getItemPartial("Axe");
            priority = 1;
        }
        if(weapon!=null)
            map.put(weapon, priority);
        return map;
    }
}
