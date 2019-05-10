package haven.automation;


import haven.*;
import haven.Window;
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;

import java.awt.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;

public class EquipWeapon implements Runnable {
    private GameUI gui;
    private static final int TIMEOUT = 2000;
    private static final Coord sqsz = new Coord(36, 33);

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
                PBotUtils.sysMsg("Already found weapon in left hand, canceling.", Color.white);
                return;
            }
        }

        if (rhand != null) {
            if (rhand.name.get().contains("Sword") || rhand.name.get().contains("Battleaxe")) {
                PBotUtils.sysMsg("Already found weapon in right hand, canceling.", Color.white);
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
            PBotUtils.sysMsg("No weapons found",Color.white);
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
        if(weapons.size() == 0){
            PBotUtils.sysMsg("No weapons found.",Color.white);
            return;
        }
        weaponItem = weapons.get(weapons.size() - 1).item;

            Equipory e = gui.getequipory();
            if (e == null)//equipory is somehow null, break
                return;

        if(lhand == null || rhand == null) {//nothing in either left or right hand, makes equipping easy.
            weaponItem.wdgmsg("take", new Coord(weaponItem.sz.x / 2, weaponItem.sz.y / 2));

            try {
                if (!Utils.waitForOccupiedHand(gui, TIMEOUT, "waitForOccupiedHand timed-out"))
                    return;
            } catch (InterruptedException ie) {
                return;
            }
            if (lhand == null) //try to find an empty hand first, otherwise drop it in left hand
                e.wdgmsg("drop", 6);
            else
                e.wdgmsg("drop", 7);
        } else {//neither hand is empty, if we're equipping a sword this is no problem, if we're not - it's a problem.
            if (weaponItem.getname().contains("Sword") ||(weaponItem.getname().contains("Axe") && !weaponItem.getname().contains("Battleaxe"))) {//if sword just drop it in a hand
                weaponItem.wdgmsg("take", new Coord(weaponItem.sz.x / 2, weaponItem.sz.y / 2));

                try {
                    if (!Utils.waitForOccupiedHand(gui, TIMEOUT, "waitForOccupiedHand timed-out"))
                        return;
                } catch (InterruptedException ie) {
                    return;
                }
                e.wdgmsg("drop", 6);
            }
            else {//else, we have to empty one hand before we can drop it in.
                PBotUtils.takeItem(lhand);
                try {
                    if (!Utils.waitForOccupiedHand(gui, TIMEOUT, "waitForOccupiedHand timed-out"))
                        return;
                } catch (InterruptedException ie) {
                    return;
                }
                if (beltInv != null && PBotUtils.getFreeInvSlots(beltInv).size() > 0)//if not null and at least 1 slot, drop it to belt, otherwise drop to inv.
                    PBotUtils.dropItemToInventory(PBotUtils.getFreeInvSlot(beltInv), beltInv);
                else if (quickBeltInv != null && quickBeltInv.getFreeSlots().size() > 0) {//if not null and at least 1 slot, drop it to belt, otherwise drop to inv.
                    List<Coord> slots = quickBeltInv.getFreeSlots();
                    System.out.println("slots count "+slots.size()+" slots "+slots);
                    for (Coord i : slots) {
                        if(PBotUtils.getGItemAtHand() == null)
                            break;
                        Coord dc = i.add(sqsz.div(2)).div(sqsz);
                        // convert single row coordinate into multi-row
                        if (dc.x >= quickBeltInv.isz.x) {
                            dc.y = dc.x / quickBeltInv.isz.x;
                            dc.x = dc.x % quickBeltInv.isz.x;
                        }
                        quickBeltInv.wdgmsg("drop",dc);
                        PBotUtils.sleep(100);
                    }
                }
                else
                    PBotUtils.dropItemToInventory(PBotUtils.getFreeInvSlot(PBotAPI.gui.maininv), PBotAPI.gui.maininv);
                int timeout = 0;
                while (PBotUtils.getGItemAtHand() != null) {//sleep till we recognize item is gone from cursor
                    timeout++;
                    if (timeout > 200)//exit script if we stuck in this while loop
                        return;
                    PBotUtils.sleep(10);
                }
                PBotUtils.takeItem(weaponItem);
                try {
                    if (!Utils.waitForOccupiedHand(gui, TIMEOUT, "waitForOccupiedHand timed-out"))
                        return;
                } catch (InterruptedException ie) {
                    return;
                }
                e.wdgmsg("drop", 6);
                while (PBotUtils.getGItemAtHand() == weaponItem)//sleep till we recognize item on cursor has changed.
                    PBotUtils.sleep(100);
            }
        }

        GItem hand = PBotUtils.getGItemAtHand();
        if (hand != null) { //try to empty hand into belt
            if (beltInv != null) {
                List<Coord> slots = PBotUtils.getFreeInvSlots(beltInv);
                for (Coord i : slots) {
                    if(PBotUtils.getGItemAtHand() == null)
                        break;
                    PBotUtils.dropItemToInventory(i, beltInv);
                    PBotUtils.sleep(100);
                }
            } else if (quickBeltInv != null) {
                List<Coord> slots = quickBeltInv.getFreeSlots();
                for (Coord i : slots) {
                    if(PBotUtils.getGItemAtHand() == null)
                        break;
                    Coord dc = i.add(sqsz.div(2)).div(sqsz);
                    // convert single row coordinate into multi-row
                    if (dc.x >= quickBeltInv.isz.x) {
                        dc.y = dc.x / quickBeltInv.isz.x;
                        dc.x = dc.x % quickBeltInv.isz.x;
                    }
                    quickBeltInv.wdgmsg("drop",dc);
                    PBotUtils.sleep(100);
                }
            }
        }
        if (PBotUtils.getGItemAtHand() != null) { //hand still not empty, dump into main inventory
            List<Coord> slots = PBotUtils.getFreeInvSlots(PBotAPI.gui.maininv);
            for (Coord i : slots) {
                if(PBotUtils.getGItemAtHand() == null)
                    break;
                PBotUtils.dropItemToInventory(i, PBotAPI.gui.maininv);
                PBotUtils.sleep(100);
            }
        }
            PBotUtils.sysLogAppend("Finished equipWeapon script, if a weapon did not equip check to ensure you had inventory space to remove a bindle/sack.","white");
    }catch(Exception e){
            PBotUtils.sysMsg("Exception occurred in EquipWeapon script, ignored.",Color.white);
           e.printStackTrace();
        }//ignore all exceptions, this script will likely be used in a combat situation and crashes are unacceptable
    }

    private HashMap<WItem, Integer> getWeapon(Inventory inv) {
        HashMap<WItem, Integer> map = new HashMap<>();
        int priority;
        WItem weapon = inv.getItemPartial("Battleaxe");
        priority = 5;
        if (weapon == null) {
            weapon = inv.getItemPartial("Cutblade");
            priority = 4;
        }
        if (weapon == null) {
            weapon = inv.getItemPartial("Sword");
            priority = 3;
        }
        if (weapon == null) {
            weapon = inv.getItemPartial("Boar Spear");
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
        int priority;
        WItem weapon = inv.getItemPartial("Battleaxe");
        priority = 5;
        if (weapon == null) {
            weapon = inv.getItemPartial("Cutblade");
            priority = 4;
        }
        if (weapon == null) {
            weapon = inv.getItemPartial("Sword");
            priority = 3;
        }
        if (weapon == null) {
            weapon = inv.getItemPartial("Boar Spear");
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
