package haven.automation;


import haven.*;
import haven.Window;
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;

import java.awt.*;
import java.util.*;
import java.util.List;

public class EquipSwordShield implements Runnable {
    private GameUI gui;
    private static final int TIMEOUT = 2000;
    private static final Coord sqsz = new Coord(36, 33);

    public EquipSwordShield(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        try {//start giant try catch to prevent any loading/null crashes when using this.
            Inventory beltInv = null;
            InventoryBelt quickBeltInv = null;
            WItem lhand = gui.getequipory().quickslots[6];
            WItem rhand = gui.getequipory().quickslots[7];
            HashMap<WItem, Integer> wepmap = new HashMap<>();
            int iterations = 0;


            if (lhand != null && rhand != null) {
                if ((lhand.name.get().contains("Sword") && rhand.name.get().contains("shield")) || (lhand.name.get().contains("shield") && rhand.name.get().contains("Sword"))) {
                    PBotUtils.sysMsg("Already found traveler sacks, canceling.", Color.white);
                    return;
                }
            }
            if (lhand == null && rhand == null) {//if hands are empty obviously we need to run this twice to attempt to equip 2 sacks.
                iterations = 2;
            }
            else { //else figure out if we already have 1 sack equipped when we run
                if (lhand != null) {
                    if (!lhand.name.get().contains("Sword") && !lhand.name.get().contains("shield"))
                        iterations++;
                }
                else
                    iterations++;
                if (rhand != null) {
                    if (!rhand.name.get().contains("Sword") && !rhand.name.get().contains("shield"))
                        iterations++;
                }
                else
                    iterations++;
            }

           // System.out.println("equip sack iterations : "+iterations);

            for (int p = 0; p < iterations; p++) {
                wepmap.putAll(getWeapon(gui.maininv));
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
                    PBotUtils.sysMsg("No sword/shield found", Color.white);
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

                if (lhand == null) //try to find an empty hand first, otherwise drop it in left hand
                    e.wdgmsg("drop", 6);
                else if (rhand == null)
                    e.wdgmsg("drop", 7);
                else {//resolve what's in both hands to ensure we don't overwrite a sack with another sack
                    if (!lhand.name.get().contains("Sword") && !lhand.name.get().contains("shield"))
                        e.wdgmsg("drop", 6);
                    else if (!rhand.name.get().contains("Sword") && !rhand.name.get().contains("shield"))
                        e.wdgmsg("drop", 7);
                    try {
                        if (!Utils.waitForOccupiedHand(gui, TIMEOUT, "waitForOccupiedHand2 timed-out"))
                            return;
                    } catch (InterruptedException ie) {
                        return;
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
                if (gui.vhand != null) { //hand still not empty, dump into main inventory
                    List<Coord> slots = PBotUtils.getFreeInvSlots(PBotAPI.gui.maininv);
                    for (Coord i : slots) {
                        if(PBotUtils.getGItemAtHand() == null)
                            break;
                        PBotUtils.dropItemToInventory(i, PBotAPI.gui.maininv);
                        PBotUtils.sleep(100);
                    }
                }
                beltInv = null;
                quickBeltInv = null;
                lhand = PBotAPI.gui.getequipory().quickslots[6];
                rhand = PBotAPI.gui.getequipory().quickslots[7];
                wepmap.clear();
            }
        }catch(Exception e){
            PBotUtils.sysMsg("Exception occurred in EquipSwordShield script, ignored.", Color.white);
            e.printStackTrace();
        }//ignore all exceptions, this script will likely be used in a combat situation and crashes are unacceptable
    }

    private HashMap<WItem, Integer> getWeapon(Inventory inv) {
        HashMap<WItem, Integer> map = new HashMap<>();
        int priority;
        WItem weapon = inv.getItemPartial("Sword");
        priority = 3;
        if (weapon == null) {
            weapon = inv.getItemPartial("shield");
            priority = 2;
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
            weapon = inv.getItemPartial("shield");
            priority = 2;
        }
        if(weapon!=null)
            map.put(weapon, priority);
        return map;
    }
}
