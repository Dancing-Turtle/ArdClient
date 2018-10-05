package haven.automation;


import haven.*;
import haven.Window;
import haven.purus.BotUtils;
import haven.Button;
import haven.Coord;
import haven.FlowerMenu;
import haven.GItem;
import haven.GameUI;
import haven.Gob;
import haven.HavenPanel;
import haven.IMeter;
import haven.Inventory;
import haven.Label;
import haven.Widget;
import haven.Window;
import haven.purus.pbot.PBotAPI;


import java.awt.*;
import java.util.ArrayList;
import java.util.List;
public class BeltDrink implements Runnable {
    public GameUI gui;

    public BeltDrink(GameUI gui) {
        this.gui = gui;
    }

    public void run() {
        try {
            int threshold = 100;
            IMeter.Meter stam = gui.getmeter("stam", 0);
            if (stam.a > 95 || stam.a > threshold)
                return;
            WItem drinkable;
            List<Widget> children = new ArrayList<>();
            List<WItem> drinks = new ArrayList<>();
            synchronized (gui.ui.root.lchild) {
                try {
                    for (Widget q = gui.ui.root.lchild; q != null; q = q.rnext()) {
                        if (q instanceof Inventory) {
                            drinkable = getDrinkable((Inventory) q);
                            if (drinkable != null) {
                                children.add(q);
                                drinks.add(getDrinkable((Inventory) q));
                            }
                        }
                        if(q instanceof InventoryBelt){
                            drinkable = getDrinkableAlt((InventoryBelt) q);
                            if (drinkable != null) {
                                children.add(q);
                                drinks.add(getDrinkableAlt((InventoryBelt) q));
                            }
                        }

                    }

                } catch (NullPointerException q) {
                }
            }
            for (Widget x : children) {
                if(x instanceof Inventory)
                drinkable = getDrinkable((Inventory) x);
                else
                drinkable = getDrinkableAlt((InventoryBelt) x);

                GItem drinkItem = drinkable.item;
                if (drinkItem != null) {
                    FlowerMenu.setNextSelection("Drink");
                    drinkItem.wdgmsg("iact", new Coord(drinkItem.sz.x / 2, drinkItem.sz.y / 2), 3);
                    BotUtils.sleep(250);
                    while (gui.prog >= 0) {
                        BotUtils.sleep(100);
                    }
                    FlowerMenu menu = gui.ui.root.findchild(FlowerMenu.class);
                    if (menu != null) {
                        for (FlowerMenu.Petal opt : menu.opts) {
                            if (opt.name.equals("Drink")) {
                                menu.destroy();
                            }
                        }
                    }
                    return;
                }
            }
             if (drinks.size() == 0) {
                 BotUtils.sysMsg("No water found! Belt Closed?", Color.WHITE);
                 //gui.error("No water found!");
                 // }
             }
        } catch (NullPointerException q) {
        }
    }
        private WItem getDrinkable (Inventory inv){
            WItem drinkable = inv.getItemPartialDrink("Waterskin");
            // BotUtils.sysMsg("Checking Inventory", Color.WHITE);
            if (drinkable == null)
                drinkable = inv.getItemPartialDrink("Waterflask");
            if (drinkable == null)
                drinkable = inv.getItemPartialDrink("Kuska");
            if (drinkable == null) {
                return null;
            }
            return drinkable;
        }

    private WItem getDrinkableAlt (InventoryBelt inv){
        WItem drinkable = inv.getItemPartialDrink("Waterskin");
        // BotUtils.sysMsg("Checking Inventory", Color.WHITE);
        if (drinkable == null)
            drinkable = inv.getItemPartialDrink("Waterflask");
        if (drinkable == null)
            drinkable = inv.getItemPartialDrink("Kuska");
        if (drinkable == null) {
            return null;
        }
        return drinkable;
    }
    }


