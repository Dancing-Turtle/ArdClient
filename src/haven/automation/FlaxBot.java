package haven.automation;


import haven.Button;
import haven.Coord;
import haven.FlowerMenu;
import haven.GItem;
import haven.GameUI;
import haven.Gob;
import haven.GobHighlight;
import haven.IMeter;
import haven.Label;
import haven.Loading;
import haven.Resource;
import haven.Sprite;
import haven.WItem;
import haven.Widget;
import haven.Window;
import haven.purus.pbot.PBotUtils;
import haven.res.ui.tt.q.qbuff.QBuff;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import static haven.OCache.posres;

public class FlaxBot extends Window {
    public Label lblProg, lblProg2, lblhighestq;
    public int cropsHarvested;
    private double highestquality;
    private Thread runner;
    public Button stopBtn;
    public List<Gob> blacklist = new ArrayList<>();
    public Gob g;
    private boolean stopThread = false;
    private Set<String> plants = new HashSet<>(5);

    public FlaxBot(GameUI gui) {
        super(new Coord(140, 95), "Flax Farmer");

        plants.add("gfx/terobjs/plants/flax");
        cropsHarvested = 0;
        Label lblstxt = new Label("Progress:");
        add(lblstxt, new Coord(15, 35));
        lblProg = new Label("Initialising...");
        add(lblProg, new Coord(15, 45));
        Label lblstxt2 = new Label("Status:");
        add(lblstxt2, new Coord(15, 55));
        lblProg2 = new Label("Initialising...");
        add(lblProg2, new Coord(15, 65));
        Label lblhighest = new Label("Top Q:");
        add(lblhighest, new Coord(15, 75));
        lblhighestq = new Label("Initialising...");
        add(lblhighestq, new Coord(15, 85));


        stopBtn = new Button(120, "Stop") {
            @Override
            public void click() {
                stop();
            }
        };
        add(stopBtn, new Coord(0, 0));
        runner = new Thread(new FlaxBot.runner(), "Flax Farmer");
        runner.start();
    }

    private class runner implements Runnable {
        @Override
        public void run() {
            highestquality = 0;
            PBotUtils.sysMsg(ui, "Flax Bot Started!", Color.white);
            lblProg.settext(cropsHarvested + " Units Harvested");
            lblProg2.settext(cropsHarvested + "Starting");
            while (!stopThread || ui.gui.getwnd("Flax Farmer") != null) {
                try {
                    lblProg.settext(cropsHarvested + " Units Harvested");
                    IMeter.Meter stam = ui.gui.getmeter("stam", 0);
                    if (stam.a <= 30) {
                        lblProg2.settext("Drinking");
                        PBotUtils.drink(ui, true);
                    }

                    if (stopThread || ui.gui.getwnd("Flax Farmer") == null)
                        return;

                    while (PBotUtils.findNearestStageCrop(ui, 5000, 3, "gfx/terobjs/plants/flax") == null) {
                        if (stopThread || ui.gui.getwnd("Flax Farmer") == null)
                            return;
                        lblProg2.settext("No Flax");
                        PBotUtils.sleep(200);
                    }
                    while (g == null) {
                        lblProg2.settext("Found Flax");
                        if (stopThread || ui.gui.getwnd("Flax Farmer") == null)
                            return;
                        g = PBotUtils.findNearestStageCrop(ui, 5000, 3, "gfx/terobjs/plants/flax");
                    }

                    PBotUtils.doClick(ui, g, 1, 0);

                    int retryharvest = 0;
                    int retrycount = 0;
                    ui.gui.map.wdgmsg("click", Coord.z, g.rc.floor(posres), 1, 0);
                    while (PBotUtils.player(ui).rc.x != g.rc.x || PBotUtils.player(ui).rc.y != g.rc.y) {
                        if (stopThread || ui.gui.getwnd("Flax Farmer") == null)
                            return;
                        if (!PBotUtils.isMoving(ui))
                            retryharvest++;
                        lblProg2.settext("Moving to Crop");
                        if (retryharvest >= 500) {
                            lblProg2.settext("Retry Movement");
                            PBotUtils.sysLogAppend(ui, "Moving char in move loop", "white");
                            Gob player = ui.gui.map.player();
                            Coord location = player.rc.floor(posres);
                            int x = location.x + getrandom();
                            int y = location.y + getrandom();
                            Coord finalloc = new Coord(x, y);
                            ui.gui.map.wdgmsg("click", Coord.z, finalloc, 1, 0);
                            retryharvest = 0;
                            PBotUtils.sleep(1000);
                            ui.gui.map.wdgmsg("click", Coord.z, g.rc.floor(posres), 1, 0);
                        }
                        PBotUtils.sleep(10);
                    }
                    lblProg2.settext("Harvesting");
                    try {
                        PBotUtils.pfRightClick(ui, g, 0);
                    } catch (NullPointerException qq) {
                        PBotUtils.sysLogAppend(ui, "Flax I found is now null, weird. Retrying.", "white");
                        g = null;
                        while (PBotUtils.findNearestStageCrop(ui, 5000, 3, "gfx/terobjs/plants/flax") == null)
                            PBotUtils.sleep(10);
                        g = PBotUtils.findNearestStageCrop(ui, 5000, 3, "gfx/terobjs/plants/flax");
                        PBotUtils.pfRightClick(ui, g, 0);
                    }

                    // Wait for harvest menu to appear and harvest the crop
                    while (ui.root.findchild(FlowerMenu.class) == null) {
                        lblProg2.settext("Waiting for Flowermenu");
                        if (stopThread || ui.gui.getwnd("Flax Farmer") == null)
                            return;
                        retryharvest++;
                        PBotUtils.sleep(10);
                        if (PBotUtils.getItemAtHand(ui) != null) {
                            Coord slot = PBotUtils.getFreeInvSlot(ui.gui.maininv);
                            PBotUtils.dropItemToInventory(slot, ui.gui.maininv);
                            while (PBotUtils.getItemAtHand(ui) != null)
                                PBotUtils.sleep(50);
                        }
                        if (retryharvest >= 500) {
                            lblProg2.settext("Retry Harvest");
                            if (retrycount >= 3) {
                                lblProg2.settext("Unstucking");
                                PBotUtils.sysLogAppend(ui, "Moving char", "white");
                                Gob player = ui.gui.map.player();
                                Coord location = player.rc.floor(posres);
                                int x = location.x + getrandom();
                                int y = location.y + getrandom();
                                Coord finalloc = new Coord(x, y);
                                ui.gui.map.wdgmsg("click", Coord.z, finalloc, 1, 0);
                                retrycount = 0;
                                PBotUtils.sleep(1000);
                                PBotUtils.pfRightClick(ui, g, 0);
                            }
                            PBotUtils.sysLogAppend(ui, "Retrying harvest", "white");
                            lblProg2.settext("Retrying Harvest");
                            try {
                                PBotUtils.doClick(ui, g, 3, 0);
                            } catch (NullPointerException qq) {
                                PBotUtils.sysLogAppend(ui, "Flax I found is now null, weird. Retrying.", "white");
                                g = null;
                                while (PBotUtils.findNearestStageCrop(ui, 5000, 3, "gfx/terobjs/plants/flax") == null)
                                    PBotUtils.sleep(10);
                                g = PBotUtils.findNearestStageCrop(ui, 5000, 3, "gfx/terobjs/plants/flax");
                                PBotUtils.doClick(ui, g, 3, 0);
                            }
                            retryharvest = 0;
                            retrycount++;
                        }

                        if (stopThread || ui.gui.getwnd("Flax Farmer") == null)
                            return;
                    }

                    if (stopThread || ui.gui.getwnd("Flax Farmer") == null)
                        return;

                    FlowerMenu menu = ui.root.findchild(FlowerMenu.class);
                    if (menu != null) {
                        if (stopThread)
                            return;
                        for (FlowerMenu.Petal opt : menu.opts) {
                            if (opt.name.equals("Harvest")) {
                                menu.choose(opt);
                                menu.destroy();
                            }
                        }
                    }
                    while (PBotUtils.findObjectById(ui, g.id) != null) {
                        lblProg2.settext("Waiting for Harvest");
                        if (stopThread || ui.gui.getwnd("Flax Farmer") == null)
                            return;
                        retryharvest++;
                        PBotUtils.sleep(10);
                        if (retryharvest >= 500) {
                            lblProg2.settext("Retry Harvest");
                            if (retrycount >= 3) {
                                lblProg2.settext("Unstucking");
                                PBotUtils.sysLogAppend(ui, "Moving char", "white");
                                Gob player = ui.gui.map.player();
                                Coord location = player.rc.floor(posres);
                                int x = location.x + getrandom();
                                int y = location.y + getrandom();
                                Coord finalloc = new Coord(x, y);
                                ui.gui.map.wdgmsg("click", Coord.z, finalloc, 1, 0);
                                retrycount = 0;
                                PBotUtils.sleep(1000);
                            }
                            PBotUtils.sysLogAppend(ui, "Retrying harvest", "white");
                            try {
                                PBotUtils.doClick(ui, g, 3, 0);
                            } catch (NullPointerException qq) {
                                PBotUtils.sysLogAppend(ui, "Flax I found is now null, weird. Retrying.", "white");
                                g = null;
                                while (PBotUtils.findNearestStageCrop(ui, 5000, 3, "gfx/terobjs/plants/flax") == null)
                                    PBotUtils.sleep(10);
                                g = PBotUtils.findNearestStageCrop(ui, 5000, 3, "gfx/terobjs/plants/flax");
                                PBotUtils.doClick(ui, g, 3, 0);
                            }
                            retryharvest = 0;
                            retrycount++;
                        }
                    }

                    if (stopThread || ui.gui.getwnd("Flax Farmer") == null)
                        return;
                    PBotUtils.sleep(200);

                    try {
                        while (ui.gui.maininv.getItemPartial("Flax") == null)
                            PBotUtils.sleep(10);
                        WItem flax = ui.gui.maininv.getItemPartial("Flax");
                        GItem flax2 = flax.item;
                        java.util.List<WItem> items = ui.gui.maininv.getIdenticalItems((flax2));
                        sort(items);
                        for (WItem seeds : items) {
                            GItem item = seeds.item;
                            if (item.quality().q > highestquality) {
                                highestquality = item.quality().q;
                                lblhighestq.settext("Quality " + item.quality().q);
                            }
                            if (PBotUtils.getAmount(item) >= 5) {
                                lblProg2.settext("Picking Up Seeds");
                                //    PBotUtils.sysLogAppend("" + item.quality().q, "white");
                                PBotUtils.takeItem(ui, item);
                                break;
                            }
                        }
                        retryharvest = 0;
                        while (PBotUtils.getItemAtHand(ui) == null) {
                            lblProg2.settext("Waiting to Pickup Seeds");
                            PBotUtils.sleep(10);
                            retryharvest++;
                            if (retryharvest > 500) {
                                flax = ui.gui.maininv.getItemPartial("Flax");
                                flax2 = flax.item;
                                items = ui.gui.maininv.getIdenticalItems((flax2));
                                sort(items);
                                for (WItem seeds : items) {
                                    GItem item = seeds.item;
                                    if (PBotUtils.getAmount(item) >= 5) {
                                        lblProg2.settext("Picking Up Seeds");
                                        PBotUtils.takeItem(ui, item);
                                        break;
                                    }
                                }
                            }
                        }
                        // Plant the seed from hand
                        int amount;
                        amount = PBotUtils.getAmount(PBotUtils.getGItemAtHand(ui));
                        lblProg2.settext("Planting");
                        // PBotUtils.mapInteractClick();
                        ui.gui.map.wdgmsg("itemact", Coord.z, PBotUtils.player(ui).rc.floor(posres), 0, 0, (int) PBotUtils.player(ui).id, PBotUtils.player(ui).rc.floor(posres), 0, -1);
                        retrycount = 0;
                        while (PBotUtils.findNearestStageCrop(ui, 5, 0, "gfx/terobjs/plants/flax") == null || (PBotUtils.getItemAtHand(ui) != null
                                && amount == PBotUtils.getAmount(PBotUtils.getGItemAtHand(ui)))) {
                            if (stopThread || ui.gui.getwnd("Flax Farmer") == null)
                                return;
                            retryharvest++;
                            if (retryharvest > 500) {
                                lblProg2.settext("Retry Planting");
                                //  PBotUtils.mapInteractClick();
                                ui.gui.map.wdgmsg("itemact", Coord.z, PBotUtils.player(ui).rc.floor(posres), 0, 0, (int) PBotUtils.player(ui).id, PBotUtils.player(ui).rc.floor(posres), 0, -1);
                                retryharvest = 0;
                                retrycount++;
                            }
                            if (PBotUtils.getItemAtHand(ui) != null & retrycount >= 3) {
                                PBotUtils.sysLogAppend(ui, "Giving up on this replant, skipping", "white");
                                break;
                            }
                            if (PBotUtils.getItemAtHand(ui) == null && retrycount >= 3) {
                                PBotUtils.sysLogAppend(ui, "Retry pickup and plant", "white");
                                lblProg2.settext("Retry Pickup Item and plant");
                                sort(items);
                                for (WItem seeds : items) {
                                    GItem item = seeds.item;
                                    if (item.quality().q > highestquality) {
                                        highestquality = item.quality().q;
                                        lblhighestq.settext("Quality " + item.quality().q);
                                    }
                                    if (PBotUtils.getAmount(item) >= 5) {
                                        lblProg2.settext("Picking Up Seeds");
                                        PBotUtils.sysLogAppend(ui, "Replanting flax of quality : " + item.quality().q, "white");
                                        PBotUtils.takeItem(ui, item);
                                        break;
                                    }
                                }
                                //  PBotUtils.mapInteractClick();
                                ui.gui.map.wdgmsg("itemact", Coord.z, PBotUtils.player(ui).rc.floor(posres), 0, 0, (int) PBotUtils.player(ui).id, PBotUtils.player(ui).rc.floor(posres), 0, -1);
                            }
                            lblProg2.settext("Waiting for Planting Complete");
                            PBotUtils.sleep(10);
                        }
                        retrycount = 0;


// Merge seed from hand into inventory or put it in inventory
                        //commented out at request to prevent mixing high and low q seeds
                        if (ui.gui.maininv.getItemPartial("Flax") != null && PBotUtils.getItemAtHand(ui) != null && PBotUtils.getAmount(PBotUtils.getGItemAtHand(ui)) != 50) {
                            flax = ui.gui.maininv.getItemPartial("Flax");
                            items = ui.gui.maininv.getIdenticalItems((flax.item));
                            for (WItem seedslol : items) {
                                if (PBotUtils.getAmount(seedslol.item) < 50)
                                    continue;
                                if (PBotUtils.getAmount(PBotUtils.getGItemAtHand(ui)) == 50)
                                    break;
                                if (seedslol.item.quality().q == PBotUtils.getGItemAtHand(ui).quality().q) {
                                    System.out.println("Combining quality : " + PBotUtils.getGItemAtHand(ui).quality().q + " with quality : " + seedslol.item.quality().q + " seeds.");
                                    int handAmount = PBotUtils.getAmount(PBotUtils.getGItemAtHand(ui));
                                    try {
                                        seedslol.item.wdgmsg("itemact", 0);
                                    } catch (Exception e) {
                                    }
                                    while (PBotUtils.getItemAtHand(ui) != null && PBotUtils.getAmount(PBotUtils.getGItemAtHand(ui)) == handAmount) {
                                        if (stopThread || ui.gui.getwnd("Flax Farmer") == null)
                                            return;
                                        PBotUtils.sleep(50);
                                    }
                                    break;
                                }
                            }
                        }

                        if (PBotUtils.getItemAtHand(ui) != null) {
                            lblProg2.settext("Dropping Seeds to Inv");
                            Coord slot = PBotUtils.getFreeInvSlot(ui.gui.maininv);
                            if (slot != null) {
                                int freeSlots = PBotUtils.invFreeSlots(ui);
                                PBotUtils.dropItemToInventory(slot, ui.gui.maininv);
                                while (PBotUtils.getItemAtHand(ui) != null)
                                    PBotUtils.sleep(50);
                            }
                        }
                        if (PBotUtils.invFreeSlots(ui) < 3) {
                            if (stopThread || ui.gui.getwnd("Flax Farmer") == null)
                                return;
                            lblProg2.settext("Barreling");
                            Gob barrel = PBotUtils.findNearestBarrel(ui, 5000, blacklist);
                            barrel.delattr(GobHighlight.class);
                            barrel.setattr(new GobHighlight(barrel));
                            flax = ui.gui.maininv.getItemPartial("Flax");
                            flax2 = flax.item;
                            items = ui.gui.maininv.getIdenticalItems((flax2));
                            sort(items);
                            if (PBotUtils.getItemAtHand(ui) != null)
                                PBotUtils.dropItem(ui, 0);
                            PBotUtils.pfRightClick(ui, barrel, 0);
                            PBotUtils.waitForWindow(ui, (Resource.getLocString(Resource.BUNDLE_WINDOW, "Barrel")));
                            if (PBotUtils.getItemAtHand(ui) != null) {
                                ui.gui.map.wdgmsg("itemact", Coord.z, barrel.rc.floor(posres), 0, 0, (int) barrel.id,
                                        barrel.rc.floor(posres), 0, -1);
                                int i = 0;
                                while (PBotUtils.getItemAtHand(ui) != null) {
                                    if (i > 250) {
                                        PBotUtils.sysLogAppend(ui, "Blacklisting barrel, appears to be full", "white");
                                        blacklist.add(barrel);
                                        barrel = PBotUtils.findNearestBarrel(ui, 2000, blacklist);
                                        PBotUtils.sleep(500);
                                        if (PBotUtils.getItemAtHand(ui) != null) {
                                            lblProg2.settext("Dropping Seeds to Inv");
                                            Coord slot = PBotUtils.getFreeInvSlot(ui.gui.maininv);
                                            if (slot != null) {
                                                PBotUtils.dropItemToInventory(slot, ui.gui.maininv);
                                                while (PBotUtils.getItemAtHand(ui) != null)
                                                    PBotUtils.sleep(50);
                                            }
                                        }
                                        break;
                                    }
                                    PBotUtils.sleep(10);
                                    i++;
                                }
                            }
                            items.subList(0, 14).clear();
                            for (WItem seed : items) {
                                if (stopThread)
                                    break;
                                GItem item = seed.item;
                                PBotUtils.takeItem(ui, item);

                                ui.gui.map.wdgmsg("itemact", Coord.z, barrel.rc.floor(posres), 0, 0, (int) barrel.id,
                                        barrel.rc.floor(posres), 0, -1);
                                int i = 0;
                                while (PBotUtils.getItemAtHand(ui) != null) {
                                    if (i > 250) {
                                        PBotUtils.sysLogAppend(ui, "Blacklisting barrel, appears to be full", "white");
                                        blacklist.add(barrel);
                                        Coord slot = PBotUtils.getFreeInvSlot(ui.gui.maininv);
                                        PBotUtils.dropItemToInventory(slot, ui.gui.maininv);
                                        PBotUtils.sleep(250);
                                        barrel = PBotUtils.findNearestBarrel(ui, 2000, blacklist);
                                        PBotUtils.pfRightClick(ui, barrel, 0);
                                        int retryclick = 0;
                                        while (ui.gui.getwnd((Resource.getLocString(Resource.BUNDLE_WINDOW, "Barrel"))) == null) {
                                            if (retryclick > 200) {
                                                retryclick = 0;
                                                PBotUtils.pfRightClick(ui, barrel, 0);
                                            }
                                            retryclick++;
                                            PBotUtils.sleep(10);
                                        }
                                        break;
                                    }
                                    PBotUtils.sleep(10);
                                    i++;
                                }
                            }
                        }

                    } catch (NullPointerException x) {
                        PBotUtils.sysLogAppend(ui, "Null pointer exception caught, crash prevented.", "white");
                    }
                    g = null;
                    cropsHarvested++;
                    lblProg.settext(cropsHarvested + " Units Harvested");
                } catch (Loading | Sprite.ResourceException | NullPointerException e) {
                    e.printStackTrace();
                }
            }
        }
    }


    public int getrandom() {
        Random r = new Random();
        int randomNumber = r.ints(1, -6000, 6000).findFirst().getAsInt();
        return randomNumber;
    }

    class CoordSort implements Comparator<Gob> {
        public int compare(Gob a, Gob b) {
            if (a.rc.floor().x == b.rc.floor().x) {
                if (a.rc.floor().x % 2 == 0)
                    return (a.rc.floor().y < b.rc.floor().y) ? 1 : (a.rc.floor().y > b.rc.floor().y) ? -1 : 0;
                else
                    return (a.rc.floor().y < b.rc.floor().y) ? -1 : (a.rc.floor().y > b.rc.floor().y) ? 1 : 0;
            } else
                return (a.rc.floor().x < b.rc.floor().x) ? -1 : (a.rc.floor().x > b.rc.floor().x) ? 1 : 0;
        }
    }

    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (sender == cbtn) {
            stopThread = true;
            stop();
            reqdestroy();
        } else
            super.wdgmsg(sender, msg, args);
    }

    public void stop() {
        // Stops thread
        PBotUtils.sysMsg(ui, "Flax Farmer stopped!", Color.white);
        runner.interrupt();
        stopThread = true;
        this.destroy();
    }

    public void sort(List<WItem> items) {
        Collections.sort(items, (a, b) -> {
            QBuff aq = a.item.quality();
            QBuff bq = b.item.quality();
            if (aq == null || bq == null)
                return 0;
            else if (aq.q == bq.q)
                return 0;
            else if (aq.q > bq.q)
                return -1;
            else
                return 1;
        });
    }

    public WItem getItemPartial(String name) {
        for (Widget wdg = child; wdg != null; wdg = wdg.next) {
            if (wdg instanceof WItem) {
                String wdgname = ((WItem) wdg).item.getname();
                if (wdgname.contains(name))
                    return (WItem) wdg;
            }
        }
        return null;
    }
}


