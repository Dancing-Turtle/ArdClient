package haven.automation;


import haven.*;
import haven.Button;
import haven.Label;
import haven.Window;
import haven.purus.BotUtils;
import haven.purus.SeedCropFarmer;
import haven.purus.pbot.PBotAPI;
import haven.res.ui.tt.q.qbuff.QBuff;
import net.dv8tion.jda.client.entities.Application;

import javax.swing.text.Highlighter;
import java.awt.*;
import java.util.*;
import java.util.List;

import static haven.OCache.posres;

public class FlaxBot extends Window {
    public Label lblProg, lblProg2;
    public int cropsHarvested;
    private Thread runner;
    GameUI gui;
    public Button stopBtn;
    public Gob g;
    private boolean stopThread = false;
    private Set<String> plants = new HashSet<>(5);
    public String cropname = "gfx/terobjs/plants/flax";
    public String seedname = "gfx/invobjs/seed-flax";

    public FlaxBot(GameUI gui) {
        super(new Coord(140, 75), "Flax Farmer");
        this.gui = gui;
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
                BotUtils.sysMsg("Flax Bot Started!", Color.white);
                lblProg.settext(cropsHarvested + " Units Harvested");
                lblProg2.settext(cropsHarvested + "Starting");
                GameUI gui = HavenPanel.lui.root.findchild(GameUI.class);
                while (!stopThread) {
                    lblProg.settext(cropsHarvested + " Units Harvested");
                    IMeter.Meter stam = gui.getmeter("stam", 0);
                    if (stam.a <= 30) {
                        lblProg2.settext("Drinking");
                        BotUtils.drink();
                    }

                    if (stopThread)
                        stop();

                   while(BotUtils.findNearestStageCropPartial(5000,3,"flax") == null) {
                       lblProg2.settext("No Flax");
                       BotUtils.sleep(5000);
                   }
                    while(g==null) {
                        lblProg2.settext("Found Flax");
                        g = BotUtils.findNearestStageCropPartial(5000, 3, "flax");

                    }

                        BotUtils.doClick(g, 1, 0);

                    int retryharvest = 0;
                    int retrycount = 0;
                        BotUtils.gui.map.wdgmsg("click", Coord.z, g.rc.floor(posres), 1, 0);
                        while (BotUtils.player().rc.x != g.rc.x || BotUtils.player().rc.y != g.rc.y) {
                            if (stopThread)
                                return;
                            retryharvest++;
                            lblProg2.settext("Moving to Crop");
                            if(retryharvest >= 500){
                                lblProg2.settext("Retry Movement");
                                BotUtils.sysLogAppend("Moving char in move loop","white");
                                Gob player = gui.map.player();
                                Coord location = player.rc.floor(posres);
                                int x = location.x + (int)Math.random() * 2000 - 1000;
                                int y = location.y + (int)Math.random() * 2000 - 1000;
                                Coord finalloc = new Coord(x, y);
                                gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
                                retryharvest = 0;
                                BotUtils.sleep(1000);
                                BotUtils.gui.map.wdgmsg("click", Coord.z, g.rc.floor(posres), 1, 0);
                            }
                            BotUtils.sleep(10);
                        }
                    lblProg2.settext("Harvesting");
                        BotUtils.pfRightClick(g, 0);

                        // Wait for harvest menu to appear and harvest the crop
                        while (ui.root.findchild(FlowerMenu.class) == null) {
                            lblProg2.settext("Waiting for Flowermenu");
                            if (stopThread)
                                return;
                            retryharvest++;
                            BotUtils.sleep(10);
                            if (retryharvest >= 500){
                                lblProg2.settext("Retry Harvest");
                                if(retrycount>=3){
                                    lblProg2.settext("Unstucking");
                                    BotUtils.sysLogAppend("Moving char","white");
                                    Gob player = gui.map.player();
                                    Coord location = player.rc.floor(posres);
                                        int x = location.x + (int)Math.random() * 2000 - 1000;
                                        int y = location.y + (int)Math.random() * 2000 - 1000;
                                        Coord finalloc = new Coord(x, y);
                                        gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
                                        retrycount = 0;
                                        BotUtils.sleep(1000);
                                    BotUtils.pfRightClick(g, 0);
                                }
                                BotUtils.sysLogAppend("Retrying harvest","white");
                                lblProg2.settext("Retrying Harvest");
                                BotUtils.doClick(g, 3, 0);
                                retryharvest = 0;
                                retrycount ++;
                            }

                            if (stopThread)
                                return;
                        }

                        if (stopThread)
                            return;

                        FlowerMenu menu = gui.ui.root.findchild(FlowerMenu.class);
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
                        while (BotUtils.findObjectById(g.id) != null) {
                            lblProg2.settext("Waiting for Harvest");
                            if (stopThread)
                                return;
                            retryharvest++;
                            BotUtils.sleep(10);
                            if (retryharvest >= 500){
                                lblProg2.settext("Retry Harvest");
                                if(retrycount>=3){
                                    lblProg2.settext("Unstucking");
                                    BotUtils.sysLogAppend("Moving char","white");
                                    Gob player = gui.map.player();
                                    Coord location = player.rc.floor(posres);
                                    int x = location.x + (int)Math.random() * 2000 - 1000;
                                    int y = location.y + (int)Math.random() * 2000 - 1000;
                                    Coord finalloc = new Coord(x, y);
                                    gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
                                    retrycount = 0;
                                    BotUtils.sleep(1000);
                                }
                                BotUtils.sysLogAppend("Retrying harvest","white");
                                BotUtils.doClick(g, 3, 0);
                                retryharvest = 0;
                                retrycount ++;
                            }
                        }

                        if (stopThread)
                            return;

                        try {
                            while (gui.maininv.getItemPartial("Flax") == null)
                                BotUtils.sleep(10);
                            WItem flax = gui.maininv.getItemPartial("Flax");
                            GItem flax2 = flax.item;
                            java.util.List<WItem> items = gui.maininv.getIdenticalItems((flax2));
                            sort(items);
                            for (WItem seeds : items) {
                                GItem item = seeds.item;
                                if (BotUtils.getAmount(item) >= 5) {
                                    lblProg2.settext(cropsHarvested + "Picking Up Seeds");
                                    BotUtils.sysLogAppend("Replanting flax of quality : " + item.quality().q, "white");
                                    BotUtils.takeItem(item);
                                    break;
                                }
                            }

                            while (BotUtils.getItemAtHand() == null){
                                lblProg2.settext("Waiting to Pickup Seeds");
                                BotUtils.sleep(10);
                        }
                                // Plant the seed from hand
                                int amount = 0;
                                if (seedname.contains("seed"))
                                    BotUtils.getAmount(BotUtils.getItemAtHand());
                            lblProg2.settext("Planting");
                                BotUtils.mapInteractClick(0);
                                while (BotUtils.findNearestStageCropPartial(5, 0, "flax") == null || (BotUtils.getItemAtHand() != null && (seedname.contains("seed") && amount == BotUtils.getAmount(BotUtils.getItemAtHand())))) {
                                    retryharvest++;
                                    if(retryharvest > 500){
                                        lblProg2.settext("Retry Planting");
                                        BotUtils.mapInteractClick(0);
                                        retryharvest = 0;
                                    }
                                    if(BotUtils.getItemAtHand() == null){
                                        lblProg2.settext("Retry Pickup Item and plant");
                                        sort(items);
                                        for (WItem seeds : items) {
                                            GItem item = seeds.item;
                                            if (BotUtils.getAmount(item) >= 5) {
                                                lblProg2.settext("Picking Up Seeds");
                                                BotUtils.sysLogAppend("Replanting flax of quality : " + item.quality().q, "white");
                                                BotUtils.takeItem(item);
                                                break;
                                            }
                                        }
                                        while(BotUtils.getItemAtHand() == null) {
                                            retryharvest++;
                                            lblProg2.settext("No Seeds On Cursor");
                                            if(retryharvest > 5000){
                                                BotUtils.sysMsg("Somehow I don't have any plantable flax. Stopping.",Color.white);
                                                stopBtn.click();
                                            }
                                            BotUtils.sleep(10);
                                        }
                                        BotUtils.mapInteractClick(0);
                                    }
                                    lblProg2.settext("Waiting for Planting Complete");
                                    BotUtils.sleep(10);
                                }


                                // Merge seed from hand into inventory or put it in inventory
                            //commented out at request to prevent mixing high and low q seeds
                             /*   for (Widget w = BotUtils.playerInventory().child; w != null; w = w.next) {
                                    if (w instanceof GItem && ((GItem) w).resource().name.equals(seedname)) {
                                        GItem item = (GItem) w;
                                        if (BotUtils.getItemAtHand() != null && BotUtils.getAmount(item) < 50) {
                                            int handAmount = BotUtils.getAmount(BotUtils.getItemAtHand());
                                            try {
                                                item.wdgmsg("itemact", 0);
                                            } catch (Exception e) {
                                                BotUtils.sysLogAppend("exception e line 155", "white");
                                            }
                                            while (BotUtils.getItemAtHand() != null && BotUtils.getAmount(BotUtils.getItemAtHand()) == handAmount)
                                                BotUtils.sleep(50);
                                        }
                                    }
                                }*/




                                if (BotUtils.getItemAtHand() != null) {
                                    lblProg2.settext("Dropping Seeds to Inv");
                                    Coord slot = BotUtils.getFreeInvSlot(BotUtils.playerInventory());
                                    if (slot != null) {
                                        int freeSlots = BotUtils.invFreeSlots();
                                        BotUtils.dropItemToInventory(slot, BotUtils.playerInventory());
                                        while (BotUtils.getItemAtHand() != null)
                                            BotUtils.sleep(50);
                                    }
                                }
                                if (BotUtils.invFreeSlots() < 3) {
                                    lblProg2.settext("Barreling");
                                    Gob barrel = BotUtils.findObjectByNames(2000, "gfx/terobjs/barrel");
                                    barrel.delattr(GobHighlight.class);
                                    barrel.setattr(new GobHighlight(barrel));
                                    // BotUtils.sysLogAppend("inv free slots <3", "white");
                                    flax = gui.maininv.getItemPartial("Flax");
                                    flax2 = flax.item;
                                    items = gui.maininv.getIdenticalItems((flax2));
                                    sort(items);
                                    // BotUtils.sysLogAppend("after sort", "white");
                                    for (int i = 0; i < 5; i++) {
                                        QBuff qual = items.get(i).item.quality();
                                        BotUtils.sysLogAppend("Item " + i + " quality  = " + qual.q, "white");
                                    }

                                    if (BotUtils.getItemAtHand() != null)
                                        BotUtils.dropItem(0);
                                    BotUtils.pfRightClick(barrel, 0);
                                    BotUtils.waitForWindow("Barrel");
                                    if (BotUtils.getItemAtHand() != null) {
                                        gameui().map.wdgmsg("itemact", Coord.z, barrel.rc.floor(posres), 0, 0, (int) barrel.id,
                                                barrel.rc.floor(posres), 0, -1);
                                        int i = 0;
                                        while (BotUtils.getItemAtHand() != null) {
                                            if (i == 60000)
                                                break;
                                            BotUtils.sleep(10);
                                            i++;
                                        }
                                    }
                                    // while (BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedname)).size() != 0) {
                                    items.subList(0, 14).clear();
                                    for (int i = 0; i < 5; i++) {
                                        QBuff qual = items.get(i).item.quality();
                                        BotUtils.sysLogAppend("Item2 " + i + " quality  = " + qual.q, "white");
                                    }
                                    for (WItem seed : items) {
                                        if (stopThread)
                                            break;
                                        GItem item = seed.item;
                                        //   item = BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedname)).get(0).item;
                                        BotUtils.takeItem(item);

                                        gameui().map.wdgmsg("itemact", Coord.z, barrel.rc.floor(posres), 0, 0, (int) barrel.id,
                                                barrel.rc.floor(posres), 0, -1);
                                        int i = 0;
                                        while (BotUtils.getItemAtHand() != null) {
                                            if (i == 60000)
                                                break;
                                            BotUtils.sleep(10);
                                            i++;
                                        }
                                    }
                                }

                        } catch (NullPointerException x) {
                        }
                        g = null;
                       // BotUtils.sysLogAppend("finished loop " + cropsHarvested, "white");
                        cropsHarvested++;
                        lblProg.settext(cropsHarvested + " Units Harvested");
                    }
                }
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
        public void wdgmsg (Widget sender, String msg, Object...args){
            if (sender == cbtn) {
                stopThread = true;
                stop();
                reqdestroy();
            } else
                super.wdgmsg(sender, msg, args);
        }

        public void stop () {
            // Stops thread
            BotUtils.sysMsg("Flax Farmer stopped!", Color.white);
            try {
                if (gameui().map.pfthread != null) {
                    gameui().map.pfthread.interrupt();
                }
            } catch (NullPointerException q) {
            }
            stopThread = true;
            this.destroy();
        }

        public void sort (List < WItem > items) {
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

        public WItem getItemPartial (String name){
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


