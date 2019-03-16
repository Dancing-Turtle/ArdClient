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
    public Label lblProg, lblProg2, lblhighestq;
    public int cropsHarvested;
    private double highestquality;
    private Thread runner;
    GameUI gui;
    public Button stopBtn;
    public List<Gob> blacklist = new ArrayList<>();
    public Gob g;
    private boolean stopThread = false;
    private Set<String> plants = new HashSet<>(5);
    public String cropname = "gfx/terobjs/plants/flax";
    public String seedname = "gfx/invobjs/seed-flax";

    public FlaxBot(GameUI gui) {
        super(new Coord(140, 95), "Flax Farmer");
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
            BotUtils.sysMsg("Flax Bot Started!", Color.white);
            lblProg.settext(cropsHarvested + " Units Harvested");
            lblProg2.settext(cropsHarvested + "Starting");
            GameUI gui = HavenPanel.lui.root.findchild(GameUI.class);
            while (!stopThread) {
                try {
                    lblProg.settext(cropsHarvested + " Units Harvested");
                    IMeter.Meter stam = gui.getmeter("stam", 0);
                    if (stam.a <= 30) {
                        lblProg2.settext("Drinking");
                        BotUtils.drink();
                    }

                    if (stopThread)
                        stop();

                    while (BotUtils.findNearestStageCropPartial(5000, 3, "flax") == null) {
                        lblProg2.settext("No Flax");
                        BotUtils.sleep(200);
                    }
                    while (g == null) {
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
                        if(!BotUtils.isMoving())
                        retryharvest++;
                        lblProg2.settext("Moving to Crop");
                        if (retryharvest >= 500) {
                            lblProg2.settext("Retry Movement");
                            BotUtils.sysLogAppend("Moving char in move loop", "white");
                            Gob player = gui.map.player();
                            Coord location = player.rc.floor(posres);
                            int x = location.x + getrandom();
                            int y = location.y + getrandom();
                            Coord finalloc = new Coord(x, y);
                            gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
                            retryharvest = 0;
                            BotUtils.sleep(1000);
                            BotUtils.gui.map.wdgmsg("click", Coord.z, g.rc.floor(posres), 1, 0);
                        }
                        BotUtils.sleep(10);
                    }
                    lblProg2.settext("Harvesting");
                    try {
                        BotUtils.pfRightClick(g, 0);
                    } catch (NullPointerException qq) {
                        BotUtils.sysLogAppend("Flax I found is now null, weird. Retrying.", "white");
                        g = null;
                        while (BotUtils.findNearestStageCropPartial(5000, 3, "flax") == null)
                            BotUtils.sleep(10);
                        g = BotUtils.findNearestStageCropPartial(5000, 3, "flax");
                        BotUtils.pfRightClick(g, 0);
                    }

                    // Wait for harvest menu to appear and harvest the crop
                    while (ui.root.findchild(FlowerMenu.class) == null) {
                        lblProg2.settext("Waiting for Flowermenu");
                        if (stopThread)
                            return;
                        retryharvest++;
                        BotUtils.sleep(10);
                        if(BotUtils.getItemAtHand() != null){
                            Coord slot = BotUtils.getFreeInvSlot(BotUtils.playerInventory());
                            BotUtils.dropItemToInventory(slot,BotUtils.playerInventory());
                            while(BotUtils.getItemAtHand() != null)
                                BotUtils.sleep(50);
                        }
                        if (retryharvest >= 500) {
                            lblProg2.settext("Retry Harvest");
                            if (retrycount >= 3) {
                                lblProg2.settext("Unstucking");
                                BotUtils.sysLogAppend("Moving char", "white");
                                Gob player = gui.map.player();
                                Coord location = player.rc.floor(posres);
                                int x = location.x + getrandom();
                                int y = location.y + getrandom();
                                Coord finalloc = new Coord(x, y);
                                gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
                                retrycount = 0;
                                BotUtils.sleep(1000);
                                BotUtils.pfRightClick(g, 0);
                            }
                            BotUtils.sysLogAppend("Retrying harvest", "white");
                            lblProg2.settext("Retrying Harvest");
                            try {
                                BotUtils.doClick(g,3,0);
                            } catch (NullPointerException qq) {
                                BotUtils.sysLogAppend("Flax I found is now null, weird. Retrying.", "white");
                                g = null;
                                while (BotUtils.findNearestStageCropPartial(5000, 3, "flax") == null)
                                    BotUtils.sleep(10);
                                g = BotUtils.findNearestStageCropPartial(5000, 3, "flax");
                                BotUtils.doClick(g,3,0);
                            }
                            retryharvest = 0;
                            retrycount++;
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
                        if (retryharvest >= 500) {
                            lblProg2.settext("Retry Harvest");
                            if (retrycount >= 3) {
                                lblProg2.settext("Unstucking");
                                BotUtils.sysLogAppend("Moving char", "white");
                                Gob player = gui.map.player();
                                Coord location = player.rc.floor(posres);
                                int x = location.x + getrandom();
                                int y = location.y + getrandom();
                                Coord finalloc = new Coord(x, y);
                                gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
                                retrycount = 0;
                                BotUtils.sleep(1000);
                            }
                            BotUtils.sysLogAppend("Retrying harvest", "white");
                            try {
                                BotUtils.doClick(g,3,0);
                            } catch (NullPointerException qq) {
                                BotUtils.sysLogAppend("Flax I found is now null, weird. Retrying.", "white");
                                g = null;
                                while (BotUtils.findNearestStageCropPartial(5000, 3, "flax") == null)
                                    BotUtils.sleep(10);
                                g = BotUtils.findNearestStageCropPartial(5000, 3, "flax");
                                BotUtils.doClick(g,3,0);
                            }
                            retryharvest = 0;
                            retrycount++;
                        }
                    }

                    if (stopThread)
                        return;
                    BotUtils.sleep(200);

                    try {
                        while (gui.maininv.getItemPartial("Flax") == null)
                            BotUtils.sleep(10);
                        WItem flax = gui.maininv.getItemPartial("Flax");
                        GItem flax2 = flax.item;
                        java.util.List<WItem> items = gui.maininv.getIdenticalItems((flax2));
                        sort(items);
                        for (WItem seeds : items) {
                            GItem item = seeds.item;
                            if(item.quality().q > highestquality){
                                highestquality = item.quality().q;
                                lblhighestq.settext("Quality "+item.quality().q);
                            }
                            if (BotUtils.getAmount(item) >= 5) {
                                lblProg2.settext("Picking Up Seeds");
                                BotUtils.sysLogAppend("" + item.quality().q, "white");
                                BotUtils.takeItem(item);
                                break;
                            }
                        }
                        retryharvest = 0;
                        while (BotUtils.getItemAtHand() == null) {
                            lblProg2.settext("Waiting to Pickup Seeds");
                            BotUtils.sleep(10);
                            retryharvest++;
                            if(retryharvest > 500){
                                flax = gui.maininv.getItemPartial("Flax");
                                flax2 = flax.item;
                                items = gui.maininv.getIdenticalItems((flax2));
                                sort(items);
                                for (WItem seeds : items) {
                                    GItem item = seeds.item;
                                    if (BotUtils.getAmount(item) >= 5) {
                                        lblProg2.settext("Picking Up Seeds");
                                        BotUtils.takeItem(item);
                                        break;
                                    }
                                }
                            }
                        }
                        // Plant the seed from hand
                        int amount = 0;
                        if (seedname.contains("seed"))
                            amount = BotUtils.getAmount(BotUtils.getItemAtHand());
                        lblProg2.settext("Planting");
                        BotUtils.mapInteractClick(0);

                        retrycount = 0;
                        while (BotUtils.findNearestStageCropPartial(5, 0, "flax") == null || (BotUtils.getItemAtHand() != null
                                && amount == BotUtils.getAmount(BotUtils.getItemAtHand()))) {
                            retryharvest++;
                            if (retryharvest > 500) {
                                lblProg2.settext("Retry Planting");
                                BotUtils.mapInteractClick(0);
                                retryharvest = 0;
                                retrycount++;
                            }
                            if (BotUtils.getItemAtHand() != null & retrycount >= 3) {
                                BotUtils.sysLogAppend("Giving up on this replant, skipping", "white");
                                break;
                            }
                            if (BotUtils.getItemAtHand() == null && retrycount >= 3) {
                                BotUtils.sysLogAppend("Retry pickup and plant", "white");
                                lblProg2.settext("Retry Pickup Item and plant");
                                sort(items);
                                for (WItem seeds : items) {
                                    GItem item = seeds.item;
                                    if(item.quality().q > highestquality){
                                        highestquality = item.quality().q;
                                        lblhighestq.settext("Quality "+item.quality().q);
                                    }
                                    if (BotUtils.getAmount(item) >= 5) {
                                        lblProg2.settext("Picking Up Seeds");
                                        BotUtils.sysLogAppend("Replanting flax of quality : " + item.quality().q, "white");
                                        BotUtils.takeItem(item);
                                        break;
                                    }
                                }
                                BotUtils.mapInteractClick(0);
                            }
                            lblProg2.settext("Waiting for Planting Complete");
                            BotUtils.sleep(10);
                        }
                        retrycount = 0;


// Merge seed from hand into inventory or put it in inventory
                        //commented out at request to prevent mixing high and low q seeds
                        if (gui.maininv.getItemPartial("Flax") != null && BotUtils.getItemAtHand() != null && BotUtils.getAmount(BotUtils.getItemAtHand()) != 50) {
                            flax = gui.maininv.getItemPartial("Flax");
                            items = gui.maininv.getIdenticalItems((flax.item));
                            for (WItem seedslol : items) {
                                if (BotUtils.getAmount(seedslol.item) < 50)
                                    continue;
                                if (BotUtils.getAmount(BotUtils.getItemAtHand()) == 50)
                                    break;
                                if (seedslol.item.quality().q == BotUtils.getItemAtHand().quality().q) {
                                    System.out.println("Combining quality : " + BotUtils.getItemAtHand().quality().q + " with quality : " + seedslol.item.quality().q + " seeds.");
                                    int handAmount = BotUtils.getAmount(BotUtils.getItemAtHand());
                                    try {
                                        seedslol.item.wdgmsg("itemact", 0);
                                    } catch (Exception e) {
                                    }
                                    while (BotUtils.getItemAtHand() != null && BotUtils.getAmount(BotUtils.getItemAtHand()) == handAmount)
                                        BotUtils.sleep(50);
                                    break;
                                }
                            }
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
                            Gob barrel = BotUtils.findNearestBarrel(5000,blacklist);
                            barrel.delattr(GobHighlight.class);
                            barrel.setattr(new GobHighlight(barrel));
                            flax = gui.maininv.getItemPartial("Flax");
                            flax2 = flax.item;
                            items = gui.maininv.getIdenticalItems((flax2));
                            sort(items);
                            if (BotUtils.getItemAtHand() != null)
                                BotUtils.dropItem(0);
                            BotUtils.pfRightClick(barrel, 0);
                            BotUtils.waitForWindow((Resource.getLocString(Resource.BUNDLE_WINDOW, "Barrel")));
                            if (BotUtils.getItemAtHand() != null) {
                                gameui().map.wdgmsg("itemact", Coord.z, barrel.rc.floor(posres), 0, 0, (int) barrel.id,
                                        barrel.rc.floor(posres), 0, -1);
                                int i = 0;
                                while (BotUtils.getItemAtHand() != null) {
                                    if (i > 250) {
                                       BotUtils.sysLogAppend("Blacklisting barrel, appears to be full","white");
                                       blacklist.add(barrel);
                                       barrel = BotUtils.findNearestBarrel(2000, blacklist);
                                       BotUtils.sleep(500);
                                        if (BotUtils.getItemAtHand() != null) {
                                            lblProg2.settext("Dropping Seeds to Inv");
                                            Coord slot = BotUtils.getFreeInvSlot(BotUtils.playerInventory());
                                            if (slot != null) {
                                                BotUtils.dropItemToInventory(slot, BotUtils.playerInventory());
                                                while (BotUtils.getItemAtHand() != null)
                                                    BotUtils.sleep(50);
                                            }
                                        }
                                       break;
                                    }
                                    BotUtils.sleep(10);
                                    i++;
                                }
                            }
                            // while (BotUtils.getInventoryItemsByNames(BotUtils.playerInventory(), Arrays.asList(seedname)).size() != 0) {
                            items.subList(0, 14).clear();
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
                                    if (i > 250) {
                                        BotUtils.sysLogAppend("Blacklisting barrel, appears to be full","white");
                                        blacklist.add(barrel);
                                        Coord slot = BotUtils.getFreeInvSlot(BotUtils.playerInventory());
                                        BotUtils.dropItemToInventory(slot,BotUtils.playerInventory());
                                        BotUtils.sleep(250);
                                        barrel = BotUtils.findNearestBarrel(2000,blacklist);
                                        BotUtils.pfRightClick(barrel, 0);
                                        int retryclick = 0;
                                        while(gui.getwnd((Resource.getLocString(Resource.BUNDLE_WINDOW, "Barrel"))) == null){
                                            if(retryclick > 200){
                                                retryclick = 0;
                                                BotUtils.pfRightClick(barrel,0);
                                            }
                                            retryclick++;
                                            BotUtils.sleep(10);
                                        }
                                        break;
                                    }
                                    BotUtils.sleep(10);
                                    i++;
                                }
                            }
                        }

                    } catch (NullPointerException x) {
                        BotUtils.sysLogAppend("Null pointer exception caught, crash prevented.","white");
                    }
                    g = null;
                    cropsHarvested++;
                    lblProg.settext(cropsHarvested + " Units Harvested");
                }catch(Loading | Sprite.ResourceException | NullPointerException e){}
            }
        }
    }


    public int getrandom(){
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
        runner.interrupt();
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


