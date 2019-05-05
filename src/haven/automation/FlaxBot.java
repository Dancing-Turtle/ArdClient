package haven.automation;


import haven.*;
import haven.Button;
import haven.Label;
import haven.Window;
import haven.purus.SeedCropFarmer;

import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;
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
            PBotUtils.sysMsg("Flax Bot Started!", Color.white);
            lblProg.settext(cropsHarvested + " Units Harvested");
            lblProg2.settext(cropsHarvested + "Starting");
            GameUI gui = HavenPanel.lui.root.findchild(GameUI.class);
            while (!stopThread || gui.getwnd("Flax Farmer") != null) {
                try {
                    lblProg.settext(cropsHarvested + " Units Harvested");
                    IMeter.Meter stam = gui.getmeter("stam", 0);
                    if (stam.a <= 30) {
                        lblProg2.settext("Drinking");
                        PBotUtils.drink(true);
                    }

                    if(stopThread || gui.getwnd("Flax Farmer") == null)
                        return;

                    while (PBotUtils.findNearestStageCrop(5000, 3, "gfx/terobjs/plants/flax") == null) {
                        if(stopThread || gui.getwnd("Flax Farmer") == null)
                            return;
                        lblProg2.settext("No Flax");
                        PBotUtils.sleep(200);
                    }
                    while (g == null) {
                        lblProg2.settext("Found Flax");
                        if(stopThread || gui.getwnd("Flax Farmer") == null)
                            return;
                        g = PBotUtils.findNearestStageCrop(5000, 3, "gfx/terobjs/plants/flax");
                    }

                    PBotUtils.doClick(g, 1, 0);

                    int retryharvest = 0;
                    int retrycount = 0;
                    PBotAPI.gui.map.wdgmsg("click", Coord.z, g.rc.floor(posres), 1, 0);
                    while (PBotUtils.player().rc.x != g.rc.x || PBotUtils.player().rc.y != g.rc.y) {
                        if(stopThread || gui.getwnd("Flax Farmer") == null)
                            return;
                        if(!PBotUtils.isMoving())
                        retryharvest++;
                        lblProg2.settext("Moving to Crop");
                        if (retryharvest >= 500) {
                            lblProg2.settext("Retry Movement");
                            PBotUtils.sysLogAppend("Moving char in move loop", "white");
                            Gob player = gui.map.player();
                            Coord location = player.rc.floor(posres);
                            int x = location.x + getrandom();
                            int y = location.y + getrandom();
                            Coord finalloc = new Coord(x, y);
                            gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
                            retryharvest = 0;
                            PBotUtils.sleep(1000);
                            PBotAPI.gui.map.wdgmsg("click", Coord.z, g.rc.floor(posres), 1, 0);
                        }
                        PBotUtils.sleep(10);
                    }
                    lblProg2.settext("Harvesting");
                    try {
                        PBotUtils.pfRightClick(g, 0);
                    } catch (NullPointerException qq) {
                        PBotUtils.sysLogAppend("Flax I found is now null, weird. Retrying.", "white");
                        g = null;
                        while (PBotUtils.findNearestStageCrop(5000, 3, "gfx/terobjs/plants/flax") == null)
                            PBotUtils.sleep(10);
                        g = PBotUtils.findNearestStageCrop(5000, 3, "gfx/terobjs/plants/flax");
                        PBotUtils.pfRightClick(g, 0);
                    }

                    // Wait for harvest menu to appear and harvest the crop
                    while (ui.root.findchild(FlowerMenu.class) == null) {
                        lblProg2.settext("Waiting for Flowermenu");
                        if(stopThread || gui.getwnd("Flax Farmer") == null)
                            return;
                        retryharvest++;
                        PBotUtils.sleep(10);
                        if(PBotUtils.getItemAtHand() != null){
                            Coord slot = PBotUtils.getFreeInvSlot(PBotAPI.gui.maininv);
                            PBotUtils.dropItemToInventory(slot,PBotAPI.gui.maininv);
                            while(PBotUtils.getItemAtHand() != null)
                                PBotUtils.sleep(50);
                        }
                        if (retryharvest >= 500) {
                            lblProg2.settext("Retry Harvest");
                            if (retrycount >= 3) {
                                lblProg2.settext("Unstucking");
                                PBotUtils.sysLogAppend("Moving char", "white");
                                Gob player = gui.map.player();
                                Coord location = player.rc.floor(posres);
                                int x = location.x + getrandom();
                                int y = location.y + getrandom();
                                Coord finalloc = new Coord(x, y);
                                gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
                                retrycount = 0;
                                PBotUtils.sleep(1000);
                                PBotUtils.pfRightClick(g, 0);
                            }
                            PBotUtils.sysLogAppend("Retrying harvest", "white");
                            lblProg2.settext("Retrying Harvest");
                            try {
                                PBotUtils.doClick(g,3,0);
                            } catch (NullPointerException qq) {
                                PBotUtils.sysLogAppend("Flax I found is now null, weird. Retrying.", "white");
                                g = null;
                                while (PBotUtils.findNearestStageCrop(5000, 3, "gfx/terobjs/plants/flax") == null)
                                    PBotUtils.sleep(10);
                                g = PBotUtils.findNearestStageCrop(5000, 3, "gfx/terobjs/plants/flax");
                                PBotUtils.doClick(g,3,0);
                            }
                            retryharvest = 0;
                            retrycount++;
                        }

                        if(stopThread || gui.getwnd("Flax Farmer") == null)
                            return;
                    }

                    if(stopThread || gui.getwnd("Flax Farmer") == null)
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
                    while (PBotUtils.findObjectById(g.id) != null) {
                        lblProg2.settext("Waiting for Harvest");
                        if(stopThread || gui.getwnd("Flax Farmer") == null)
                            return;
                        retryharvest++;
                        PBotUtils.sleep(10);
                        if (retryharvest >= 500) {
                            lblProg2.settext("Retry Harvest");
                            if (retrycount >= 3) {
                                lblProg2.settext("Unstucking");
                                PBotUtils.sysLogAppend("Moving char", "white");
                                Gob player = gui.map.player();
                                Coord location = player.rc.floor(posres);
                                int x = location.x + getrandom();
                                int y = location.y + getrandom();
                                Coord finalloc = new Coord(x, y);
                                gameui().map.wdgmsg("click", Coord.z, finalloc, 1, 0);
                                retrycount = 0;
                                PBotUtils.sleep(1000);
                            }
                            PBotUtils.sysLogAppend("Retrying harvest", "white");
                            try {
                                PBotUtils.doClick(g,3,0);
                            } catch (NullPointerException qq) {
                                PBotUtils.sysLogAppend("Flax I found is now null, weird. Retrying.", "white");
                                g = null;
                                while (PBotUtils.findNearestStageCrop(5000, 3, "gfx/terobjs/plants/flax") == null)
                                    PBotUtils.sleep(10);
                                g = PBotUtils.findNearestStageCrop(5000, 3, "gfx/terobjs/plants/flax");
                                PBotUtils.doClick(g,3,0);
                            }
                            retryharvest = 0;
                            retrycount++;
                        }
                    }

                    if(stopThread || gui.getwnd("Flax Farmer") == null)
                        return;
                    PBotUtils.sleep(200);

                    try {
                        while (gui.maininv.getItemPartial("Flax") == null)
                            PBotUtils.sleep(10);
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
                            if (PBotUtils.getAmount(item) >= 5) {
                                lblProg2.settext("Picking Up Seeds");
                            //    PBotUtils.sysLogAppend("" + item.quality().q, "white");
                                PBotUtils.takeItem(item);
                                break;
                            }
                        }
                        retryharvest = 0;
                        while (PBotUtils.getItemAtHand() == null) {
                            lblProg2.settext("Waiting to Pickup Seeds");
                            PBotUtils.sleep(10);
                            retryharvest++;
                            if(retryharvest > 500){
                                flax = gui.maininv.getItemPartial("Flax");
                                flax2 = flax.item;
                                items = gui.maininv.getIdenticalItems((flax2));
                                sort(items);
                                for (WItem seeds : items) {
                                    GItem item = seeds.item;
                                    if (PBotUtils.getAmount(item) >= 5) {
                                        lblProg2.settext("Picking Up Seeds");
                                        PBotUtils.takeItem(item);
                                        break;
                                    }
                                }
                            }
                        }
                        // Plant the seed from hand
                        int amount;
                        amount = PBotUtils.getAmount(PBotUtils.getGItemAtHand());
                        lblProg2.settext("Planting");
                       // PBotUtils.mapInteractClick();
                        gui.map.wdgmsg("itemact", Coord.z, PBotUtils.player().rc.floor(posres), 0, 0, (int) PBotUtils.player().id, PBotUtils.player().rc.floor(posres), 0, -1);
                        retrycount = 0;
                        while (PBotUtils.findNearestStageCrop(5, 0, "gfx/terobjs/plants/flax") == null || (PBotUtils.getItemAtHand() != null
                                && amount == PBotUtils.getAmount(PBotUtils.getGItemAtHand()))) {
                            if(stopThread || gui.getwnd("Flax Farmer") == null)
                                return;
                            retryharvest++;
                            if (retryharvest > 500) {
                                lblProg2.settext("Retry Planting");
                              //  PBotUtils.mapInteractClick();
                                gui.map.wdgmsg("itemact", Coord.z, PBotUtils.player().rc.floor(posres), 0, 0, (int) PBotUtils.player().id, PBotUtils.player().rc.floor(posres), 0, -1);
                                retryharvest = 0;
                                retrycount++;
                            }
                            if (PBotUtils.getItemAtHand() != null & retrycount >= 3) {
                                PBotUtils.sysLogAppend("Giving up on this replant, skipping", "white");
                                break;
                            }
                            if (PBotUtils.getItemAtHand() == null && retrycount >= 3) {
                                PBotUtils.sysLogAppend("Retry pickup and plant", "white");
                                lblProg2.settext("Retry Pickup Item and plant");
                                sort(items);
                                for (WItem seeds : items) {
                                    GItem item = seeds.item;
                                    if(item.quality().q > highestquality){
                                        highestquality = item.quality().q;
                                        lblhighestq.settext("Quality "+item.quality().q);
                                    }
                                    if (PBotUtils.getAmount(item) >= 5) {
                                        lblProg2.settext("Picking Up Seeds");
                                        PBotUtils.sysLogAppend("Replanting flax of quality : " + item.quality().q, "white");
                                        PBotUtils.takeItem(item);
                                        break;
                                    }
                                }
                              //  PBotUtils.mapInteractClick();
                                gui.map.wdgmsg("itemact", Coord.z, PBotUtils.player().rc.floor(posres), 0, 0, (int) PBotUtils.player().id, PBotUtils.player().rc.floor(posres), 0, -1);
                            }
                            lblProg2.settext("Waiting for Planting Complete");
                            PBotUtils.sleep(10);
                        }
                        retrycount = 0;


// Merge seed from hand into inventory or put it in inventory
                        //commented out at request to prevent mixing high and low q seeds
                        if (gui.maininv.getItemPartial("Flax") != null && PBotUtils.getItemAtHand() != null && PBotUtils.getAmount(PBotUtils.getGItemAtHand()) != 50) {
                            flax = gui.maininv.getItemPartial("Flax");
                            items = gui.maininv.getIdenticalItems((flax.item));
                            for (WItem seedslol : items) {
                                if (PBotUtils.getAmount(seedslol.item) < 50)
                                    continue;
                                if (PBotUtils.getAmount(PBotUtils.getGItemAtHand()) == 50)
                                    break;
                                if (seedslol.item.quality().q == PBotUtils.getGItemAtHand().quality().q) {
                                    System.out.println("Combining quality : " + PBotUtils.getGItemAtHand().quality().q + " with quality : " + seedslol.item.quality().q + " seeds.");
                                    int handAmount = PBotUtils.getAmount(PBotUtils.getGItemAtHand());
                                    try {
                                        seedslol.item.wdgmsg("itemact", 0);
                                    } catch (Exception e) {
                                    }
                                    while (PBotUtils.getItemAtHand() != null && PBotUtils.getAmount(PBotUtils.getGItemAtHand()) == handAmount) {
                                        if(stopThread || gui.getwnd("Flax Farmer") == null)
                                            return;
                                        PBotUtils.sleep(50);
                                    }
                                    break;
                                }
                            }
                        }

                        if (PBotUtils.getItemAtHand() != null) {
                            lblProg2.settext("Dropping Seeds to Inv");
                            Coord slot = PBotUtils.getFreeInvSlot(PBotAPI.gui.maininv);
                            if (slot != null) {
                                int freeSlots = PBotUtils.invFreeSlots();
                                PBotUtils.dropItemToInventory(slot, PBotAPI.gui.maininv);
                                while (PBotUtils.getItemAtHand() != null)
                                    PBotUtils.sleep(50);
                            }
                        }
                        if (PBotUtils.invFreeSlots() < 3) {
                            if(stopThread || gui.getwnd("Flax Farmer") == null)
                                return;
                            lblProg2.settext("Barreling");
                            Gob barrel = PBotUtils.findNearestBarrel(5000,blacklist);
                            barrel.delattr(GobHighlight.class);
                            barrel.setattr(new GobHighlight(barrel));
                            flax = gui.maininv.getItemPartial("Flax");
                            flax2 = flax.item;
                            items = gui.maininv.getIdenticalItems((flax2));
                            sort(items);
                            if (PBotUtils.getItemAtHand() != null)
                                PBotUtils.dropItem(0);
                            PBotUtils.pfRightClick(barrel, 0);
                            PBotUtils.waitForWindow((Resource.getLocString(Resource.BUNDLE_WINDOW, "Barrel")));
                            if (PBotUtils.getItemAtHand() != null) {
                                gameui().map.wdgmsg("itemact", Coord.z, barrel.rc.floor(posres), 0, 0, (int) barrel.id,
                                        barrel.rc.floor(posres), 0, -1);
                                int i = 0;
                                while (PBotUtils.getItemAtHand() != null) {
                                    if (i > 250) {
                                        PBotUtils.sysLogAppend("Blacklisting barrel, appears to be full","white");
                                       blacklist.add(barrel);
                                       barrel = PBotUtils.findNearestBarrel(2000, blacklist);
                                        PBotUtils.sleep(500);
                                        if (PBotUtils.getItemAtHand() != null) {
                                            lblProg2.settext("Dropping Seeds to Inv");
                                            Coord slot = PBotUtils.getFreeInvSlot(PBotAPI.gui.maininv);
                                            if (slot != null) {
                                                PBotUtils.dropItemToInventory(slot, PBotAPI.gui.maininv);
                                                while (PBotUtils.getItemAtHand() != null)
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
                                PBotUtils.takeItem(item);

                                gameui().map.wdgmsg("itemact", Coord.z, barrel.rc.floor(posres), 0, 0, (int) barrel.id,
                                        barrel.rc.floor(posres), 0, -1);
                                int i = 0;
                                while (PBotUtils.getItemAtHand() != null) {
                                    if (i > 250) {
                                        PBotUtils.sysLogAppend("Blacklisting barrel, appears to be full","white");
                                        blacklist.add(barrel);
                                        Coord slot = PBotUtils.getFreeInvSlot(PBotAPI.gui.maininv);
                                        PBotUtils.dropItemToInventory(slot,PBotAPI.gui.maininv);
                                        PBotUtils.sleep(250);
                                        barrel = PBotUtils.findNearestBarrel(2000,blacklist);
                                        PBotUtils.pfRightClick(barrel, 0);
                                        int retryclick = 0;
                                        while(gui.getwnd((Resource.getLocString(Resource.BUNDLE_WINDOW, "Barrel"))) == null){
                                            if(retryclick > 200){
                                                retryclick = 0;
                                                PBotUtils.pfRightClick(barrel,0);
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
                        PBotUtils.sysLogAppend("Null pointer exception caught, crash prevented.","white");
                    }
                    g = null;
                    cropsHarvested++;
                    lblProg.settext(cropsHarvested + " Units Harvested");
                }catch(Loading | Sprite.ResourceException | NullPointerException e){e.printStackTrace();}
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
        PBotUtils.sysMsg("Flax Farmer stopped!", Color.white);
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


