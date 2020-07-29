package haven.purus;

import haven.Button;
import haven.Coord;
import haven.Frame;
import haven.GOut;
import haven.Gob;
import haven.Label;
import haven.Loading;
import haven.WItem;
import haven.Widget;
import haven.Window;
import haven.automation.GobSelectCallback;
import haven.purus.pbot.PBotUtils;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collections;

import static haven.OCache.posres;

public class StockpileFiller extends Window implements GobSelectCallback, ItemClickCallback {

    private ArrayList<Gob> stockpiles = new ArrayList<Gob>();
    private String invobj, terobj;

    private boolean stop = false;
    private boolean terobjCallback = false;
    private Button clearBtn, startBtn;

    public StockpileFiller() {
        super(new Coord(270, 200), "Stockpile Filler");

        Widget inf = add(new Widget(new Coord(245, 30)) {
            public void draw(GOut g) {
                g.chcolor(0, 0, 0, 128);
                g.frect(Coord.z, sz);
                g.chcolor();
                super.draw(g);
            }

        }, new Coord(10, 10).add(wbox.btloff()));
        Frame.around(this, Collections.singletonList(inf));
        Label infolbl = inf.add(new Label("Alt + Click to select stockpiles"), new Coord(5, 0));
        int y = 40;
        Button invobjBtn = new Button(140, "Choose item from inventory") {
            @Override
            public void click() {
                PBotUtils.sysMsg(ui, "Click the stockpile item in inventory", Color.GREEN);
                registerItemCallback();
            }
        };
        add(invobjBtn, new Coord(20, y));
        y += 35;
        Button terobjBtn = new Button(140, "Choose item from ground") {
            @Override
            public void click() {
                terobjCallback = true;
                PBotUtils.sysMsg(ui, "Alt + Click to select ground item", Color.GREEN);
            }
        };
        add(terobjBtn, new Coord(20, y));
        y += 35;
        clearBtn = new Button(140, "Clear/Stop") {
            @Override
            public void click() {
                startBtn.show();
                stop = true;
                if (t != null)
                    t.interrupt();
                stockpiles.clear();
                PBotUtils.sysMsg(ui, "Cleared the list of selected stockpiles", Color.GREEN);
            }
        };
        add(clearBtn, new Coord(20, y));
        y += 35;
        startBtn = new Button(140, "Start") {
            @Override
            public void click() {
                this.hide();
                stop = false;
                if (stockpiles.isEmpty()) {
                    PBotUtils.sysMsg(ui, "No stockpiles chosen!", Color.GREEN);
                } else if (terobj == null) {
                    PBotUtils.sysMsg(ui, "No ground item chosen!", Color.GREEN);
                } else if (invobj == null) {
                    PBotUtils.sysMsg(ui, "No inventory item chosen!", Color.GREEN);
                } else {
                    t.start();
                }
            }
        };
        add(startBtn, new Coord(20, y));
        y += 35;
    }

    Thread t = new Thread(new Runnable() {
        public void run() {
            main:
            try {
                while (PBotUtils.findObjectByNames(ui, 5000, terobj) != null) {
                    System.out.println("In main loop");
                    if (stop)
                        break;
                    while (PBotUtils.getItemAtHand(ui) == null) {
                        if (stop)
                            break;
                        if (PBotUtils.findObjectByNames(ui, 5000, terobj) == null) {
                            PBotUtils.sysLogAppend(ui, "Out of items to stockpile, finishing.", "white");
                            stop = true;
                            break;
                        }
                        PBotUtils.sysLogAppend(ui, "Grabbing stuff.", "white");
                        Gob g = PBotUtils.findObjectByNames(ui, 5000, terobj);
                        ui.gui.map.wdgmsg("click", g.sc, g.rc.floor(posres), 3, 1, 0, (int) g.id, g.rc.floor(posres), 0, -1);
                        PBotUtils.sleep(1000);

                        while (PBotUtils.getItemAtHand(ui) == null & PBotUtils.findObjectByNames(ui, 5000, terobj) != null && PBotUtils.isMoving(ui)) {
                            System.out.println("waiting for item on  hand");
                            PBotUtils.sleep(10);
                        }
                        System.out.println("inv free slots : " + PBotUtils.invFreeSlots(ui));
                    }

                    PBotUtils.sysLogAppend(ui, "Done Grabbing stuff.", "white");
                    if (stop)
                        break;
                    while (PBotUtils.getInventoryItemsByName(ui.gui.maininv, invobj).size() != 0 && !stop) {
                        if (stop)
                            break;
                        System.out.println("In stockpile loop");
                        PBotUtils.sleep(1000);
                        if (PBotUtils.getItemAtHand(ui) != null)
                            PBotUtils.dropItem(ui, 0);
                        if (stockpiles.isEmpty()) {
                            System.out.println("Stockpiles empty");
                            PBotUtils.sysMsg(ui, "All chosen stockpiles full!", Color.GREEN);
                            stop = true;
                            break;
                        }

                        if (PBotUtils.stockpileIsFull(PBotUtils.findObjectById(ui, stockpiles.get(0).id))) {
                            System.out.println("Stockpile full");
                            stockpiles.remove(0);
                            continue;
                        }
                        if (stop)
                            break;
                        if (stockpiles.size() == 0) {
                            PBotUtils.sysMsg(ui, "Stockpile list now empty, stopping.", Color.white);
                            stop = true;
                            stop();
                        }
                        PBotUtils.pfRightClick(ui, stockpiles.get(0), 0);
                        int retry = 0;
                        while (ui.gui.getwnd("Stockpile") == null) {
                            if (!PBotUtils.isMoving(ui))
                                retry++;
                            if (retry > 100) {
                                if (stop)
                                    break;
                                retry = 0;
                                System.out.println("Retry : " + retry);
                                PBotUtils.sysLogAppend(ui, "Retrying stockpile interaction", "white");
                                PBotUtils.dropItem(ui, 0);
                                PBotUtils.pfRightClick(ui, stockpiles.get(0), 0);
                            }
                            PBotUtils.sleep(10);
                        }
                        PBotUtils.sleep(1000);
                        System.out.println("clicking stockpile");
                        try {
                            while (PBotUtils.getItemAtHand(ui) == null)
                                PBotUtils.takeItem(ui, PBotUtils.getInventoryItemsByName(ui.gui.maininv, invobj).get(0).item);
                        } catch (NullPointerException q) {
                            //break on null pointer here, bot is prob done
                            stop = true;
                            break main;
                        }
                        int cnt = PBotUtils.invFreeSlots(ui);
                        try {
                            ui.gui.map.wdgmsg("itemact", Coord.z, stockpiles.get(0).rc.floor(posres), 1, 0, (int) stockpiles.get(0).id, stockpiles.get(0).rc.floor(posres), 0, -1);
                        } catch (IndexOutOfBoundsException lolindexes) {
                            PBotUtils.sysMsg(ui, "Critical error in stockpile list, stopping thread to prevent crash.", Color.white);
                            stop = true;
                            stop();
                        }
                        while (PBotUtils.invFreeSlots(ui) == cnt) {
                            System.out.println("waiting for inv update");
                            PBotUtils.sleep(100);
                        }
                    }
                    if (PBotUtils.findObjectByNames(ui, 5000, terobj) == null)
                        break;
                }
                PBotUtils.sysMsg(ui, "Stockpile Filler finished!", Color.GREEN);
                startBtn.show();
                reqdestroy();
            } catch (Loading | NullPointerException q) {
            }
        }
    });

    private void registerItemCallback() {
        synchronized (GobSelectCallback.class) {
            ui.gui.registerItemCallback(this);
        }
    }

    @Override
    public void gobselect(Gob gob) {
        if (terobjCallback) {
            terobjCallback = false;
            terobj = gob.getres().name;
            PBotUtils.sysMsg(ui, "Ground item chosen!", Color.GREEN);
        } else if (gob.getres().basename().contains("stockpile")) {
            stockpiles.add(gob);
            PBotUtils.sysMsg(ui, "Stockpile added to list! Total stockpiles chosen: " + stockpiles.size(), Color.GREEN);
        }
        synchronized (GobSelectCallback.class) {
            ui.gui.map.registerGobSelect(this);
        }
    }

    @Override
    public void itemClick(WItem item) {
        invobj = item.item.getres().name;
        PBotUtils.sysMsg(ui, "Inventory item to put in the stockpiles chosen!", Color.GREEN);
        synchronized (ItemClickCallback.class) {
            ui.gui.unregisterItemCallback();
        }
    }

    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (msg == "close") {
            stop();
            reqdestroy();
        } else
            super.wdgmsg(sender, msg, args);
    }

    public void stop() {
        t.interrupt();
        stop = true;
        synchronized (ItemClickCallback.class) {
            ui.gui.unregisterItemCallback();
        }
        synchronized (ItemClickCallback.class) {
            ui.gui.unregisterItemCallback();
        }
        reqdestroy();
        //ui.gui.map.wdgmsg("click", Coord.z, new Coord((int)BotUtils.player().rc.x, (int)BotUtils.player().rc.y), 1, 0);
    }
}