package haven.purus;

import haven.*;
import haven.automation.GobSelectCallback;
import haven.purus.pbot.PBotUtils;

import java.awt.Color;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static haven.OCache.posres;

public class TroughFiller extends Window implements GobSelectCallback {
    private Gob trough;

    private List<String> invobjs = Arrays.asList("gfx/invobjs/carrot", "gfx/invobjs/beet", "gfx/invobjs/beetleaves", "gfx/invobjs/pumpkin", "gfx/invobjs/turnip");
    private String[] terobjs = {"gfx/terobjs/items/carrot", "gfx/terobjs/items/beet", "gfx/terobjs/items/beetleaves", "gfx/terobjs/items/pumpkin", "gfx/terobjs/items/turnip"};

    private boolean stop = false;

    private Button stopBtn;
    UI ui;

    public TroughFiller(UI ui) {
        super(new Coord(270, 100), "Trough Filler");
        this.ui = ui;

        Widget inf = add(new Widget(new Coord(245, 30)) {
            public void draw(GOut g) {
                g.chcolor(0, 0, 0, 128);
                g.frect(Coord.z, sz);
                g.chcolor();
                super.draw(g);
            }

        }, new Coord(10, 10).add(wbox.btloff()));
        Frame.around(this, Collections.singletonList(inf));
        Label infolbl = inf.add(new Label("Alt + Click to select trough/compost bin"), new Coord(5, 0));
        stopBtn = new Button(120, "Stop") {
            @Override
            public void click() {
                stop();
            }
        };
        add(stopBtn, new Coord(75, 65));
    }

    Thread t = new Thread(new Runnable() {
        public void run() {

            main:
            while (!stop) {
                try {

                    if (stop) {
                        System.out.println("stop detected ,breaking");
                        return;
                    }
                    System.out.println("Main loop start");
                    int timeout = 0;
                    while (PBotUtils.findObjectByNames(ui, 5000, terobjs) == null) {
                        System.out.println("Waiting to detect objects");
                        timeout++;
                        if (stop) {
                            stop = true;
                            break main;
                        }
                        if (timeout >= 100) {
                            System.out.println("Timeout expired, stopping");
                            stop();
                            break;
                        }
                        PBotUtils.sleep(50);
                    }
                    if (PBotUtils.findObjectByNames(ui, 5000, terobjs) == null && PBotUtils.getInventoryItemsByNames(ui.gui.maininv, invobjs).size() == 0) {
                        System.out.println("No objects on ground or in inv, breaking.");
                        break;
                    }
                    while (PBotUtils.getItemAtHand(ui) == null) {
                        System.out.println("Waiting for item on cursor");
                        if (stop) {
                            System.out.println("Stop detected, breaking.");
                            stop = true;
                            break main;
                        }


                        if (PBotUtils.findObjectByNames(ui, 5000, terobjs) == null) {
                            System.out.println("No objects on ground, breaking.");
                            break;
                        }

                        Gob g = PBotUtils.findObjectByNames(ui, 5000, terobjs);
                        ui.gui.map.wdgmsg("click", g.sc, g.rc.floor(posres), 3, 1, 0, (int) g.id, g.rc.floor(posres), 0, -1);
                        PBotUtils.sleep(1000);
                        while (PBotUtils.getItemAtHand(ui) == null & PBotUtils.findObjectByNames(ui, 5000, terobjs) != null && PBotUtils.isMoving(ui))
                            PBotUtils.sleep(10);
                    }

                    if (stop) {
                        System.out.println("stop detected ,breaking");
                        return;
                    }

                    PBotUtils.sleep(500);

                    if (PBotUtils.getItemAtHand(ui) != null)
                        PBotUtils.dropItem(ui, 0);

                    PBotUtils.pfRightClick(ui, trough, 0);
                    int retry = 0;
                    while (ui.gui.getwnd("Trough") == null && ui.gui.getwnd("Compost Bin") == null) {
                        retry++;
                        if (retry > 500) {
                            retry = 0;
                            PBotUtils.sysLogAppend(ui, "Retrying trough/Compost Bin interaction", "white");
                            PBotUtils.dropItem(ui, 0);
                            PBotUtils.pfRightClick(ui, trough, 0);
                        }
                        PBotUtils.sleep(10);
                    }
                    while (PBotUtils.getInventoryItemsByNames(ui.gui.maininv, invobjs).size() > 0 && !stop) {

                        if (stop) {
                            System.out.println("stop detected ,breaking");
                            return;
                        }
                        if (PBotUtils.getItemAtHand(ui) == null) {
                            GItem item = PBotUtils.getInventoryItemsByNames(ui.gui.maininv, invobjs).get(0).item;
                            PBotUtils.takeItem(ui, item);
                            PBotUtils.sleep(100);
                        }
                        if (PBotUtils.getItemAtHand(ui) == null) {
                            System.out.println("Hand null, breaking");
                            break;
                        }
                        List<WItem> list = PBotUtils.getInventoryItemsByNames(ui.gui.maininv, invobjs);
                        ui.gui.map.wdgmsg("itemact", Coord.z, trough.rc.floor(posres), 1, 0, (int) trough.id, trough.rc.floor(posres), 0, -1);
                        int i = 0;
                        while (PBotUtils.getInventoryItemsByNames(ui.gui.maininv, invobjs).size() == list.size()) {
                            if (i > 500) {
                                System.out.println("Trough appears to be full, breaking.");
                                break;
                            }
                            PBotUtils.sleep(10);
                            i++;
                        }
                    }
                    PBotUtils.sleep(250);
                    if (PBotUtils.getItemAtHand(ui) != null)
                        ui.gui.map.wdgmsg("itemact", Coord.z, trough.rc.floor(posres), 0, 0, (int) trough.id, trough.rc.floor(posres), 0, -1);
                } catch (Loading | Resource.LoadException | NullPointerException idklol) {
                    PBotUtils.sysLogAppend(ui, "Error captured in main thread.", "white");
                }
            }
            PBotUtils.sysMsg(ui, "Trough Filler finished", Color.WHITE);
            stop = true;
            stop();
        }
    });

    @Override
    public void gobselect(Gob gob) {
        if (gob.getres().basename().contains("trough") || gob.getres().basename().contains("compostbin")) {
            trough = gob;
            t.start();
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
        PBotUtils.sysMsg(ui, "Stopping Trough Filler", Color.white);
        this.destroy();
    }
}