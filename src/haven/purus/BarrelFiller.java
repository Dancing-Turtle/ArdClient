package haven.purus;

import haven.Coord;
import haven.Frame;
import haven.GItem;
import haven.GOut;
import haven.Gob;
import haven.Label;
import haven.Widget;
import haven.Window;
import haven.automation.GobSelectCallback;
import haven.purus.pbot.PBotUtils;

import java.awt.Color;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static haven.OCache.posres;

public class BarrelFiller extends Window implements GobSelectCallback {

    private Gob barrel;

    private List<String> invobjs = Arrays.asList("gfx/invobjs/seed-barley", "gfx/invobjs/seed-wheat", "gfx/invobjs/seed-poppy", "gfx/invobjs/seed-flax", "gfx/invobjs/seed-hemp");
    private String[] terobjs = {"gfx/terobjs/items/seeds"};

    private boolean stop = false;

    public BarrelFiller() {
        super(new Coord(270, 50), "Barrel Filler");

        Widget inf = add(new Widget(new Coord(245, 30)) {
            public void draw(GOut g) {
                g.chcolor(0, 0, 0, 128);
                g.frect(Coord.z, sz);
                g.chcolor();
                super.draw(g);
            }

        }, new Coord(10, 10).add(wbox.btloff()));
        Frame.around(this, Collections.singletonList(inf));
        Label infolbl = inf.add(new Label("Alt + Click to select a barrel"), new Coord(5, 0));
    }

    Thread t = new Thread(new Runnable() {
        public void run() {
            main:
            while (true) {
                if (PBotUtils.findObjectByNames(ui, 1000, terobjs) == null
                        && PBotUtils.getInventoryItemsByNames(ui.gui.maininv, invobjs).size() == 0)
                    break;
                while (PBotUtils.invFreeSlots(ui) > 0) {
                    if (stop)
                        break main;
                    if (PBotUtils.findObjectByNames(ui, 1000, terobjs) == null)
                        break;

                    Gob g = PBotUtils.findObjectByNames(ui, 1000, terobjs);
                    PBotUtils.pfRightClick(ui, g, 0);
                    int i = 0;
                    while (PBotUtils.findObjectById(ui, g.id) != null) {
                        if (i == 100)
                            break;
                        PBotUtils.sleep(100);
                        i++;
                    }
                }

                if (stop)
                    break main;
                if (PBotUtils.getItemAtHand(ui) != null)
                    PBotUtils.dropItem(ui, 0);
                PBotUtils.pfRightClick(ui, barrel, 0);
                PBotUtils.waitForWindow(ui, "Barrel");

                while (PBotUtils.getInventoryItemsByNames(ui.gui.maininv, invobjs).size() != 0) {
                    if (stop)
                        break main;
                    GItem item = PBotUtils.getInventoryItemsByNames(ui.gui.maininv, invobjs).get(0).item;
                    PBotUtils.takeItem(ui, item);

                    ui.gui.map.wdgmsg("itemact", Coord.z, barrel.rc.floor(posres), 0, 0, (int) barrel.id,
                            barrel.rc.floor(posres), 0, -1);
                    int i = 0;
                    while (PBotUtils.getItemAtHand(ui) != null) {
                        if (i == 60000)
                            break main;
                        PBotUtils.sleep(10);
                        i++;
                    }
                }

            }
            PBotUtils.sysMsg(ui, "Barrel Filler finished", Color.WHITE);
            reqdestroy();
        }
    });

    @Override
    public void gobselect(Gob gob) {
        if (gob.getres().basename().contains("barrel")) {
            barrel = gob;
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
        stop = true;
        reqdestroy();
    }
}