package haven.automation;

import haven.Button;
import haven.CheckBox;
import haven.Coord;
import haven.GItem;
import haven.GSprite;
import haven.Gob;
import haven.Label;
import haven.MCache;
import haven.MapView;
import haven.Resource;
import haven.Text;
import haven.WItem;
import haven.Widget;
import haven.Window;
import haven.purus.pbot.PBotUtils;
import haven.res.ui.tt.q.qbuff.QBuff;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static haven.OCache.posres;

public class PepperBot extends Window implements AreaSelectCallback, GobSelectCallback {
    private Coord a, b;
    private boolean containeronly = false, replant = true, replantcontainer = false;
    private static final Text.Foundry infof = new Text.Foundry(Text.sans, 10).aa(true);
    private CheckBox replantChkbox, fillContainerChkbox, replantBarrelChkbox;
    private Gob barrel, hfire, rowmarker, water, cauldron, htable, grinder;
    public Thread testthread;
    private final int rowgap = 4200;
    private final int northtravel = 20000;
    private int section = 1;
    private int direction = 1;
    List<WItem> ItemList;
    public boolean allowrun;
    private final Label lblc4, lblc3;
    private final int travel = 20000;
    public int xx, yy;

    public PepperBot() {
        super(new Coord(180, 290), "Pepper Bot");

        int y = 0;
        final Label lbl5 = new Label("Setup Selected", infof);
        add(lbl5, new Coord(20, y));
        y += 15;
        lblc4 = new Label("Tables West-->East", Text.num12boldFnd, Color.WHITE);
        add(lblc4, new Coord(20, y));
        y += 35;
        final Label lbl6 = new Label("Section Selected", infof);
        add(lbl6, new Coord(20, y));
        y += 15;
        lblc3 = new Label(section + "", Text.num12boldFnd, Color.WHITE);
        add(lblc3, new Coord(20, y));
        y += 25;

        if (PBotUtils.findObjectByNames(ui, 100, "gfx/terobjs/quern") != null) {
            grinder = PBotUtils.findObjectByNames(ui, 100, "gfx/terobjs/quern");
            if (!MapView.markedGobs.contains(grinder.id))
                MapView.markedGobs.add(grinder.id);
            PBotUtils.sysLogAppend(ui, "Auto added Quern", "white");
        }
        if (PBotUtils.findObjectByNames(ui, 100, "gfx/terobjs/well") != null) {
            water = PBotUtils.findObjectByNames(ui, 100, "gfx/terobjs/well");
            if (!MapView.markedGobs.contains(water.id))
                MapView.markedGobs.add(water.id);
            PBotUtils.sysLogAppend(ui, "Auto added water source", "white");
        }
        if (PBotUtils.findObjectByNames(ui, 100, "gfx/terobjs/cauldron") != null) {
            cauldron = PBotUtils.findObjectByNames(ui, 100, "gfx/terobjs/cauldron");
            if (!MapView.markedGobs.contains(cauldron.id))
                MapView.markedGobs.add(cauldron.id);
            PBotUtils.sysLogAppend(ui, "Auto added cauldron", "white");
        }
        if (PBotUtils.findObjectByNames(ui, 100, "gfx/terobjs/pow") != null) {
            hfire = PBotUtils.findObjectByNames(ui, 100, "gfx/terobjs/pow");
            if (!MapView.markedGobs.contains(hfire.id))
                MapView.markedGobs.add(hfire.id);
            PBotUtils.sysLogAppend(ui, "Auto added hearthfire", "white");
        }
        if (PBotUtils.findObjectByNames(ui, 100, "gfx/terobjs/barrel") != null) {
            barrel = PBotUtils.findObjectByNames(ui, 100, "gfx/terobjs/barrel");
            if (!MapView.markedGobs.contains(barrel.id))
                MapView.markedGobs.add(barrel.id);
            PBotUtils.sysLogAppend(ui, "Auto added barrel", "white");
        }


        Button trelHarBtn = new Button(140, "Trellis harvest") {
            @Override
            public void click() {
                allowrun = true;
                if (hfire == null) {
                    PBotUtils.sysMsg(ui, "No Hearthfire Selected.", Color.white);
                    allowrun = false;
                }
                if (barrel == null) {
                    PBotUtils.sysMsg(ui, "No barrel Selected.", Color.white);
                    allowrun = false;
                }
                if (water == null) {
                    PBotUtils.sysMsg(ui, "No water source Selected.", Color.white);
                    allowrun = false;
                }
                if (cauldron == null) {
                    PBotUtils.sysMsg(ui, "No cauldron Selected.", Color.white);
                    allowrun = false;
                }


                if (a != null && b != null && allowrun) {
                    PepperBotRun bf = new PepperBotRun(a, b, true, false, false, barrel, water, cauldron, section, hfire, direction);

                    ui.gui.add(bf, new Coord(ui.gui.sz.x / 2 - bf.sz.x / 2, ui.gui.sz.y / 2 - bf.sz.y / 2 - 200));
                    new Thread(bf).start();
                    this.parent.destroy();
                } else if (allowrun) {
                    PBotUtils.sysMsg(ui, "Area not selected!", Color.WHITE);
                }
            }
        };
        add(trelHarBtn, new Coord(20, y));
        y += 35;
        Button GrindBtn = new Button(140, "Grind Pepper") {
            @Override
            public void click() {
                allowrun = true;
                if (grinder == null) {
                    PBotUtils.sysMsg(ui, "No grinder Selected.", Color.white);
                    allowrun = false;
                }
                if (a != null && b != null && allowrun) {
                    PepperGrinderRun bf = new PepperGrinderRun(a, b, grinder, section, direction);
                    ui.gui.add(bf, new Coord(ui.gui.sz.x / 2 - bf.sz.x / 2, ui.gui.sz.y / 2 - bf.sz.y / 2 - 200));
                    new Thread(bf).start();
                    this.parent.destroy();
                } else if (allowrun) {
                    PBotUtils.sysMsg(ui, "Area not selected!", Color.WHITE);
                }
            }
        };
        add(GrindBtn, new Coord(20, y));
        y += 35;
        Button areaSelBtn = new Button(140, "Select Area") {
            @Override
            public void click() {
                PBotUtils.sysMsg(ui, "Drag area over crops", Color.WHITE);
                ui.gui.map.farmSelect = true;
            }
        };
        add(areaSelBtn, new Coord(20, y));
        y += 35;
        Button sectionSelBtn = new Button(140, "Select Section") {
            @Override
            public void click() {
                section++;
                if (section > 4)
                    section = 1;
                PBotUtils.sysMsg(ui, "Section is now : " + section, Color.white);
                lblc3.settext(section + "");
            }
        };
        add(sectionSelBtn, new Coord(20, y));
        y += 35;
        Button sectionDirBtn = new Button(140, "Direction") {
            @Override
            public void click() {
                direction++;
                if (direction > 4)
                    direction = 1;
                if (direction == 1)
                    lblc4.settext("Tables West --> East");
                if (direction == 2)
                    lblc4.settext("Tables East --> West");
                if (direction == 3)
                    lblc4.settext("Tables North --> South");
                if (direction == 4)
                    lblc4.settext("Tables South --> North");
                //BotUtils.sysMsg("Section is now : "+section,Color.white);
            }
        };
        add(sectionDirBtn, new Coord(20, y));
        y += 35;
        Button RunBtn = new Button(140, "Run Test") {
            @Override
            public void click() {
                testthread = new Thread(new PepperBot.testthread(), "Pepper Bot");
                testthread.start();
            }
        };
        add(RunBtn, new Coord(20, y));
    }

    public void gobselect(Gob gob) {
        if (gob.getres().basename().contains("barrel")) {
            barrel = gob;
            PBotUtils.sysMsg(ui, "Barrel selected!x : " + gob.rc.x + " y : " + gob.rc.y, Color.WHITE);
        } else if (gob.getres().basename().contains("well") || (gob.getres().basename().contains("cistern"))) {
            water = gob;
            PBotUtils.sysMsg(ui, "Well/Cistern selected! x : " + gob.rc.x + " y : " + gob.rc.y, Color.white);
        } else if (gob.getres().basename().contains("pow")) {
            hfire = gob;
            PBotUtils.sysMsg(ui, "Hearthfire selected!x : " + gob.rc.x + " y : " + gob.rc.y, Color.white);
        } else if (gob.getres().basename().contains("cauldron")) {
            cauldron = gob;
            PBotUtils.sysMsg(ui, "Cauldron Selected!x : " + gob.rc.x + " y : " + gob.rc.y, Color.white);
        } else if (gob.getres().basename().contains("htable")) {
            htable = gob;
            int stage = gob.getStage();
            PBotUtils.sysMsg(ui, "table selected : " + gob.rc.x + " y : " + gob.rc.y + " stage : " + stage + " overlay : " + gob.ols.size(), Color.white);
        } else if (gob.getres().basename().contains("quern")) {
            grinder = gob;
            PBotUtils.sysMsg(ui, "grinder selected : " + gob.rc.x + " y : " + gob.rc.y, Color.white);
        }
    }


    private void registerGobSelect() {
        synchronized (GobSelectCallback.class) {
            ui.gui.map.registerGobSelect(this);
        }
    }

    public void areaselect(Coord a, Coord b) {
        this.a = a.mul(MCache.tilesz2);
        this.b = b.mul(MCache.tilesz2).add(11, 11);
        PBotUtils.sysMsg(ui, "Area selected!", Color.WHITE);
        ui.gui.map.unregisterAreaSelect();
    }

    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (sender == cbtn)
            reqdestroy();
        else
            super.wdgmsg(sender, msg, args);
    }

    public class testthread implements Runnable {
        @Override
        public void run() {
            PBotUtils.sysMsg(ui, "Started", Color.white);
            PBotUtils.pfRightClick(ui, barrel, 0);
            PBotUtils.waitForWindow(ui, "Barrel");
            GItem item = PBotUtils.getInventoryItemsByNames(ui.gui.maininv, Arrays.asList("gfx/invobjs/seed-flax")).get(0).item;
            PBotUtils.takeItem(ui, item);
            while (PBotUtils.getInventoryItemsByName(ui.gui.maininv, "gfx/invobjs/seed-flax").size() > 0) {
                if (PBotUtils.getItemAtHand(ui) == null) {
                    System.out.println("Hand null, breaking");
                    break;
                }
                List<WItem> list = PBotUtils.getInventoryItemsByName(ui.gui.maininv, "gfx/invobjs/seed-flax");
                ui.gui.map.wdgmsg("itemact", Coord.z, barrel.rc.floor(posres), 1, 0, (int) barrel.id, barrel.rc.floor(posres), 0, -1);
                while (PBotUtils.getInventoryItemsByName(ui.gui.maininv, "gfx/invobjs/seed-flax").size() == list.size()) {
                    System.out.println("Waiting for lists to update");
                    PBotUtils.sleep(10);
                }
            }
            if (PBotUtils.getItemAtHand(ui) != null) {//still have seeds on cursor, dropping them in an empty inventory slot
                ui.gui.map.wdgmsg("itemact", Coord.z, barrel.rc.floor(posres), 0, 0, (int) barrel.id, barrel.rc.floor(posres), 0, -1);
                PBotUtils.sleep(250);
            }
            System.out.println("Finished");
        }

        public double round(double value, int places) {
            if (places < 0) throw new IllegalArgumentException();

            long factor = (long) Math.pow(10, places);
            value = value * factor;
            long tmp = Math.round(value);
            return (double) tmp / factor;
        }

        public void sort(java.util.List<WItem> items) {
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

        public java.util.List<WItem> getIdenticalItems(GItem item) {
            List<WItem> items = new ArrayList<WItem>();
            GSprite sprite = item.spr();
            if (sprite != null) {
                String name = sprite.getname();
                String resname = item.resource().name;
                for (Widget wdg = child; wdg != null; wdg = wdg.next) {
                    if (wdg instanceof WItem) {
                        sprite = ((WItem) wdg).item.spr();
                        if (sprite != null) {
                            Resource res = ((WItem) wdg).item.resource();
                            if (res != null && res.name.equals(resname) && (name == null || name.equals(sprite.getname())))
                                items.add((WItem) wdg);
                        }
                    }
                }
            }
            return items;
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
}
