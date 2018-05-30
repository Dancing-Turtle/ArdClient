package haven.automation;


import haven.Button;
import haven.*;
import haven.Label;
import haven.Window;
import haven.Coord;
import haven.purus.BotUtils;
import haven.purus.pbot.PBotAPI;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Comparator;

import java.util.List;

import static haven.OCache.posres;

public class CoalToSmelters extends Window implements GobSelectCallback {
    private static final Text.Foundry infof = new Text.Foundry(Text.sans, 10).aa(true);
    private GameUI gui;
    private Coord selectedAreaA, selectedAreaB;
    private Gob gob;
    private static final int TIMEOUT_ACT = 6000;
    private Gob smelter;
    private ArrayList<Gob> smelters = new ArrayList<Gob>();
    private int count = 1;
    private final Label lblc, lblc2, LabelFuel, lblc4;
    public boolean terminate = false;
    public boolean terminate2 = true;
    private int smeltercount;
    private Button runbtn, clearbtn, stopbtn, clearbtn2, runbtn2, fuelbtn, switchbtn, light1btn, light2btn, lightallbtn, runbtn3, areaSelBtn, swapbtn;
    private static final int TIMEOUT = 2000;
    private static final int HAND_DELAY = 8;
    private Thread runner;
    private Thread light;
    private Thread selectingarea;
    private int bankselected = 1;
    private int countretain;
    private int torchselected = 1;
    public static int delay = 100;



    List<Gob> list = new ArrayList<>();
    List<Gob> list2 = new ArrayList<>();
    List<Gob> activelist = new ArrayList<>();
    List<Gob> torchlist = new ArrayList<>();
    List<Gob> biglist = new ArrayList<>();


    public CoalToSmelters(GameUI gui) {


        super(new Coord(270, 210), "Add Coal To Smelters");

        final Label lbl = new Label("Alt + Click to select Ovens or Smelters", infof);
        add(lbl, new Coord(53, 5));

        final Label lbl4 = new Label("Number of coal/branches being added.", infof);
        add(lbl4, new Coord(50, 15));
        LabelFuel = new Label("1", Text.num12boldFnd, Color.WHITE);
        add(LabelFuel, new Coord(130, 25));
        LabelFuel.settext(count + "");

        final Label lbl2 = new Label("Count Bank 1", infof);
        add(lbl2, new Coord(35, 45));

        lblc = new Label("0", Text.num12boldFnd, Color.WHITE);
        add(lblc, new Coord(63, 56));


        final Label lbl3 = new Label("Count Bank 2", infof);
        add(lbl3, new Coord(185, 45));

        lblc2 = new Label("0", Text.num12boldFnd, Color.WHITE);
        add(lblc2, new Coord(213, 56));

        final Label lbl5 = new Label("Oven/Smelter Bank Selected", infof);
        add(lbl5, new Coord(65, 35));
        lblc4 = new Label("1", Text.num12boldFnd, Color.WHITE);
        add(lblc4, new Coord(130, 45));


        switchbtn = new Button(100, "Switch Oven/Smelter Banks") {
            @Override
            public void click() {
                BotUtils.sysMsg("Switching banks", Color.white);
                terminate = false;
                if (bankselected == 1) {
                    bankselected = 2;
                } else if (bankselected == 2)
                    bankselected = 1;
                else if (bankselected == 3)
                    bankselected = 1;
                lblc4.settext(bankselected + "");
            }
        };

        add(switchbtn, new Coord(50, 65));

        clearbtn = new Button(100, "Clear Bank 1") {
            @Override
            public void click() {
                terminate = false;
                BotUtils.sysMsg("Bank 1 Cleared", Color.white);
                list.clear();
                activelist = null;
                lblc.settext(list.size() + "");
            }
        };

        add(clearbtn, new Coord(30, 90));

        clearbtn2 = new Button(100, "Clear Bank 2") {
            @Override
            public void click() {
                terminate = false;
                BotUtils.sysMsg("Bank 2 Cleared", Color.white);
                list2.clear();
                activelist = null;
                lblc2.settext(list2.size() + "");
            }
        };
        add(clearbtn2, new Coord(155, 90));

        fuelbtn = new Button(100, "Fuel Count") {
            @Override
            public void click() {
                gui.error("Adjusting Fuel Count");
                terminate = false;
                if (count == 0) {
                    count = 1;
                } else if (count == 1) {
                    count = 4;
                } else if (count == 4) {
                    count = 8;
                } else if (count == 8) {
                    count = 9;
                } else if (count == 9) {
                    count = 11;
                } else if (count == 11) {
                    count = 12;
                } else if (count == 12) {
                    count = 1;
                }
                LabelFuel.settext(count + "");
            }
        };

        add(fuelbtn, new Coord(90, 115));

        runbtn = new Button(100, "Run bank 1") {
            @Override
            public void click() {
                if (list.size() == 0) {
                    gameui().error("No Smelters/Ovens");
                    return;
                }
                this.hide();
                cbtn.hide();
                runbtn2.hide();
                stopbtn.show();
                fuelbtn.hide();
                switchbtn.hide();
                terminate = false;
                clearbtn2.hide();
                swapbtn.hide();
                areaSelBtn.hide();
                light1btn.hide();
                light2btn.hide();
                lightallbtn.hide();
                terminate = false;
                clearbtn.hide();
                bankselected = 1;
                lblc4.settext(bankselected + "");
                runner = new Thread(new CoalToSmelters.Runner(), "Add Coal To Smelters");
                runner.start();
            }
        };
        add(runbtn, new Coord(0, 140));
        runbtn2 = new Button(100, "Run bank 2") {
            @Override
            public void click() {
                if (list2.size() == 0) {
                    gameui().error("No Smelters/Ovens");
                    return;
                }
                this.hide();
                runbtn.hide();
                cbtn.hide();
                switchbtn.hide();
                clearbtn2.hide();
                terminate = false;
                swapbtn.hide();
                areaSelBtn.hide();
                fuelbtn.hide();
                lightallbtn.hide();
                light2btn.hide();
                light1btn.hide();
                stopbtn.show();
                terminate = false;
                clearbtn.hide();
                bankselected = 2;
                lblc4.settext(bankselected + "");
                runner = new Thread(new CoalToSmelters.Runner(), "Add Coal To Smelters");
                runner.start();
            }
        };
        add(runbtn2, new Coord(170, 140));
        runbtn3 = new Button(70, "Run All") {
            @Override
            public void click() {
                if (list2.size() == 0 || list.size() == 0) {
                    gameui().error("No Smelters/Ovens in one of the banks.");
                    return;
                }
                this.hide();
                runbtn2.hide();
                runbtn.hide();
                cbtn.hide();
                swapbtn.hide();
                areaSelBtn.hide();
                stopbtn.show();
                terminate = false;
                clearbtn.hide();
                bankselected = 3;
                lblc4.settext(bankselected + "");
                runner = new Thread(new CoalToSmelters.Runner(), "Add Coal To Smelters");
                runner.start();
            }
        };
        add(runbtn3, new Coord(100, 146));
        stopbtn = new Button(100, "Stop") {
            @Override
            public void click() {
                BotUtils.sysMsg("Stopping", Color.white);
                // terminate();
                this.hide();
                terminate = true;
                terminate2 = true;
                terminate();
                runbtn.show();
                runbtn2.show();
                runbtn3.show();
                clearbtn.show();
                clearbtn2.show();
                fuelbtn.show();
                swapbtn.show();
                areaSelBtn.show();
                switchbtn.show();
                light1btn.show();
                light2btn.show();
                lightallbtn.show();
                cbtn.show();
            }
        };
        stopbtn.hide();
        add(stopbtn, new Coord(90, 140));
        light1btn = new Button(100, "Light 1") {
            @Override
            public void click() {
                this.hide();
                runbtn2.hide();
                runbtn.hide();
                stopbtn.show();
                runbtn3.show();
                swapbtn.hide();
                areaSelBtn.hide();
                terminate = false;
                clearbtn.show();
                clearbtn2.show();
                cbtn.show();
                torchselected = 1;
                light = new Thread(new CoalToSmelters.light(), "Add Coal To Smelters");
                light.start();
            }
        };
        add(light1btn, new Coord(0, 165));
        light2btn = new Button(100, "Light 2") {
            @Override
            public void click() {
                this.hide();
                light1btn.hide();
                lightallbtn.hide();
                stopbtn.show();
                runbtn2.hide();
                terminate = false;
                swapbtn.hide();
                areaSelBtn.hide();
                runbtn.hide();
                clearbtn.show();
                cbtn.show();
                torchselected = 2;
                light = new Thread(new CoalToSmelters.light(), "Add Coal To Smelters");
                light.start();
            }
        };
        add(light2btn, new Coord(170, 165));
        lightallbtn = new Button(70, "Light all") {
            @Override
            public void click() {
                this.hide();
                terminate = false;
                light2btn.hide();
                light1btn.hide();
                stopbtn.show();
                runbtn2.hide();
                runbtn.hide();
                clearbtn.hide();
                swapbtn.hide();
                areaSelBtn.hide();
                clearbtn2.hide();
                fuelbtn.hide();
                switchbtn.hide();
                cbtn.show();
                torchselected = 3;
                light = new Thread(new CoalToSmelters.light(), "Add Coal To Smelters");
                light.start();
            }
        };
        add(lightallbtn, new Coord(100, 171));
        swapbtn = new Button(70, "AutoDrop") {
            @Override
            public void click() {
                if (terminate2){
                    terminate2 = false;
                    BotUtils.sysMsg("Autodrop on",Color.white);
                }
                else {
                    terminate2 = true;
                    BotUtils.sysMsg("Autodrop off",Color.white);
                }

            }
        };
        add(swapbtn, new Coord(170, 195));
        areaSelBtn = new Button(70, "Select") {
            @Override
            public void click() {
               selectingarea = new Thread(new CoalToSmelters.selectingarea(), "Add Coal To Smelters");
                selectingarea.start();
            }
        };
        add(areaSelBtn, new Coord(100, 195));
        ActionListener timedevent = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (!terminate2) {
                    try {
                        Window w = gui.getwnd("Ore Smelter");
                        if (w == null)
                            return;
                        if (Config.dropsmelterstones) {
                            for (Widget smelter = w.lchild; smelter != null; smelter = smelter.prev) {
                                if (smelter instanceof Inventory) {
                                    List<WItem> stones = getstones((Inventory) smelter);
                                    List<WItem> bars = getbars((Inventory) smelter);
                                    for (WItem item : stones) {
                                        GItem stone = item.item;
                                        stone.wdgmsg("drop", Coord.z);
                                    }
                                    for (WItem bar : bars) {
                                        GItem barslol = bar.item;
                                        barslol.wdgmsg("transfer", Coord.z);
                                    }
                                }
                            }
                        }
                    } catch (NullPointerException p) {
                    }
                }
            }
        };
            new javax.swing.Timer(delay, timedevent).start();

    }

        private class selectingarea implements Runnable {
        @Override
            public void run() {
            BotUtils.sysMsg("Drag area over smelters/Ovens", Color.WHITE);
            PBotAPI.selectArea();
            //gui.map.PBotAPISelect = true;
            // while(gui.map.PBotAPISelect)
            //BotUtils.sleep(100);
            // BotUtils.sysMsg("Adding", Color.WHITE);
            try {
                selectedAreaA = PBotAPI.getSelectedAreaA();
                selectedAreaB = PBotAPI.getSelectedAreaB();
                biglist.addAll(Smelters());

                for (int i = 0; i < 8; i++) {
                        list.add(biglist.get(i));
                }
                for (int i = 8; i < 16; i++) {
                    list2.add(biglist.get(i));
                }
                }catch(IndexOutOfBoundsException | NullPointerException idklol){BotUtils.sysMsg("Error detected, please try closing and reopening the script window.",Color.white);}
                    lblc.settext(list.size() + "");
                    lblc2.settext(list2.size() + "");
                    lblc4.settext(bankselected + "");
                    biglist = null;
        }
        }

        private class Runner implements Runnable {
            @Override
            public void run() {
                GameUI gui = gameui();
                countretain = count;
                try {
                    if (bankselected == 1) {
                        activelist = list;
                    } else if (bankselected == 2) {
                        activelist = list2;
                    } else if (bankselected == 3) {
                        activelist.addAll(list);
                        activelist.addAll(list2);
                    }
                    if (count == 0) {
                        BotUtils.sysMsg("Please increase fuel count above 0", Color.white);
                        stopbtn.click();
                        return;
                    }
                    if (activelist.size() == 0) {
                        BotUtils.sysMsg("Found list is null.", Color.white);
                        terminate = true;
                        stopbtn.click();
                    }
                    while (!terminate) {
                        for (int l = 0; l < activelist.size(); l++) {
                            count = countretain;
                            WItem coalw = gui.maininv.getItemPartial("Coal");
                            WItem coalw2 = gui.maininv.getItemPartial("Branch");
                            if (coalw == null) {
                                if(coalw2==null) {
                                    gui.error("No fuel found in the inventory");
                                    stopbtn.click();
                                    return;
                                }
                            }
                            if (coalw != null) {

                                GItem coal = coalw.item;
                                coal.wdgmsg("take", new Coord(coal.sz.x / 2, coal.sz.y / 2));
                                int timeout = 0;
                                while (gui.hand.isEmpty() || gui.vhand == null) {
                                    timeout += HAND_DELAY;
                                    if (timeout >= TIMEOUT) {
                                        gui.error("No coal found in the inventory");
                                        stopbtn.click();
                                        return;
                                    }
                                    try {
                                        Thread.sleep(HAND_DELAY);
                                    } catch (InterruptedException e) {
                                        return;
                                    }
                                }
                                coal = gui.vhand.item;
                                for (; count > 0; count--) {
                                    gui.map.wdgmsg("itemact", Coord.z, activelist.get(l).rc.floor(posres), count == 1 ? 0 : 1, 0, (int) activelist.get(l).id, activelist.get(l).rc.floor(posres), 0, -1);
                                    timeout = 0;
                                    while (true) {
                                        WItem newcoal = gui.vhand;
                                        if (newcoal != null && newcoal.item != coal) {
                                            coal = newcoal.item;
                                            break;
                                        } else if (newcoal == null && count == 1) {
                                            break;
                                        }
                                        timeout += HAND_DELAY;
                                        if (timeout >= TIMEOUT) {
                                            gui.error("Not enough coal. Need to add " + (count - 1) + " more.");
                                            stopbtn.click();
                                            return;
                                        }
                                        try {
                                            Thread.sleep(HAND_DELAY);
                                        } catch (InterruptedException e) {
                                            // return;
                                        }
                                    }
                                }
                            }
                            if (coalw2 != null) {
                                    if (coalw2 == null) {
                                        gui.error("No fuel found in the inventory");
                                        stopbtn.click();
                                        return;
                                    }
                                GItem coal = coalw2.item;
                                coal.wdgmsg("take", new Coord(coal.sz.x / 2, coal.sz.y / 2));
                                int timeout = 0;
                                while (gui.hand.isEmpty() || gui.vhand == null) {
                                    timeout += HAND_DELAY;
                                    if (timeout >= TIMEOUT) {
                                        gui.error("No coal found in the inventory");
                                        stopbtn.click();
                                        return;
                                    }
                                    try {
                                        Thread.sleep(HAND_DELAY);
                                    } catch (InterruptedException e) {
                                        return;
                                    }
                                }
                                coal = gui.vhand.item;
                                for (; count > 0; count--) {
                                    gui.map.wdgmsg("itemact", Coord.z, activelist.get(l).rc.floor(posres), count == 1 ? 0 : 1, 0, (int) activelist.get(l).id, activelist.get(l).rc.floor(posres), 0, -1);
                                    timeout = 0;
                                    while (true) {
                                        WItem newcoal = gui.vhand;
                                        if (newcoal != null && newcoal.item != coal) {
                                            coal = newcoal.item;
                                            break;
                                        } else if (newcoal == null && count == 1) {
                                            break;
                                        }
                                        timeout += HAND_DELAY;
                                        if (timeout >= TIMEOUT) {
                                            gui.error("Not enough coal. Need to add " + (count - 1) + " more.");
                                            stopbtn.click();
                                            return;
                                        }
                                        try {
                                            Thread.sleep(HAND_DELAY);
                                        } catch (InterruptedException e) {
                                            // return;
                                        }
                                    }
                                }

                            }
                        }
                    }

                } catch (NullPointerException ie) {

                }
                count = countretain;
                BotUtils.sysMsg("Done", Color.white);
                lblc.settext(list.size() + "");
                lblc2.settext(list2.size() + "");
                if (bankselected == 3)
                    bankselected = 1;
                activelist= null;
                lblc4.settext(bankselected + "");
                stopbtn.click();
            }
        }

        private class light implements Runnable {
            @Override
            public void run() {
                GameUI gui = gameui();
                if (torchselected == 1) {
                    torchlist = list;
                } else if (torchselected == 2) {
                    torchlist = list2;
                } else if (torchselected == 3) {
                    torchlist.addAll(list);
                    torchlist.addAll(list2);
                }
                while (!terminate) {
                    for (int i = 0; i < torchlist.size(); i++) {
                        try {
                            if (list.size() == 0 && torchselected == 1 || list.size() == 0 && torchselected == 3) {
                                BotUtils.sysMsg("No Smelters/Ovens found in bank 1", Color.white);
                                stopbtn.click();
                                return;
                            }
                            if (list2.size() == 0 && torchselected == 2 || list.size() == 0 && torchselected == 3) {
                                BotUtils.sysMsg("No Smelters/Ovens found in bank 2", Color.white);
                                stopbtn.click();
                                return;
                            }

                            Equipory e = gui.getequipory();
                            WItem l = e.quickslots[6];
                            WItem r = e.quickslots[7];

                            boolean noltorch = true;
                            boolean nortorch = true;

                            if (l != null) {
                                String lname = l.item.getname();
                                if (lname.contains("Lit Torch"))
                                    noltorch = false;
                            }
                            if (r != null) {
                                String rname = r.item.getname();
                                if (rname.contains("Lit Torch"))
                                    nortorch = false;
                            }

                            // take torch from equipment, otherwise assume it's already in the hand
                            if (!noltorch || !nortorch) {
                                WItem w = e.quickslots[noltorch ? 7 : 6];
                                w.mousedown(new Coord(w.sz.x / 2, w.sz.y / 2), 1);

                                try {
                                    Thread.sleep(100);
                                } catch (InterruptedException ie) {
                                    e.wdgmsg("drop", noltorch ? 7 : 6);
                                    return;
                                }
                            }
                            gui.map.wdgmsg("itemact", Coord.z, torchlist.get(i).rc.floor(posres), 0, 0, (int) torchlist.get(i).id, torchlist.get(i).rc.floor(posres), 0, -1);

                            if (!Utils.waitForProgressFinish(gui, TIMEOUT_ACT, "Oops something went wrong. Timeout when trying to light with torch.")) {
                                e.wdgmsg("drop", noltorch ? 7 : 6);
                                return;
                            }

                            // e.wdgmsg("drop", noltorch ? 7 : 6);
                        } catch (InterruptedException ie) {
                        }
                    }
                    BotUtils.sysMsg("Done", Color.white);
                    torchselected =1;
                    torchlist = null;
                    lblc.settext(list.size() + "");
                    lblc2.settext(list2.size() + "");
                    stopbtn.click();
                }
            }
        }


        private void registerGobSelect () {
            synchronized (GobSelectCallback.class) {
                BotUtils.sysMsg("Registering Gob", Color.white);
                BotUtils.gui.map.registerGobSelect(this);
            }
        }
//    public void areaselect(Coord a, Coord b){
//        BotUtils.sysMsg("Area Select",Color.white);
//        this.a = a.mul(MCache.tilesz2);
//        this.b = b.mul(MCache.tilesz2).add(11, 11);
//        BotUtils.sysMsg("Area selected!", Color.WHITE);
//        BotUtils.gui.map.unregisterAreaSelect();
//        BotUtils.sysMsg("Coord a : "+a+" Coord b : "+b,Color.white);
//    }

    public void gobselect (Gob gob){
            Resource res = gob.getres();
            if (res != null) {
                if (res.name.equals("gfx/terobjs/smelter") || res.name.equals("gfx/terobjs/oven")) {
                    if (!list.contains(gob) && bankselected == 1) {
                        list.add(gob);
                        lblc.settext(list.size() + "");
                    }
                    if (!list2.contains(gob) && bankselected == 2) {
                        list2.add(gob);
                        lblc2.settext(list2.size() + "");
                    }
                }
            }
        }
        @Override
        public void wdgmsg (Widget sender, String msg, Object...args){
            if (sender == cbtn) {
                terminate = true;
                terminate2 = true;
                reqdestroy();
            }
            else
                super.wdgmsg(sender, msg, args);
        }

        @Override
        public boolean type ( char key, KeyEvent ev){
            if (key == 27) {
                if (cbtn.visible) {
                    reqdestroy();
                    terminate = true;
                    terminate2 = true;
                }
                return true;
            }
            return super.type(key, ev);
        }

        public void terminate () {
            terminate = true;
            if (runner != null)
                runner.interrupt();
            if (light != null)
                light.interrupt();
            this.destroy();
        }
        private List<WItem> getstones (Inventory inv){
        List<WItem> getstones = inv.getItemsPartial("Gneiss");
        return getstones;
    }
    private List<WItem> getbars (Inventory inv){
        List<WItem> getbars = inv.getItemsPartial("Bar");
        return getbars;
    }


    public ArrayList<Gob> Smelters() {
        // Initialises list of crops to harvest between the selected coordinates
        ArrayList<Gob> gobs = new ArrayList<Gob>();
        double bigX = selectedAreaA.x > selectedAreaB.x ? selectedAreaA.x : selectedAreaB.x;
        double smallX = selectedAreaA.x < selectedAreaB.x ? selectedAreaA.x : selectedAreaB.x;
        double bigY = selectedAreaA.y > selectedAreaB.y ? selectedAreaA.y : selectedAreaB.y;
        double smallY = selectedAreaA.y < selectedAreaB.y ? selectedAreaA.y : selectedAreaB.y;
        synchronized (ui.sess.glob.oc) {
            for (Gob gob : ui.sess.glob.oc) {
                if (gob.rc.x <= bigX && gob.rc.x >= smallX && gob.getres() != null && gob.rc.y <= bigY
                        && gob.rc.y >= smallY && gob.getres().name.contains("smelter") ||gob.rc.x <= bigX && gob.rc.x >= smallX && gob.getres() != null && gob.rc.y <= bigY
                        && gob.rc.y >= smallY && gob.getres().name.contains("oven") ) {
                    gobs.add(gob);
                }
            }
        }
        gobs.sort(new CoordSort());
        return gobs;
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
}

