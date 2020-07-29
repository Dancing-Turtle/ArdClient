package haven.automation;


import haven.Button;
import haven.Label;
import haven.Window;
import haven.*;
import haven.purus.pbot.PBotUtils;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import static haven.OCache.posres;

public class LightWithTorch extends Window implements GobSelectCallback {
    private static final Text.Foundry infof = new Text.Foundry(Text.sans, 10).aa(true);
    private Gob gob;
    private static final int TIMEOUT_ACT = 6000;
    private Gob smelter;
    private int count;
    private final Label lblc;
    public boolean terminate = false;
    private int smeltercount;
    private Button runbtn, clearbtn, stopbtn;
    private static final int TIMEOUT = 2000;
    private static final int HAND_DELAY = 8;
    private Thread runner;
    public Gob selection;
    UI ui;


    List<Gob> list = new ArrayList<>();

    public LightWithTorch(UI ui) {
        super(new Coord(270, 180), "Torch Lighter");
        this.ui = ui;

        final Label lbl = new Label("Alt + Click to select Ovens or Smelters", infof);
        add(lbl, new Coord(50, 20));

        Label lblctxt = new Label("Smelters/Ovens Selected:", infof);
        add(lblctxt, new Coord(85, 60));
        lblc = new Label("0", Text.num12boldFnd, Color.WHITE);
        add(lblc, new Coord(220, 58));


        clearbtn = new Button(140, "Clear Selection") {
            @Override
            public void click() {
                list.clear();
                lblc.settext(list.size() + "");
            }
        };
        add(clearbtn, new Coord(65, 90));

        runbtn = new Button(140, "Run") {
            @Override
            public void click() {
                if (list.size() == 0) {
                    ui.gui.error("No Smelters/Ovens");
                    return;
                }
                this.hide();
                cbtn.hide();
                stopbtn.show();
                terminate = false;
                clearbtn.hide();
                runner = new Thread(new LightWithTorch.Runner(), "Torch Lighter");
                runner.start();
            }
        };
        add(runbtn, new Coord(65, 140));

        stopbtn = new Button(140, "Stop") {
            @Override
            public void click() {
                terminate = true;
                // TODO: terminate PF
                this.hide();
                runbtn.show();
                clearbtn.show();
                cbtn.show();
            }
        };
        stopbtn.hide();
        add(stopbtn, new Coord(65, 140));
    }

    private class Runner implements Runnable {
        @Override
        public void run() {
            for (int i = 0; i < list.size(); i++) {
                //   gui.error("Lighting number : " + i);
                try {
                    if (list.get(i) == null) {
                        ui.gui.error("No smelters/ovens found");
                        return;
                    }

                    Equipory e = ui.gui.getequipory();
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

                    ui.gui.map.wdgmsg("itemact", Coord.z, list.get(i).rc.floor(posres), 0, 0, (int) list.get(i).id, list.get(i).rc.floor(posres), 0, -1);

                    if (!Utils.waitForProgressFinish(ui.gui, TIMEOUT_ACT, "Oops something went wrong. Timeout when trying to light with torch.")) {
                        e.wdgmsg("drop", noltorch ? 7 : 6);
                        return;
                    }

                    //e.wdgmsg("drop", noltorch ? 7 : 6);
                } catch (InterruptedException ie) {
                }
            }
            PBotUtils.sysMsg(ui, "Done", Color.white);
        }
    }


    private void registerGobSelect() {
        synchronized (GobSelectCallback.class) {
            PBotUtils.sysMsg(ui, "Registering Gob", Color.white);
            ui.gui.map.registerGobSelect(this);
        }
    }

    public void gobselect(Gob gob) {
        Resource res = gob.getres();
        if (res != null) {
            if (res.name.equals("gfx/terobjs/smelter") || res.name.equals("gfx/terobjs/oven")) {
                if (!list.contains(gob)) {
                    list.add(gob);
                    lblc.settext(list.size() + "");
                }
            }
        }
    }

    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (sender == cbtn)
            reqdestroy();
        else
            super.wdgmsg(sender, msg, args);
    }

    @Override
    public boolean type(char key, KeyEvent ev) {
        if (key == 27) {
            if (cbtn.visible)
                reqdestroy();
            return true;
        }
        return super.type(key, ev);
    }

    public void terminate() {
        terminate = true;
        if (runner != null)
            runner.interrupt();
        this.destroy();
    }
}


