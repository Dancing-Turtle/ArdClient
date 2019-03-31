package haven.automation;


import haven.*;
import haven.Label;

import haven.Window;
import haven.Button;
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.ConcurrentModificationException;
import java.util.List;

public class DestroyArea extends Window implements GobSelectCallback {
    private static final Text.Foundry infof = new Text.Foundry(Text.sans, 10).aa(true);
    public Thread selectingarea, runner;
    private Button runbtn, stopbtn, selectareabtn, selectgobbtn;
    private Coord selectedAreaA, selectedAreaB;
    private int TIMEOUT_ACT = 6000;
    public GameUI gui;
    private final Label lblc2, lblc, lblc3;
    private boolean terminate = false;
    private Gob gobselected;
    List<Gob> list = new ArrayList<>();

    public DestroyArea(GameUI gui) {

        super(new Coord(170, 220), "Destroy Gobs in Area");

        this.gui = gui;
        int ycoord = 5;
        final haven.Label lbl = new haven.Label("Alt + Click to select Gob to destroy", infof);
        add(lbl, new Coord(0, ycoord));

        final Label lbl2 = new Label("Count of Gobs", infof);
        add(lbl2, new Coord(0, ycoord+=15));
        lblc2 = new Label("", Text.num12boldFnd, Color.WHITE);
        add(lblc2, new Coord(0, ycoord+=15));

        final Label lbl3 = new Label("Gob Selected", infof);
        add(lbl3, new Coord(0, ycoord+=15));
        lblc = new Label("", Text.num12boldFnd, Color.WHITE);
        add(lblc, new Coord(0, ycoord+=15));
        final Label lbl4 = new Label("Gobs remaining",infof);
        add(lbl4, new Coord(0,ycoord+=15));
        lblc3 = new Label("",Text.num12boldFnd,Color.white);
        add(lblc3,new Coord(0,ycoord+=15));


        runbtn = new haven.Button(100, "Run") {
            @Override
            public void click() {
                if (list.size() == 0) {
                    gameui().error("No Gobs");
                    return;
                }
                this.hide();
                stopbtn.show();
                runner = new Thread(new DestroyArea.runner(), "Destroy Gobs in Area");
                runner.start();
            }
        };
        add(runbtn, new Coord(0, ycoord+=35));
        stopbtn = new haven.Button(100, "Stop") {
            @Override
            public void click() {
                PBotUtils.sysMsg("Stopping", Color.white);
                this.hide();
               runner.interrupt();
                runbtn.show();
               cbtn.show();
            }
        };
        stopbtn.hide();
        add(stopbtn, new Coord(0, ycoord+=35));
        selectareabtn = new Button(100, "Select") {
            @Override
            public void click() {
                selectingarea = new Thread(new DestroyArea.selectingarea(), "Destroy Gobs in Area");
                selectingarea.start();
            }
        };
        add(selectareabtn, new Coord(0, ycoord+=35));
    }

    private class runner implements Runnable {
        @Override
        public void run() {
            try {
                int remaining = list.size();
                for (Gob i : list) {
                    boolean destroyed = false;
                    PBotUtils.destroyGob(i);
                    PBotUtils.sleep(200);
                    while (!destroyed) {
                        List<Gob> allgobs = PBotUtils.getGobs();
                        if (!allgobs.contains(i))
                            destroyed = true;
                        else
                            PBotUtils.sleep(300);
                    }
                remaining --;
                    lblc3.settext(remaining+"");
                }
                runbtn.click();
            }catch(NullPointerException | ConcurrentModificationException ip){}
        }
    }

    private class selectingarea implements Runnable {
        @Override
        public void run() {

            PBotUtils.sysMsg("Drag area over Gobs", Color.WHITE);
            PBotUtils.selectArea();
            list.clear();
            //gui.map.PBotAPISelect = true;
            // while(gui.map.PBotAPISelect)
            //BotUtils.sleep(100);
            // BotUtils.sysMsg("Adding", Color.WHITE);
            try {
                selectedAreaA = PBotUtils.getSelectedAreaA();
                selectedAreaB = PBotUtils.getSelectedAreaB();
                list.addAll(GobList());
            }catch(IndexOutOfBoundsException | NullPointerException idklol){PBotUtils.sysMsg("Error detected, please try closing and reopening the script window.",Color.white);}
            lblc2.settext(list.size() + "");
        }
    }
    private void registerGobSelect () {
        synchronized (GobSelectCallback.class) {
            PBotUtils.sysMsg("Registering Gob", Color.white);
            PBotAPI.gui.map.registerGobSelect(this);
        }
    }
    public void gobselect (Gob gob){
        this.gobselected = gob;
        PBotUtils.sysMsg("Gob selected!",Color.white);
        lblc.settext(gob.getres().basename());
    }
    public ArrayList<Gob> GobList() {
        // Initialises list of crops to harvest between the selected coordinates
        ArrayList<Gob> gobs = new ArrayList<Gob>();
        double bigX = selectedAreaA.x > selectedAreaB.x ? selectedAreaA.x : selectedAreaB.x;
        double smallX = selectedAreaA.x < selectedAreaB.x ? selectedAreaA.x : selectedAreaB.x;
        double bigY = selectedAreaA.y > selectedAreaB.y ? selectedAreaA.y : selectedAreaB.y;
        double smallY = selectedAreaA.y < selectedAreaB.y ? selectedAreaA.y : selectedAreaB.y;
        synchronized (ui.sess.glob.oc) {
            for (Gob gob : ui.sess.glob.oc) {
                if (gob.rc.x <= bigX && gob.rc.x >= smallX && gob.getres() != null && gob.rc.y <= bigY
                        && gob.rc.y >= smallY && gob.getres().name == gobselected.getres().name) {
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
    @Override
    public void wdgmsg (Widget sender, String msg, Object...args){
        if (sender == cbtn) {
            terminate();
            terminate= true;
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
            }
            return true;
        }
        return super.type(key, ev);
    }

    public void terminate () {
        if (runner != null)
            runner.interrupt();
        terminate = true;
    }
}
