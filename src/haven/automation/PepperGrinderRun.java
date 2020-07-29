package haven.automation;

import haven.Button;
import haven.Coord;
import haven.FastMesh;
import haven.GItem;
import haven.Gob;
import haven.IMeter;
import haven.Inventory;
import haven.Label;
import haven.WItem;
import haven.Widget;
import haven.Window;
import haven.purus.pbot.PBotUtils;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Random;

import static haven.OCache.posres;

public class PepperGrinderRun extends Window implements Runnable {
    private Coord rc1, rc2;
    private ArrayList<Gob> crops = new ArrayList<Gob>();
    private ArrayList<Gob> tables = new ArrayList<Gob>();
    public ArrayList<Gob> blacklist = new ArrayList<Gob>();
    private boolean stopThread = false;
    private Label lblProg;
    private ArrayList<String> cropName = new ArrayList<String>();
    private ArrayList<String> seedName = new ArrayList<String>();
    private String trellis = "gfx/terobjs/plants/trellis";
    private boolean harvest = false;
    private boolean destroy = false;
    public Gob htable;
    private boolean replant = false;
    private static final int TIMEOUT = 1000;
    public Button stopBtn;
    public int x, y;
    private Gob chest, water, rowmarker, cauldron, barrel, hfire, grinder;
    private final int rowgap = 4200;
    private final int travel = 20000;

    private int section, direction;
    public Widget craftall;
    private Boolean boilmode = false;
    private Coord finalloc;
    private Thread t;

    public PepperGrinderRun(Coord rc1, Coord rc2, Gob grinder, int section, int direction) {
        super(new Coord(120, 45), "Pepper Grinder");
        this.grinder = grinder;
        this.rc1 = rc1;
        this.rc2 = rc2;
        this.direction = direction;
        this.section = section;

        // Initialise arraylists

        lblProg = new Label("Initialising...");
        add(lblProg, new Coord(0, 35));

        stopBtn = new Button(120, "Stop") {
            @Override
            public void click() {
                stopThread = true;
                stop();
            }
        };
        add(stopBtn, new Coord(0, 0));

    }

    public void run() {

        //section = 4;
        tables = Tables();
        PBotUtils.sysMsg(ui, "Pepper Grinder Bot started! Tables selected : " + tables.size(), Color.white);

        ui.gui.wdgmsg("act", "craft", "blackpepper");
        PBotUtils.waitForWindow(ui, "Crafting");

        if (stopThread) // Checks if aborted
            return;

        while (tables.size() > 0 && !stopThread) {
            // Check if stamina is under 30%, drink if needed
            //gui = this.parent.findchild(GameUI.class);
            IMeter.Meter stam = ui.gui.getmeter("stam", 0);
            if (stam.a <= 60) {
                lblProg.settext("Drinking");
                PBotUtils.drink(ui, true);
                PBotUtils.sleep(5000);
            }


            if (stopThread)
                return;

            int finishtimeout = 0;
            while (PBotUtils.invFreeSlots(ui) > 2 && !stopThread) {
                if (stopThread)
                    return;
                finishtimeout++;
                if (finishtimeout > 10000) {
                    stopThread = true;
                    return;
                }
                lblProg.settext("Status - Collecting");
                while (htable == null) {
                    finishtimeout++;
                    if (finishtimeout > 10000) {
                        stopThread = true;
                        return;
                    }
                    //BotUtils.sysLogAppend("while loop", "white");
                    for (Gob tablelol : tables) {
                        //BotUtils.sysLogAppend("table loop", "white");
                        if (tablelol.ols.size() > 0)
                            htable = tablelol;
                        else
                            blacklist.add(tablelol);
                    }
                }
                tables.removeAll(blacklist);
                blacklist.clear();
                //BotUtils.sleep(500);
                //	BotUtils.sysLogAppend("Found table, clicking", "white");
                //PBotUtils.pfRightClick(htable, 0);
                //PBotAPI.gui.map.pfRightClick(htable,-1,3,0,null);
                PBotUtils.PathfinderRightClick(ui, htable, 0);
                PBotUtils.sleep(1000);
                int retrytimer = 0;
                int retrycount = 0;
                while (ui.gui.getwnd("Herbalist Table") == null) {
                    retrytimer++;
                    if (retrytimer > 1000) {
                        retrytimer = 0;
                        retrycount++;
                        if (retrycount > 1) {
                            lblProg.settext("Unstucking");
                            Gob player = ui.gui.map.player();
                            Coord location = player.rc.floor(posres);
                            int x = location.x + +getrandom();
                            int y = location.y + +getrandom();
                            Coord finalloc = new Coord(x, y);
                            ui.gui.map.wdgmsg("click", Coord.z, finalloc, 1, 0);
                            retrycount = 0;
                            PBotUtils.sleep(1000);
                        }
                        //	PBotUtils.pfRightClick(htable, 0);
                        //PBotAPI.gui.map.pfRightClick(htable,-1,3,0,null);
                        PBotUtils.PathfinderRightClick(ui, htable, 0);
                    }
                    PBotUtils.sleep(10);
                }
                PBotUtils.waitForWindow(ui, "Herbalist Table");
                Window herbtable = ui.gui.getwnd("Herbalist Table");
                if (herbtable == null)
                    continue;
                int freeslots;
                for (Widget w = herbtable.lchild; w != null; w = w.prev) {
                    if (w instanceof Inventory) {
                        Inventory inv = (Inventory) w;
                        List<WItem> items = PBotUtils.getInventoryContents(inv);
                        for (WItem item : items) {
                            freeslots = PBotUtils.invFreeSlots(ui);
                            if (freeslots > 16) {
                                System.out.println("Transferring pepper freeslots : " + freeslots);
                                item.item.wdgmsg("transfer", Coord.z);
                            } else if (freeslots > 2) {
                                System.out.println("Transferring pepper freeslots : " + freeslots);
                                item.item.wdgmsg("transfer", Coord.z);
                                PBotUtils.sleep(300);
                            } else
                                break;
                        }
                    }
                }
                herbtable.close();
                //	BotUtils.sysLogAppend("Tables Size : "+tables.size(),"white");
                htable = null;
            }
            if (PBotUtils.invFreeSlots(ui) <= 2) {
                GItem pepperlol = ui.gui.maininv.getItemPartial("Dried").item;
                lblProg.settext("Status - Going to Grind");
                //	PBotUtils.pfRightClick(grinder, 0);
                //	PBotAPI.gui.map.pfRightClick(grinder,-1,3,0,null);
                PBotUtils.PathfinderRightClick(ui, grinder, 0);
                PBotUtils.sleep(6000); //sleep 6 seconds to walk to grinder
                int timeout = 0;
                int retrycount = 0;
                while (ui.gui.maininv.getIdenticalItems((pepperlol)).size() > 5) {
                    timeout++;
                    if (timeout > 5000) {
                        ui.gui.maininv.getItemPartial("Dried").item.wdgmsg("drop", Coord.z);
                        timeout = 0;
                    }
                    while (ui.gui.prog >= 0) {
                        PBotUtils.sleep(100);
                        lblProg.settext("Status - Grinding");
                    }
                    if (PBotUtils.getStamina(ui) > 50) {
                        PBotUtils.craftItem(ui, "blackpepper", 1);
                        PBotUtils.sleep(2000);
                        retrycount++;
                        if (retrycount > 1) {
                            lblProg.settext("Unstucking");
                            Gob player = ui.gui.map.player();
                            Coord location = player.rc.floor(posres);
                            int x = location.x + +getrandom();
                            int y = location.y + +getrandom();
                            Coord finalloc = new Coord(x, y);
                            ui.gui.map.wdgmsg("click", Coord.z, finalloc, 1, 0);
                            retrycount = 0;
                            PBotUtils.sleep(1000);
                            //PBotUtils.pfRightClick(grinder, 0);
                            //	PBotAPI.gui.map.pfRightClick(grinder,-1,3,0,null);
                            PBotUtils.PathfinderRightClick(ui, grinder, 0);
                        }
                    } else {
                        lblProg.settext("Status - Drinking");
                        PBotUtils.drink(ui, true);
                        PBotUtils.sleep(5000);
                    }
                }
            }
            if (stopThread)
                return;
        }
        PBotUtils.sysMsg(ui, "Done", Color.white);
        stopThread = true;
    }

    public int getrandom() {
        Random r = new Random();
        int randomNumber = r.ints(1, -6000, 6000).findFirst().getAsInt();
        return randomNumber;
    }


    public ArrayList<Gob> Crops(boolean checkStage) {
        // Initialises list of crops to harvest between selected coordinates
        ArrayList<Gob> gobs = new ArrayList<Gob>();
        double bigX = rc1.x > rc2.x ? rc1.x : rc2.x;
        double smallX = rc1.x < rc2.x ? rc1.x : rc2.x;
        double bigY = rc1.y > rc2.y ? rc1.y : rc2.y;
        double smallY = rc1.y < rc2.y ? rc1.y : rc2.y;
        synchronized (ui.sess.glob.oc) {
            for (Gob gob : ui.sess.glob.oc) {
                if (gob.rc.x <= bigX && gob.rc.x >= smallX && gob.getres() != null && gob.rc.y <= bigY
                        && gob.rc.y >= smallY && cropName.contains(gob.getres().name)) {
                    // Add to list if its max stage
                    if (checkStage) {
                        int cropstgmaxval = 0;
                        for (FastMesh.MeshRes layer : gob.getres().layers(FastMesh.MeshRes.class)) {
                            int stg = layer.id / 10;
                            if (stg > cropstgmaxval)
                                cropstgmaxval = stg;
                        }
                        if (gob.getStage() == cropstgmaxval) {
                            gobs.add(gob);
                        }
                    } else
                        gobs.add(gob);
                }
            }
        }
        gobs.sort(new CoordSortEW());
        return gobs;
    }

    public ArrayList<Gob> Tables() {
        // Initialises list of crops to harvest between selected coordinates
        ArrayList<Gob> gobs = new ArrayList<Gob>();
        double bigX = rc1.x > rc2.x ? rc1.x : rc2.x;
        double smallX = rc1.x < rc2.x ? rc1.x : rc2.x;
        double bigY = rc1.y > rc2.y ? rc1.y : rc2.y;
        double smallY = rc1.y < rc2.y ? rc1.y : rc2.y;
        synchronized (ui.sess.glob.oc) {
            for (Gob gob : ui.sess.glob.oc) {
                if (gob.rc.x <= bigX && gob.rc.x >= smallX && gob.getres() != null && gob.rc.y <= bigY
                        && gob.rc.y >= smallY && gob.getres().basename().contains("htable")) {
                    gobs.add(gob);
                }
            }
        }
        if (direction == 1 || direction == 2)
            gobs.sort(new CoordSortNS());
        else
            gobs.sort(new CoordSortEW());
        return gobs;
    }

    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (sender == cbtn) {
            stop();
            reqdestroy();
        } else
            super.wdgmsg(sender, msg, args);
    }

    // Sorts coordinate array to efficient sequence
    class CoordSortEW implements Comparator<Gob> { // sorts high Y to low Y along same X Axis
        public int compare(Gob a, Gob b) {

            if (a.rc.x == b.rc.x) {
                if (a.rc.x % 2 == 0)
                    return (a.rc.y < b.rc.y) ? 1 : (a.rc.y > b.rc.y) ? -1 : 0;
                else
                    return (a.rc.y < b.rc.y) ? -1 : (a.rc.y > b.rc.y) ? 1 : 0;
            } else
                return (a.rc.x < b.rc.x) ? -1 : (a.rc.x > b.rc.x) ? 1 : 0;
        }
    }

    class CoordSortNS implements Comparator<Gob> { // sorts high X to low X along the same Y Axis
        public int compare(Gob a, Gob b) {

            if (a.rc.y == b.rc.y) {
                if (a.rc.y % 2 == 0)
                    return (a.rc.x < b.rc.x) ? 1 : (a.rc.x > b.rc.x) ? -1 : 0;
                else
                    return (a.rc.x < b.rc.x) ? -1 : (a.rc.x > b.rc.x) ? 1 : 0;
            } else
                return (a.rc.y < b.rc.y) ? -1 : (a.rc.y > b.rc.y) ? 1 : 0;
        }
    }


    public void stop() {
        // Stops thread
        PBotUtils.sysMsg(ui, "Pepper Grinder Stopped!", Color.white);
        //ui.gui.map.wdgmsg("click", Coord.z, ui.gui.map.player().rc.floor(posres), 1, 0);
        if (ui.gui.map.pfthread != null) {
            ui.gui.map.pfthread.interrupt();
        }
        stopThread = true;
        this.destroy();
    }
}
