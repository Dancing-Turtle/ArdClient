package haven.automation;


import haven.Button;
import haven.*;
import haven.Label;
import haven.Utils;
import haven.Window;
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotGobAPI;
import haven.purus.pbot.PBotUtils;
import haven.sloth.gob.Mark;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public class MinerAlert extends Window {
    private GameUI gui;
    private int countiron, countgold, countsilver, countcopper, counttin, countbasaslt, countcinnabar, countdolomite, countfeldspar, countflint, countgneiss, countgranite, counthornblende;
    private int countlimestone, countmarble, countporphyry, countquartz, countsandstone, countschist;
    private int countcassiterite, countchalcopyrite, countmalachite, countilmenite, countlimonite, counthematite, countmagnetite, countgalena, countargentite;
    private int countpetzite, countsylvanite, countnagyagite, counthornsilver,countslimes, countslimestotal;

    private static final int TIMEOUT = 2000;
    private static final int HAND_DELAY = 8;
    private Thread runner;
    public Boolean terminate = false;
    public static int delay = 5000, maxmarks = 50;
    public Gob gob;
    private Button runbtn, stopbtn, mutebtn;
    private final Label labelcountiron, labelcountgold, labelcountcinnabar, labelcountsilver, labelcounttin, labelcountcopper, labelcountmagnetite, labelcounthematite,labelcountslimes,labelcountslimestotal;
    private static final Text.Foundry infof = new Text.Foundry(Text.sans, 10).aa(true);
    private double lasterror = 0;
    public List<Gob> slimecount = new ArrayList<>();
    private static final Resource goldsfx = Resource.local().loadwait("sfx/Zelda");
    private static final Resource silversfx = Resource.local().loadwait("sfx/gold");
    private static final Resource supportalertsfx = Resource.local().loadwait("custom/sfx/omni/Z_OOT_Navi_WatchOut");
    private Boolean audiomute, SupportAlertHalf = false, SupportAlertQuarter = false, MarkTileArrows = true; // quarter is 25% damage, half is 50% damage
    private CheckBox SupportsQuarter, SupportsHalf, MarkTiles;// quarter is 25% damage, half is 50% damage
    private List<String> reslist = Arrays.asList("gfx/tiles/rocks/cassiterite","gfx/tiles/rocks/chalcopyrite","gfx/tiles/rocks/malachite","gfx/tiles/rocks/ilmenite","gfx/tiles/rocks/limonite",
            "gfx/tiles/rocks/hematite","gfx/tiles/rocks/magnetite","gfx/tiles/rocks/galena","gfx/tiles/rocks/argentite","gfx/tiles/rocks/hornsilver","gfx/tiles/rocks/petzite","gfx/tiles/rocks/sylvanite",
            "gfx/tiles/rocks/nagyagite","gfx/tiles/rocks/cinnabar");

    private final HashMap<String,String> smeltchance = new HashMap<String, String>(15) {{
        put("cassiterite","30% Tin");
        put("chalcopyrite","8% Copper 4% Iron");
        put("cinnabar","12% Quicksilver");
        put("malachite","20% Copper");
        put("peacockore","30% Copper");
        put("ilmenite","6% Iron");
        put("limonite","12% Iron");
        put("hematite","20% Iron");
        put("magnetite","30% Iron");
        put("galena","10% Silver");
        put("argentite","20% Silver");
        put("hornsilver","30% Silver");
        put("petzite","10% Gold");
        put("sylvanite","20% Gold");
        put("nagyagite","25% Gold");
    }};

    public MinerAlert(GameUI gui) {
        super(new Coord(220, 320), "Miner Alert", "Miner Alert");
        this.gui = gui;
        int yvalue = 17;
        int yvalue2 = 8;
        audiomute = false;

        final Label labeliron = new Label("Number of Iron tiles visible.", infof);
        add(labeliron, new Coord(10, yvalue2));
        labelcountiron = new Label("0", Text.num12boldFnd, Color.WHITE);
        add(labelcountiron, new Coord(65, yvalue));

        final Label labeltin = new Label("Number of Tin tiles visible.", infof);
        add(labeltin, new Coord(10, yvalue2+=20));
        labelcounttin = new Label("0", Text.num12boldFnd, Color.WHITE);
        add(labelcounttin, new Coord(65, yvalue+=20));

        final Label labelcopper = new Label("Number of Copper tiles visible.", infof);
        add(labelcopper, new Coord(10, yvalue2+=20));
        labelcountcopper = new Label("0", Text.num12boldFnd, Color.WHITE);
        add(labelcountcopper, new Coord(65, yvalue+=20));

        final Label labelgold = new Label("Number of Gold tiles visible.", infof);
        add(labelgold, new Coord(10, yvalue2+=20));
        labelcountgold = new Label("0", Text.num12boldFnd, Color.WHITE);
        add(labelcountgold, new Coord(65, yvalue+=20));

        final Label labelsilver = new Label("Number of Silver Tiles visible.", infof);
        add(labelsilver, new Coord(10, yvalue2+=20));
        labelcountsilver = new Label("0", Text.num12boldFnd, Color.WHITE);
        add(labelcountsilver, new Coord(65, yvalue+=20));

        final Label labelcinnabar = new Label("Number of Cinnabar Tiles visible.", infof);
        add(labelcinnabar, new Coord(10, yvalue2+=20));
        labelcountcinnabar = new Label("0", Text.num12boldFnd, Color.WHITE);
        add(labelcountcinnabar, new Coord(65, yvalue+=20));

        final Label labelmagnetite = new Label("Number of Black Ore Tiles visible.", infof);
        add(labelmagnetite, new Coord(10, yvalue2+=20));
        labelcountmagnetite = new Label("0", Text.num12boldFnd, Color.WHITE);
        add(labelcountmagnetite, new Coord(65, yvalue+=20));

        final Label labelhematite = new Label("Number of Bloodstone Tiles visible.", infof);
        add(labelhematite, new Coord(10, yvalue2+=20));
        labelcounthematite = new Label("0", Text.num12boldFnd, Color.WHITE);
        add(labelcounthematite, new Coord(65, yvalue+=20));

        final Label labelslimes = new Label("Number of Slimes Visible", infof);
        add(labelslimes, new Coord(10, yvalue2+=20));
        labelcountslimes = new Label("0", Text.num12boldFnd, Color.WHITE);
        add(labelcountslimes, new Coord(65, yvalue+=20));

        final Label labelslimestotal = new Label("Number of Slimes Total", infof);
        add(labelslimestotal, new Coord(10, yvalue2+=20));
        labelcountslimestotal = new Label("0", Text.num12boldFnd, Color.WHITE);
        add(labelcountslimestotal, new Coord(65, yvalue+=20));

        SupportsQuarter = new CheckBox("Stop Mining at <25% HP Supports"){
            {
                a = SupportAlertQuarter;
            }

            public void set(boolean val) {
                SupportAlertQuarter = val;
                a = val;
            }
        };
        add(SupportsQuarter,10,yvalue+=20);
        SupportsHalf = new CheckBox("Stop Mining at <50% HP Supports"){
            {
                a = SupportAlertHalf;
            }

            public void set(boolean val) {
                SupportAlertHalf = val;
                a = val;
            }
        };
        add(SupportsHalf,10,yvalue+=20);
        MarkTiles = new CheckBox("Mark ore tiles with arrows above them."){
            {
                a = MarkTileArrows;
            }

            public void set(boolean val) {
                MarkTileArrows = val;
                a = val;
            }
        };
        add(MarkTiles,10,yvalue+=20);
        runbtn = new Button(100, "Run") {
            @Override
            public void click() {
                terminate = false;
                runner = new Thread(new MinerAlert.runner(), "Miner Alert");
                runner.start();
            }
        };

        stopbtn = new Button(100, "Stop") {
            @Override
            public void click() {
                cbtn.show();
                terminate = true;
            }
        };

        mutebtn = new Button(100, "Mute") {
            @Override
            public void click() {
                audiomute = !audiomute;
                PBotUtils.sysMsg("Mute status : "+audiomute,Color.white);
            }
        };
        add(mutebtn, new Coord(35, yvalue+=20));
        runbtn.click();
    }

    private class runner implements Runnable {
        @Override
        public void run() {
            while (gui.getwnd("Miner Alert") != null) {
                PBotUtils.sleep(5000);//sleep 5 seconds every iteration, no reason to update more than once every 5 seconds.
                try {
                    if(PBotAPI.gui == null || PBotAPI.gui.ui == null)
                        break;
                    maxmarks = 50;
                    countiron = 0;
                    countgold = 0;
                    countsilver = 0;
                    Glob g = gui.map.glob;
                    Gob player = gui.map.player();
                    List<Gob> allGobs = PBotUtils.getGobs();
                    List<Gob> list = new ArrayList<>();
                    List<Gob> supportlist = new ArrayList<>();

                    for (int i = 0; i < allGobs.size(); i++) {
                        try {
                            if(allGobs.get(i).type.toString().contains("SUPPORT"))
                                supportlist.add(allGobs.get(i));
                            Resource res = allGobs.get(i).getres();
                            if (res.name.endsWith("greenooze") && !allGobs.get(i).isDead()) {
                                list.add(allGobs.get(i));
                                if (!slimecount.contains(allGobs.get(i)))
                                    slimecount.add(allGobs.get(i));
                            }
                        } catch (NullPointerException | Loading e) { }
                    }
                    countslimes = list.size();
                    while(PBotUtils.player() == null)
                        PBotUtils.sleep(10); //sleep if player is null, teleporting through a road?
                    Coord pltc = new Coord((int) player.getc().x / 11, (int) player.getc().y / 11);

                    if(SupportAlertHalf || SupportAlertQuarter){//if support alerts toggled, resolve mine supports and HP
                        for(Gob support : supportlist) {
                            double distFromPlayer = support.rc.dist(PBotUtils.player().rc);
                            if (distFromPlayer <= 13 * 11) {    //support is less than or equal to 13 tiles from current player position, check it's HP
                                if(support.getattr(GobHealth.class) != null && support.getattr(GobHealth.class).hp <= 2 && SupportAlertHalf){
                                    PBotUtils.sysMsg("Detected mine support at 50% or less HP",Color.ORANGE);
                                    synchronized (gui.map.glob.oc) {
                                        support.addol(new Mark(4000));
                                        support.delattr(GobHighlight.class);
                                        support.setattr(new GobHighlight(support));
                                    }
                                    if(PBotGobAPI.player().getPoses().contains("gfx/borka/choppan") || PBotGobAPI.player().getPoses().contains("gfx/borka/pickan")) {
                                        PBotAPI.gui.ui.root.wdgmsg("gk", 27);
                                        Audio.play(supportalertsfx);
                                    }
                                }else if(support.getattr(GobHealth.class) != null && support.getattr(GobHealth.class).hp <= 1 && SupportAlertQuarter){
                                    PBotUtils.sysMsg("Detected mine support at 25% or less HP less than 13 tiles away",Color.RED);
                                    synchronized (gui.map.glob.oc) {
                                        support.addol(new Mark(4000));
                                        support.delattr(GobHighlight.class);
                                        support.setattr(new GobHighlight(support));
                                    }
                                    if(PBotGobAPI.player().getPoses().contains("gfx/borka/choppan") || PBotGobAPI.player().getPoses().contains("gfx/borka/pickan")) {
                                        PBotAPI.gui.ui.root.wdgmsg("gk", 27);
                                        Audio.play(supportalertsfx);
                                    }
                                }
                            }
                        }
                    }

                    for (int x = -44; x < 44; x++) {
                        for (int y = -44; y < 44; y++) {
                            int t = g.map.gettile(pltc.sub(x, y));
                            Resource res = g.map.tilesetr(t);

                            if (res == null)
                                continue;
                            String name = res.name;

                            if(MarkTileArrows && reslist.contains(name) && maxmarks != 0){
                                final Coord2d mc = player.rc.sub(new Coord2d((x-1) * 11,(y-1) * 11)); //no clue why i have to subtract 1 tile here to get it to line up.
                                final Coord tc = mc.floor(MCache.tilesz);
                                final Coord2d tcd = mc.div(MCache.tilesz);

                                ui.sess.glob.map.getgridto(tc).ifPresent(grid -> {
                                    final Coord2d offset = tcd.sub(new Coord2d(grid.ul));
                                    ui.sess.glob.map.getgrido(grid.id).ifPresent(grid2 -> {
                                        final Coord2d mc2 = new Coord2d(grid2.ul).add(offset.x, offset.y).mul(MCache.tilesz);
                                        synchronized (ui.sess.glob.oc) {
                                            maxmarks--;
                                            final Gob g2 = ui.sess.glob.oc.new ModdedGob(mc2, 0);
                                            g2.addol(new Mark(4000));
                                            g2.addol(new GobCustomSprite(res.basename().substring(0,1).toUpperCase() + res.basename().substring(1)+" "+smeltchance.get(res.basename()),4000));
                                        }
                                    });
                                });
                            }

                            if (name.equals("gfx/tiles/rocks/cassiterite")) {
                                counttin = counttin + 1;
                                countcassiterite = countcassiterite + 1;
                            }
                            if (name.equals("gfx/tiles/rocks/chalcopyrite")) {
                                countiron = countiron + 1;
                                countcopper = countcopper + 1;
                                countchalcopyrite = countchalcopyrite + 1;
                            }
                            if (name.equals("gfx/tiles/rocks/malachite")) {
                                countcopper = countcopper + 1;
                                countmalachite = countmalachite + 1;
                            }
                            if (name.equals("gfx/tiles/rocks/ilmenite")) {
                                countiron = countiron + 1;
                                countilmenite = countilmenite + 1;
                            }
                            if (name.equals("gfx/tiles/rocks/limonite")) {
                                countiron = countiron + 1;
                                countlimonite = countlimonite + 1;
                            }
                            if (name.equals("gfx/tiles/rocks/hematite")) {
                                countiron = countiron + 1;
                                counthematite = counthematite + 1;
                            }
                            if (name.equals("gfx/tiles/rocks/magnetite")) {
                                countiron = countiron + 1;
                                countmagnetite = countmagnetite + 1;
                            }
                            if (name.equals("gfx/tiles/rocks/galena")) {
                                countsilver = countsilver + 1;
                                countgalena = countgalena + 1;
                            }
                            if (name.equals("gfx/tiles/rocks/argentite")) {
                                countsilver = countsilver + 1;
                                countargentite = countargentite + 1;
                            }
                            if (name.equals("gfx/tiles/rocks/hornsilver")) {
                                countsilver = countsilver + 1;
                                counthornsilver = counthornsilver + 1;
                            }
                            if (name.equals("gfx/tiles/rocks/petzite")) {
                                countgold = countgold + 1;
                                countpetzite = countpetzite + 1;
                            }
                            if (name.equals("gfx/tiles/rocks/sylvanite")) {
                                countgold = countgold + 1;
                                countsylvanite = countsylvanite + 1;
                            }
                            if (name.equals("gfx/tiles/rocks/nagyagite")) {
                                countgold = countgold + 1;
                                countnagyagite = countnagyagite + 1;
                            }
                            if (name.equals("gfx/tiles/rocks/cinnabar")) {
                                countcinnabar = countcinnabar + 1;
                            }

                        }
                    }
                    labelcountiron.settext(countiron + "");
                    labelcountcopper.settext(countcopper + "");
                    labelcounttin.settext(counttin + "");
                    labelcountgold.settext(countgold + "");
                    labelcountsilver.settext(countsilver + "");
                    labelcountmagnetite.settext(countmagnetite + "");
                    labelcounthematite.settext(counthematite + "");
                    labelcountslimes.settext(countslimes + "");
                    labelcountslimestotal.settext(slimecount.size() + "");
                    labelcountcinnabar.settext(countcinnabar + "");
                    if (countgold > 0) {
                        double now = Utils.rtime();
                        if (now - lasterror > 45) {
                            lasterror = now;
                            PBotUtils.sysMsg("Gold Visible on screen!!", Color.green);
                            if (!audiomute)
                                Audio.play(goldsfx);
                        }
                    }
                    if (countcinnabar > 0) {
                        double now = Utils.rtime();
                        if (now - lasterror > 45) {
                            PBotUtils.sysMsg("Cinnabar visible on screen!!", Color.green);
                            lasterror = now;
                        }
                    }
                    if (countsilver > 0) {
                        double now = Utils.rtime();
                        if (now - lasterror > 15) {
                            PBotUtils.sysMsg("Silver visible on screen!!", Color.green);
                            if (!audiomute) {
                                Audio.play(silversfx);
                            }
                            lasterror = now;
                        }
                    }
                    if (countslimes > 0) {
                        double now = Utils.rtime();
                        if (now - lasterror > 15) {
                            PBotUtils.sysLogAppend("Slime number spawned : " + list.size(), "white");
                            lasterror = now;
                        }
                    }
                    countiron = 0;
                    counttin = 0;
                    countcopper = 0;
                    countgold = 0;
                    countsilver = 0;
                    counthematite = 0;
                    countmagnetite = 0;
                    countcinnabar = 0;
                    countslimes = 0;
                }catch(Exception lolloadingerrors){
                    lolloadingerrors.printStackTrace();
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

    public void close() {
        reqdestroy();
    }

    @Override
    public boolean type(char key, KeyEvent ev) {
        if (key == 27) {//ignore escape key
            return true;
        }
        return super.type(key, ev);
    }

}

