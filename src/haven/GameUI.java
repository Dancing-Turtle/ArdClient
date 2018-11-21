/*
 *  This file is part of the Haven & Hearth game client.
 *  Copyright (C) 2009 Fredrik Tolf <fredrik@dolda2000.com>, and
 *                     Björn Johannessen <johannessen.bjorn@gmail.com>
 *
 *  Redistribution and/or modification of this file is subject to the
 *  terms of the GNU Lesser General Public License, version 3, as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  Other parts of this source tree adhere to other copying
 *  rights. Please see the file `COPYING' in the root directory of the
 *  source tree for details.
 *
 *  A copy the GNU Lesser General Public License is distributed along
 *  with the source tree of which this file is a part in the file
 *  `doc/LPGL-3'. If it is missing for any reason, please see the Free
 *  Software Foundation's website at <http://www.fsf.org/>, or write
 *  to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
 */

package haven;

import static haven.Action.*;
import static haven.Action.TOGGLE_KIN_LIST;
import static haven.Action.TOGGLE_OPTIONS;
import static haven.Inventory.invsq;

import haven.automation.Discord;
import haven.automation.ErrorSysMsgCallback;
import haven.automation.PickForageable;
import haven.livestock.LivestockManager;
import haven.purus.BotUtils;
import haven.purus.ItemClickCallback;
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotScriptlist;
import haven.resutil.FoodInfo;
import haven.automation.BeltDrink;
import static haven.KeyBinder.*;

import java.awt.*;
import java.util.*;
import java.awt.event.KeyEvent;
import java.awt.image.WritableRaster;
import java.util.List;

import static haven.Inventory.invsq;

public class GameUI extends ConsoleHost implements Console.Directory {
    public static final Text.Foundry msgfoundry = new Text.Foundry(Text.dfont, Text.cfg.msg);
    private static final int blpw = 142;
    public final String chrid, genus;
    public final long plid;
    private final Hidepanel ulpanel, umpanel, urpanel, brpanel, menupanel;
    public Avaview portrait;
    public MenuGrid menu;
    public MenuSearch menuSearch;
    public PBotScriptlist PBotScriptlist;
    public MapView map;
    public Fightview fv;
    private List<Widget> meters = new LinkedList<Widget>();
    private List<Widget> cmeters = new LinkedList<Widget>();
    private Text lastmsg;
    private double msgtime;
    public Window invwnd, equwnd;
    public FilterWnd filter;
    public Inventory maininv;
    public CharWnd chrwdg;
    public MapWnd mapfile;
    private Widget qqview;
    public BuddyWnd buddies;
    public Window craftbot;
    public Equipory equipory;
    public final Zergwnd zerg;
    public final Collection<Polity> polities = new ArrayList<Polity>();
    public HelpWnd help;
    public EquipProxy eqproxy;
    public OptWnd opts;
    public Collection<DraggedItem> hand = new LinkedList<DraggedItem>();
    private Collection<DraggedItem> handSave = new LinkedList<DraggedItem>();
    private int MinerDrinkTimer;
    public WItem vhand;
    public ChatUI chat;
    public Speedget speedget;
    public ChatUI.Channel syslog;
    public double prog = -1;
    private boolean afk = false;
    @SuppressWarnings("unchecked")
    public Indir<Resource>[] belt = new Indir[144];
    public Belt beltwdg = add(new NKeyBelt());
    public final Map<Integer, String> polowners = new HashMap<Integer, String>();
    public Bufflist buffs;
    public MinimapWnd minimapWnd;
    public LocalMiniMap mmap;
    public haven.timers.TimersWnd timerswnd;
    public QuickSlotsWdg quickslots;
    public StatusWdg statuswindow;
    public AlignPanel questpanel;
    public static boolean swimon = false;
    public static boolean crimeon = false;
    public static boolean trackon = false;
    public static boolean partyperm = false;
    public boolean crimeautotgld = false;
    public boolean swimautotgld = false;
    public boolean trackautotgld = false;
    public FBelt fbelt;
    public CraftHistoryBelt histbelt;
    private ErrorSysMsgCallback errmsgcb;
    public StudyWnd studywnd;
    public LivestockManager livestockwnd;
    public ItemClickCallback itemClickCallback;
    public CraftWindow makewnd;
    public ActWindow craftlist, buildlist, actlist;




    public abstract class Belt extends Widget {
        public Belt(Coord sz) {
            super(sz);
        }

        public void keyact(final int slot) {
            if (map != null) {
                Coord mvc = map.rootxlate(ui.mc);
                if (mvc.isect(Coord.z, map.sz)) {
                    map.delay(map.new Hittest(mvc) {
                        protected void hit(Coord pc, Coord2d mc, MapView.ClickInfo inf) {
                            Object[] args = {slot, 1, ui.modflags(), mc.floor(OCache.posres)};
                            if (inf != null)
                                args = Utils.extend(args, MapView.gobclickargs(inf));
                            GameUI.this.wdgmsg("belt", args);
                        }

                        protected void nohit(Coord pc) {
                            GameUI.this.wdgmsg("belt", slot, 1, ui.modflags());
                        }
                    });
                }
            }
        }
    }

    @RName("gameui")
    public static class $_ implements Factory {
        public Widget create(UI ui, Object[] args) {
            String chrid = (String) args[0];
            int plid = (Integer) args[1];
            String genus = "";
            if(args.length > 2)
                genus = (String)args[2];
            return (new GameUI(chrid, plid, genus));
        }
    }

    public GameUI(String chrid, long plid, String genus) {
        this.chrid = chrid;
        this.plid = plid;
        this.genus = genus;
        setcanfocus(true);
        setfocusctl(true);
        chat = add(new ChatUI(0, 0));
        if (Utils.getprefb("chatvis", true)) {
            chat.resize(0, chat.savedh);
            chat.show();
        }
        beltwdg.raise();
        ulpanel = add(new Hidepanel("gui-ul", null, new Coord(-1, -1)));
        umpanel = add(new Hidepanel("gui-um", null, new Coord(0, -1)) {
            @Override
            public Coord base() {
                if (base != null)
                    return base.get();
                return new Coord(parent.sz.x / 2 - this.sz.x / 2, 0);
            }
        });
        urpanel = add(new Hidepanel("gui-ur", null, new Coord(1, -1)));
        brpanel = add(new Hidepanel("gui-br", null, new Coord(1, 1)) {
            public void move(double a) {
                super.move(a);
                menupanel.move();
            }
        });
        menupanel = add(new Hidepanel("menu", new Indir<Coord>() {
            public Coord get() {
                return (new Coord(GameUI.this.sz.x, Math.min(brpanel.c.y - 79, GameUI.this.sz.y - menupanel.sz.y)));
            }
        }, new Coord(1, 0)));

        brpanel.add(new Img(Resource.loadtex("gfx/hud/brframe")), 0, 0);
        menupanel.add(new MainMenu(), 0, 0);

        portrait = ulpanel.add(new Avaview(Avaview.dasz, plid, "avacam") {
            public boolean mousedown(Coord c, int button) {
                return (true);
            }
        }, new Coord(10, 10));
        buffs = ulpanel.add(new Bufflist(), new Coord(95, 65));
        umpanel.add(new Cal(), new Coord(0, 10));
        add(new Widget(new Coord(300, 40)) {
            @Override
            public void draw(GOut g) {
                if (Config.showservertime) {
                    Tex time = ui.sess.glob.servertimetex;
                    if (time != null)
                        g.image(time, new Coord(300 / 2 - time.sz().x / 2, 0));
                }
            }
        }, new Coord(HavenPanel.w / 2 - 300 / 2, umpanel.sz.y));
        filter = new FilterWnd();
        add(filter,300,300);
        syslog = chat.add(new ChatUI.Log(Resource.getLocString(Resource.BUNDLE_LABEL, "System")));
        opts = add(new OptWnd());
        opts.hide();
        zerg = add(new Zergwnd(), 187, 50);
        zerg.hide();


        timerswnd = new haven.timers.TimersWnd(this);
        timerswnd.hide();
        add(timerswnd, new Coord(HavenPanel.w / 2 - timerswnd.sz.x / 2, 100));

        livestockwnd = new LivestockManager();
        livestockwnd.hide();
        add(livestockwnd, new Coord(HavenPanel.w / 2 - timerswnd.sz.x / 2, 100));

        quickslots = new QuickSlotsWdg();
        if (!Config.quickslots)
            quickslots.hide();
        add(quickslots, Utils.getprefc("quickslotsc", new Coord(430, HavenPanel.h - 160)));

        if (Config.statuswdgvisible) {
            statuswindow = new StatusWdg();
            add(statuswindow, new Coord(HavenPanel.w / 2 + 80, 10));
        }

        if (!chrid.equals("")) {
            Utils.loadprefchklist("boulderssel_" + chrid, Config.boulders);
            Utils.loadprefchklist("bushessel_" + chrid, Config.bushes);
            Utils.loadprefchklist("treessel_" + chrid, Config.trees);
            Utils.loadprefchklist("iconssel_" + chrid, Config.icons);
            opts.setMapSettings();
        }




        fbelt = new FBelt(chrid, Utils.getprefb("fbelt_vertical", true));
        fbelt.loadLocal();
        add(fbelt, Utils.getprefc("fbelt_c", new Coord(20, 200)));
        if (!Config.fbelt)
            fbelt.hide();

        histbelt = new CraftHistoryBelt(Utils.getprefb("histbelt_vertical", true));
        add(histbelt, Utils.getprefc("histbelt_c", new Coord(70, 200)));
        if (!Config.histbelt)
            histbelt.hide();

        menuSearch = new MenuSearch();
        add(menuSearch, 300, 300);
        menuSearch.hide();

        PBotScriptlist = new PBotScriptlist();
        add(PBotScriptlist, 300, 300);
        PBotScriptlist.hide();
        BotUtils.gui = this;
        PBotAPI.gui = this;
        if(!chat.visible) {
            chat.resize(0, chat.savedh);
            chat.show();
        }
    }

    @Override
    protected void attach(UI ui) {
        super.attach(ui);
        ui.gui = this;
    }

    protected void added() {
        resize(parent.sz);
        ui.cons.out = new java.io.PrintWriter(new java.io.Writer() {
            StringBuilder buf = new StringBuilder();

            public void write(char[] src, int off, int len) {
                List<String> lines = new ArrayList<String>();
                synchronized(this) {
                    buf.append(src, off, len);
                    int p;
                    while((p = buf.indexOf("\n")) >= 0) {
                        lines.add(buf.substring(0, p));
                        buf.delete(0, p + 1);
                    }
                }
                for(String ln : lines)
                    syslog.append(ln, Color.WHITE);
            }

            public void close() {
            }

            public void flush() {
            }
        });
        Debug.log = ui.cons.out;
        opts.c = sz.sub(opts.sz).div(2);
    }

   /* public void toggleCraftList() {
        if(craftlist == null){
            craftlist = add(new ActWindow("Craft...", "paginae/craft/.+"));
            craftlist.addtwdg(craftlist.add(new IButton("gfx/hud/btn-help", "","-d","-h"){
                @Override
                public void click() {
                    ItemFilter.showHelp(ui, HELP_SIMPLE, HELP_CURIO, HELP_FEP, HELP_ARMOR, HELP_SYMBEL, HELP_ATTR);
                }
            }));
        } else if(craftlist.visible) {
            craftlist.hide();
        } else {
            craftlist.show();
        }
    }*/

    /*public void toggleBuildList() {
        if(buildlist == null){
            buildlist = add(new ActWindow("Build...", "paginae/bld/.+"));
        } else if(buildlist.visible) {
            buildlist.hide();
        } else {
            buildlist.show();
        }
    }


    public void toggleActList() {
        if(actlist == null){
            actlist = add(new ActWindow("Act...", "paginae/act/.+|paginae/pose/.+|paginae/gov/.+|paginae/add/.+|gfx/fx/msrad|ui/tt/q/quality"));
        } else if(actlist.visible) {
            actlist.hide();
        } else {
            actlist.show();
        }
    }*/

    public void toggleDebug(){
        Config.dbtext = !Config.dbtext;
    }

    public void toggleInventory() {
        if((invwnd != null) && invwnd.show(!invwnd.visible)) {
            invwnd.raise();
            fitwdg(invwnd);
        }
    }

    public void toggleEquipment() {
        if((equwnd != null) && equwnd.show(!equwnd.visible)) {
            equwnd.raise();
            fitwdg(equwnd);
        }
    }

    public void toggleCharacter() {
        if((chrwdg != null) && chrwdg.show(!chrwdg.visible)) {
            chrwdg.raise();
            fitwdg(chrwdg);
        }
    }

    public void toggleKinList() {
        if(zerg.show(!zerg.visible)) {
            zerg.raise();
            fitwdg(zerg);
            setfocus(zerg);
        }
    }

    public void toggleOptions() {
        if(opts.show(!opts.visible)) {
            opts.raise();
            fitwdg(opts);
            setfocus(opts);
        }
    }

    public void toggleChat() {
        if(chat.visible && !chat.hasfocus) {
            setfocus(chat);
        } else {
            if(chat.targeth == 0) {
                chat.sresize(chat.savedh);
                setfocus(chat);
            } else {
                chat.sresize(0);
            }
        }
        Utils.setprefb("chatvis", chat.targeth != 0);
    }

  //  public void toggleCraftDB() {
    //    if(craftwnd == null) {
      //      craftwnd = add(new CraftDBWnd());
       // } else {
        //    craftwnd.close();
      //  }
   // }

    public void toggleMap() {
        if((mapfile != null) && mapfile.show(!mapfile.visible)) {
            mapfile.raise();
            fitwdg(mapfile);
            setfocus(mapfile);
        }
    }

    public class Hidepanel extends Widget {
        public final String id;
        public final Coord g;
        public final Indir<Coord> base;
        public boolean tvis;
        private double cur;

        public Hidepanel(String id, Indir<Coord> base, Coord g) {
            this.id = id;
            this.base = base;
            this.g = g;
            cur = show(tvis = true) ? 0 : 1;
        }



        public <T extends Widget> T add(T child) {
            super.add(child);
            pack();
            if (parent != null)
                move();
            return (child);
        }

        public Coord base() {
            if (base != null) return (base.get());
            return (new Coord((g.x > 0) ? parent.sz.x : (g.x < 0) ? 0 : (parent.sz.x / 2),
                    (g.y > 0) ? parent.sz.y : (g.y < 0) ? 0 : (parent.sz.y / 2)));
        }

        public void move(double a) {
            cur = a;
            Coord c = new Coord(base());
            if (g.x < 0)
                c.x -= (int) (sz.x * a);
            else if (g.x > 0)
                c.x -= (int) (sz.x * (1 - a));
            if (g.y < 0)
                c.y -= (int) (sz.y * a);
            else if (g.y > 0)
                c.y -= (int) (sz.y * (1 - a));
            this.c = c;
        }

        public void move() {
            move(cur);
        }

        public void presize() {
            move();
        }

        public boolean mshow(final boolean vis) {
            clearanims(Anim.class);
            if (vis)
                show();
            new NormAnim(0.25) {
                final double st = cur, f = vis ? 0 : 1;

                public void ntick(double a) {
                    if ((a == 1.0) && !vis)
                        hide();
                    move(st + (Utils.smoothstep(a) * (f - st)));
                }
            };
            tvis = vis;
            return (vis);
        }

        public boolean cshow(boolean vis) {
            if (vis != tvis)
                mshow(vis);
            return (vis);
        }

        public void cdestroy(Widget w) {
            parent.cdestroy(w);
        }
    }

    public static class Hidewnd extends Window {
        Hidewnd(Coord sz, String cap, boolean lg) {
            super(sz, cap, lg);
        }

        protected Hidewnd(Coord sz, String cap) {
            super(sz, cap);
        }

        public void wdgmsg(Widget sender, String msg, Object... args) {
            if ((sender == this) && msg.equals("close")) {
                this.hide();
                return;
            }
            super.wdgmsg(sender, msg, args);
        }
    }

    static class Zergwnd extends Hidewnd {
        Tabs tabs = new Tabs(Coord.z, Coord.z, this);
        final TButton kin, pol, pol2;

        class TButton extends IButton {
            Tabs.Tab tab = null;
            final Tex inv;

            TButton(String nm, boolean g) {
                super(Resource.loadimg("gfx/hud/buttons/" + nm + "u"), Resource.loadimg("gfx/hud/buttons/" + nm + "d"));
                if (g)
                    inv = Resource.loadtex("gfx/hud/buttons/" + nm + "g");
                else
                    inv = null;
            }

            public void draw(GOut g) {
                if ((tab == null) && (inv != null))
                    g.image(inv, Coord.z);
                else
                    super.draw(g);
            }

            public void click() {
                if (tab != null) {
                    tabs.showtab(tab);
                    repack();
                }
            }
        }

        Zergwnd() {
            super(Coord.z, "Kith & Kin", true);
            kin = add(new TButton("kin", false));
            kin.tooltip = Text.render("Kin");
            pol = add(new TButton("pol", true));
            pol2 = add(new TButton("rlm", true));
        }

        private void repack() {
            tabs.indpack();
            kin.c = new Coord(0, tabs.curtab.contentsz().y + 20);
            pol.c = new Coord(kin.c.x + kin.sz.x + 10, kin.c.y);
            pol2.c = new Coord(pol.c.x + pol.sz.x + 10, pol.c.y);
            this.pack();
        }

        Tabs.Tab ntab(Widget ch, TButton btn) {
            Tabs.Tab tab = add(tabs.new Tab() {
                public void cresize(Widget ch) {
                    repack();
                }
            }, tabs.c);
            tab.add(ch, Coord.z);
            btn.tab = tab;
            repack();
            return (tab);
        }

        void dtab(TButton btn) {
            btn.tab.destroy();
            btn.tab = null;
            repack();
        }

        void addpol(Polity p) {
	        /* This isn't very nice. :( */
            TButton btn = p.cap.equals("Village")?pol:pol2;
            ntab(p, btn);
            btn.tooltip = Text.render(p.cap);
        }

        @Override
        public boolean show(boolean show) {
           if (show)
                gameui().buddies.clearSearch();
            return super.show(show);
        }
    }

   public static class DraggedItem {
       public final GItem item;
        final Coord dc;

        DraggedItem(GItem item, Coord dc) {
            this.item = item;
            this.dc = dc;
        }
    }

    private void updhand() {
        if ((hand.isEmpty() && (vhand != null)) || ((vhand != null) && !hand.contains(vhand.item))) {
            ui.destroy(vhand);
            vhand = null;
        }
        if (!hand.isEmpty() && (vhand == null)) {
            DraggedItem fi = hand.iterator().next();
            vhand = add(new ItemDrag(fi.dc, fi.item));
            if (map.lastItemactClickArgs != null)
                map.iteminteractreplay();
        }
    }
    public void DiscordToggle(){
        if(Discord.jdalogin != null) {
            BotUtils.sysMsg("Discord Disconnected",Color.white);
            Discord.jdalogin.shutdownNow();
            for(int i=0;i<15;i++) {
                for (Widget w = chat.lchild; w != null; w = w.prev) {
                    if (w instanceof ChatUI.DiscordChat) {
                        w.destroy();
                    }
                }
            }
        }
    }





    private String mapfilename() {
        StringBuilder buf = new StringBuilder();
        buf.append(genus);
        String chrid = Utils.getpref("mapfile/" + this.chrid, "");
        if (!chrid.equals("")) {
            if (buf.length() > 0) buf.append('/');
            buf.append(chrid);
        }
        return (buf.toString());
    }
    public void addcmeter(Widget meter) {
        ulpanel.add(meter);
        cmeters.add(meter);
        updcmeters();
    }

    public void toggleHand(){
        if (hand.isEmpty()) {
            hand.addAll(handSave);
            handSave.clear();
            updhand();
        } else {
            handSave.addAll(hand);
            hand.clear();
            updhand();
        }
    }

    public void toggleStudy() {
        studywnd.show(!studywnd.visible);
    }
    public void takeScreenshot() {
        if(Config.screenurl != null) {
            Screenshooter.take(this, Config.screenurl);
        }
    }

    public void localScreenshot(){
        if(Config.screenurl != null)
        HavenPanel.needtotakescreenshot = true;
    }

    public <T extends Widget> void delcmeter(Class<T> cl) {
        Widget widget = null;
        for (Widget meter : cmeters) {
            if (cl.isAssignableFrom(meter.getClass())) {
                widget = meter;
                break;
            }
        }
        if (widget != null) {
            cmeters.remove(widget);
            widget.destroy();
            updcmeters();
        }
    }


    private Coord getMeterPos(int x, int y) {
        return new Coord(portrait.c.x + portrait.sz.x + 10 + x * (IMeter.fsz.x + 5), portrait.c.y + y * (IMeter.fsz.y + 2));
    }

    public void addMeterAt(Widget m, int x, int y) {
        ulpanel.add(m, getMeterPos(x, y));
        ulpanel.pack();
    }



    private void updcmeters() {
        int i = 0;
        for (Widget meter : cmeters) {
            int x = ((meters.size() + i) % 3) * (IMeter.fsz.x + 5);
            int y = ((meters.size() + i) / 3) * (IMeter.fsz.y + 2);
            meter.c = new Coord(portrait.c.x + portrait.sz.x + 10 + x, portrait.c.y + y);
            i++;
        }
    }
    public Coord optplacement(Widget child, Coord org) {
        Set<Window> closed = new HashSet<>();
        Set<Coord> open = new HashSet<>();
        open.add(org);
        Coord opt = null;
        double optscore = Double.NEGATIVE_INFINITY;
        Coord plc = null;
        {
            Gob pl = map.player();
            if (pl != null)
                plc = pl.sc;
        }
        Area parea = Area.sized(Coord.z, sz);
        while (!open.isEmpty()) {
            Coord cur = Utils.take(open);
            double score = 0;
            Area tarea = Area.sized(cur, child.sz);
            if (parea.isects(tarea)) {
                double outside = 1.0 - (((double) parea.overlap(tarea).area()) / ((double) tarea.area()));
                if ((outside > 0.75) && !cur.equals(org))
                    continue;
                score -= Math.pow(outside, 2) * 100;
            } else {
                if (!cur.equals(org))
                    continue;
                score -= 100;
            }
            {
                boolean any = false;
                for (Widget wdg = this.child; wdg != null; wdg = wdg.next) {
                    if (!(wdg instanceof Window))
                        continue;
                    Window wnd = (Window) wdg;
                    if (!wnd.visible)
                        continue;
                    Area warea = wnd.parentarea(this);
                    if (warea.isects(tarea)) {
                        any = true;
                        score -= ((double) warea.overlap(tarea).area()) / ((double) tarea.area());
                        if (!closed.contains(wnd)) {
                            open.add(new Coord(wnd.c.x - child.sz.x, cur.y));
                            open.add(new Coord(cur.x, wnd.c.y - child.sz.y));
                            open.add(new Coord(wnd.c.x + wnd.sz.x, cur.y));
                            open.add(new Coord(cur.x, wnd.c.y + wnd.sz.y));
                            closed.add(wnd);
                        }
                    }
                }
                if (!any)
                    score += 10;
            }
            if (plc != null) {
                if (tarea.contains(plc))
                    score -= 100;
                else
                    score -= (1 - Math.pow(tarea.closest(plc).dist(plc) / sz.dist(Coord.z), 2)) * 1.5;
            }
            score -= (cur.dist(org) / sz.dist(Coord.z)) * 0.75;
            if (score > optscore) {
                optscore = score;
                opt = cur;
            }
        }
        return (opt);
    }

    public void addchild(Widget child, Object... args) {
        String place = ((String) args[0]).intern();
        if (place == "mapview") {
            child.resize(sz);
            map = add((MapView) child, Coord.z);
            map.lower();
            if (minimapWnd != null)
                ui.destroy(minimapWnd);
            if(mapfile != null) {
                ui.destroy(mapfile);
                mapfile = null;
            }
            minimapWnd = minimap();
            mmap = minimapWnd.mmap;
            if(ResCache.global != null) {
                MapFile file = MapFile.load(ResCache.global, mapfilename());
                mmap.save(file);
                mapfile = new MapWnd(mmap.save, map, new Coord(700, 500), "Map");
                mapfile.hide();
                add(mapfile, 50, 50);
                minimapWnd.mapfile = mapfile;
            }

            if (trackon) {
                buffs.addchild(new Buff(Bufflist.bufftrack.indir()));
                msgnosfx(Resource.getLocString(Resource.BUNDLE_MSG, "Tracking is now turned on."));
            }
            if (crimeon) {
                buffs.addchild(new Buff(Bufflist.buffcrime.indir()));
                msgnosfx(Resource.getLocString(Resource.BUNDLE_MSG, "Criminal acts are now turned on."));
            }
            if (swimon) {
                buffs.addchild(new Buff(Bufflist.buffswim.indir()));
                msgnosfx(Resource.getLocString(Resource.BUNDLE_MSG, "Swimming is now turned on."));
            }
            if (partyperm) {
                buffs.addchild(new Buff(Bufflist.partyperm.indir()));
                msgnosfx(Resource.getLocString(Resource.BUNDLE_MSG, "Party permissions are now turned on."));
            }
        } else if (place == "menu") {
            menu = (MenuGrid)brpanel.add(child, 20, 34);
        } else if (place == "fight") {
            fv = urpanel.add((Fightview) child, 0, 0);
        } else if (place == "fsess") {
            add(child, Coord.z);
        } else if (place == "inv") {
            invwnd = new Hidewnd(Coord.z, "Inventory") {
                public void cresize(Widget ch) {
                    pack();
                }
            };
            invwnd.add(maininv = (Inventory) child, Coord.z);
            invwnd.pack();
            invwnd.show(Config.showinvonlogin);
            add(invwnd, new Coord(100, 100));
        } else if (place == "equ") {
            equwnd = new Hidewnd(Coord.z, "Equipment");
            equipory = equwnd.add((Equipory) child, Coord.z);
          //  equwnd.add(child, Coord.z);
            equwnd.pack();
            equwnd.hide();
            add(equwnd, new Coord(400, 10));
            equwnd.show(Config.logincharsheet);
        } else if (place == "hand") {
            GItem g = add((GItem) child);
            Coord lc = (Coord) args[1];
            hand.add(new DraggedItem(g, lc));
            updhand();
        } else if (place == "chr") {
        	studywnd = add(new StudyWnd(), new Coord(400,100));
        	if(!Config.loginstudywnd)
        	studywnd.hide();
            chrwdg = add((CharWnd) child, new Coord(300, 50));
            chrwdg.hide();
            if(Config.hungermeter)
            	addcmeter(new HungerMeter(chrwdg.glut));
            if(Config.fepmeter)
            	addcmeter(new FepMeter(chrwdg.feps));
        } else if (place == "craft") {
            //final Widget mkwdg = child;
                if(makewnd == null) {
                    makewnd = add(new CraftWindow(), new Coord(400, 200));
                }
                makewnd.add(child);
                makewnd.pack();
                makewnd.raise();
                makewnd.show();

            /*final Widget mkwdg = child;
            makewnd = new Window(Coord.z, "Crafting", true) {
                public void wdgmsg(Widget sender, String msg, Object... args) {
                    if ((sender == this) && msg.equals("close")) {
                        mkwdg.wdgmsg("close");
                        return;
                    }
                    super.wdgmsg(sender, msg, args);
                }

                public void cdestroy(Widget w) {
                    if (w == mkwdg) {
                        ui.destroy(this);
                        makewnd = null;
                    }
                }
            };
            makewnd.add(mkwdg, Coord.z);
            makewnd.pack();
            add(makewnd, new Coord(400, 200));*/
        } else if (place == "buddy") {
            zerg.ntab(buddies = (BuddyWnd) child, zerg.kin);
        } else if (place == "pol") {
            Polity p = (Polity)child;
            polities.add(p);
            zerg.addpol(p);
        } else if (place == "chat") {
            ChatUI.Channel prevchannel = chat.sel;
            chat.addchild(child);
            if (prevchannel != null && chat.sel.cb == null) {
                chat.select(prevchannel);
            }
        } else if (place == "party") {
            add(child, 10, 95);
        } else if (place == "meter") {
            int x = (meters.size() % 3) * (IMeter.fsz.x + 5);
            int y = (meters.size() / 3) * (IMeter.fsz.y + 2);
            ulpanel.add(child, portrait.c.x + portrait.sz.x + 10 + x, portrait.c.y + y);
            meters.add(child);
            updcmeters();
        } else if (place == "buff") {
            buffs.addchild(child);
        } else if (place == "qq") {
            if (qqview != null)
                qqview.reqdestroy();
            final Widget cref = qqview = child;
            questpanel = new AlignPanel() {
                {
                    add(cref);
                }

                protected Coord getc() {
                    return (new Coord(10, GameUI.this.sz.y - chat.sz.y - beltwdg.sz.y - this.sz.y - 10));
                }

                public void cdestroy(Widget ch) {
                    qqview = null;
                    destroy();
                }
            };
            if (Config.noquests)
                questpanel.hide();

            add(questpanel);
        } else if (place == "misc") {
            Coord c;
            if(args[1] instanceof Coord) {
                c = (Coord)args[1];
            } else if(args[1] instanceof Coord2d) {
                c = ((Coord2d)args[1]).mul(new Coord2d(this.sz.sub(child.sz))).round();
                c = optplacement(child, c);
            } else if(args[1] instanceof String) {
                c = relpos((String)args[1], child, (args.length > 2) ? ((Object[])args[2]) : new Object[] {}, 0);
            } else {
                throw(new UI.UIException("Illegal gameui child", place, args));
            }
            add(child, c);
        } else if(place == "abt") {
            add(child, Coord.z);
        }
            else {
            throw (new UI.UIException("Illegal gameui child", place, args));
        }
    }

    private MinimapWnd minimap() {
        Coord mwsz = Utils.getprefc("mmapwndsz", new Coord(290, 310));
        minimapWnd = new MinimapWnd(mwsz, map);
        add(minimapWnd, Utils.getprefc("mmapc", new Coord(10, 100)));
        mmap = (LocalMiniMap)minimapWnd.mmap;
        return minimapWnd;
    }

    public void cdestroy(Widget w) {
        if (w instanceof GItem) {
            for (Iterator<DraggedItem> i = hand.iterator(); i.hasNext(); ) {
                DraggedItem di = i.next();
                if (di.item == w) {
                    i.remove();
                    updhand();
                }
            }
        } else if (polities.contains(w)) {
            polities.remove(w);
            zerg.dtab(zerg.pol);
        } else if (w == chrwdg) {
            chrwdg = null;
        }
        if(meters.remove(w))
        	updcmeters();
        cmeters.remove(w);
    }

    private static final Resource.Anim progt = Resource.local().loadwait("gfx/hud/prog").layer(Resource.animc);
    private Tex curprog = null;
    private int curprogf, curprogb;

    private void drawprog(GOut g, double prog) {
        int fr = Utils.clip((int) Math.floor(prog * progt.f.length), 0, progt.f.length - 2);
        int bf = Utils.clip((int) (((prog * progt.f.length) - fr) * 255), 0, 255);
        if ((curprog == null) || (curprogf != fr) || (curprogb != bf)) {
            if (curprog != null)
                curprog.dispose();
            WritableRaster buf = PUtils.imgraster(progt.f[fr][0].sz);
            PUtils.blit(buf, progt.f[fr][0].img.getRaster(), Coord.z);
            PUtils.blendblit(buf, progt.f[fr + 1][0].img.getRaster(), Coord.z, bf);
            curprog = new TexI(PUtils.rasterimg(buf));
            curprogf = fr;
            curprogb = bf;
        }
        Coord hgc = new Coord(sz.x / 2, (sz.y * 4) / 10);
        g.aimage(curprog, hgc, 0.5, 0.5);
        if (Config.showprogressperc)
            g.atextstroked((int) (prog * 100) + "%", hgc, 0.5, 2.5, Color.WHITE, Color.BLACK, Text.num12boldFnd);
        }

    public void draw(GOut g) {
        beltwdg.c = new Coord(chat.c.x, Math.min(chat.c.y - beltwdg.sz.y + 4, sz.y - beltwdg.sz.y));
        super.draw(g);
        if (prog >= 0)
            drawprog(g, prog);
        int by = sz.y;
        if (chat.visible)
            by = Math.min(by, chat.c.y);
        if (beltwdg.visible)
            by = Math.min(by, beltwdg.c.y);
        if (cmdline != null) {
            drawcmd(g, new Coord(blpw + 10, by -= 20));
        } else if (lastmsg != null) {
            if ((Utils.rtime() - msgtime) > 3.0) {
                lastmsg = null;
            } else {
                g.chcolor(0, 0, 0, 192);
                g.frect(new Coord(blpw + 8, by - 22), lastmsg.sz().add(4, 4));
                g.chcolor();
                g.image(lastmsg.tex(), new Coord(blpw + 10, by -= 20));
            }
        }
        if (!chat.visible) {
            chat.drawsmall(g, new Coord(blpw + 10, by), 50);
        }
    }

    public void tick(double dt) {
        super.tick(dt);
        Resource curs = ui.root.getcurs(Coord.z);
        if (curs != null && curs.name.equals("gfx/hud/curs/mine")) {
        MinerDrinkTimer++;
        if(MinerDrinkTimer > 300) {
            MinerDrinkTimer = 0;
                IMeter.Meter stam = getmeter("stam", 0);
                if (stam.a < 60) {
                    Drink();
                }
            }
        }
        double idle = Utils.rtime() - ui.lastevent;
        if (!afk && (idle > 300)) {
            afk = true;
            wdgmsg("afk");
            if(Config.afklogout)
                logoutChar();
        } else if (afk && (idle <= 300)) {
            afk = false;
        }
    }

    private void togglebuff(String err, Resource res) {
        String name = res.basename();
        if (err.endsWith("on.") && buffs.gettoggle(name) == null) {
            buffs.addchild(new Buff(res.indir()));
            if (name.equals("swim"))
                swimon = true;
            else if (name.equals("crime"))
                crimeon = true;
            else if (name.equals("tracking"))
                trackon = true;
        } else if (err.endsWith("off.")) {
            Buff tgl = buffs.gettoggle(name);
            if (tgl != null)
                tgl.reqdestroy();
            if (name.equals("swim"))
                swimon = false;
            else if (name.equals("crime"))
                crimeon = false;
            else if (name.equals("tracking"))
                trackon = false;
        }
    }

    public void uimsg(String msg, Object... args) {
        if (msg == "err") {
            error((String) args[0]);
        } else if (msg == "msg") {
            String text = (String) args[0];
            if (text.startsWith("Swimming is now turned")) {
                togglebuff(text, Bufflist.buffswim);
                if (swimautotgld) {
                    msgnosfx(text);
                    swimautotgld = false;
                    return;
                }
            } else if (text.startsWith("Tracking is now turned")) {
                togglebuff(text, Bufflist.bufftrack);
                if (trackautotgld) {
                    msgnosfx(text);
                    trackautotgld = false;
                    return;
                }
            } else if (text.startsWith("Criminal acts are now turned")) {
                togglebuff(text, Bufflist.buffcrime);
                if (crimeautotgld) {
                    msgnosfx(text);
                    crimeautotgld = false;
                    return;
                }
            } else if (text.startsWith("Party permissions are now")) {
                togglebuff(text, Bufflist.partyperm);
            }
            msg(text);
        } else if (msg == "prog") {
            if (args.length > 0)
                prog = ((Number) args[0]).doubleValue() / 100.0;
            else
                prog = -1;
        } else if (msg == "setbelt") {
            int slot = (Integer) args[0];
            if (args.length < 2) {
                belt[slot] = null;
                if (fbelt != null)
                    fbelt.delete(slot);
            } else {
                belt[slot] = ui.sess.getres((Integer) args[1]);
                if (fbelt != null)
                    fbelt.add(slot, belt[slot]);
            }
        } else if (msg == "polowner") {
            int id = (Integer)args[0];
            String o = (String)args[1];
            boolean n = ((Integer)args[2]) != 0;
            if(o != null)
                o = o.intern();
            String cur = polowners.get(id);
            if(map != null) {
                if((o != null) && (cur == null)) {
                    map.setpoltext(id, "Entering " + o);
                } else if((o == null) && (cur != null)) {
                    map.setpoltext(id, "Leaving " + cur);
                }
            }
            polowners.put(id, o);
        } else if (msg == "showhelp") {
            Indir<Resource> res = ui.sess.getres((Integer) args[0]);
            if (help == null)
                help = adda(new HelpWnd(res), 0.5, 0.5);
            else
                help.res = res;
        } else if(msg == "map-mark") {
            long gobid = ((Integer)args[0]) & 0xffffffff;
            long oid = (Long)args[1];
            Indir<Resource> res = ui.sess.getres((Integer)args[2]);
            String nm = (String)args[3];
            if(mapfile != null)
                mapfile.markobj(gobid, oid, res, nm);
        } else {
            super.uimsg(msg, args);
        }
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
   /* System.out.println("############");
    	if(!sender.toString().contains("Camera"))
        System.out.println(sender);
    	System.out.println(msg);
    	for(Object o :args)
    		System.out.println(o);*/
        if ((sender == chrwdg) && (msg == "close")) {
            chrwdg.hide();
        } else if((polities.contains(sender)) && (msg == "close")) {
            sender.hide();
        } else if ((sender == help) && (msg == "close")) {
            ui.destroy(help);
            help = null;
            return;
        }
        super.wdgmsg(sender, msg, args);
    }

    public void fitwdg(Widget wdg) {
        if (wdg.c.x < 0)
            wdg.c.x = 0;
        if (wdg.c.y < 0)
            wdg.c.y = 0;
        if (wdg.c.x + wdg.sz.x > sz.x)
            wdg.c.x = sz.x - wdg.sz.x;
        if (wdg.c.y + wdg.sz.y > sz.y)
            wdg.c.y = sz.y - wdg.sz.y;
    }

    private static final Tex menubg = Resource.loadtex("gfx/hud/rbtn-bg");

    public static class MenuButton2 extends IButton {
        private final Action action;
        private final String tip;

        MenuButton2(String base, String tooltip, Action action) {
            super("gfx/hud/" + base, "", "-d", "-h");
            this.action = action;
            this.tip = tooltip;
           // KeyBinder.add(code, mods, action);
        }

        @Override
        public Object tooltip(Coord c, Widget prev) {
            if(!checkhit(c)) {
                return null;
            }
            KeyBinder.KeyBind bind = KeyBinder.get(action);
            String tt = tip;
            if(bind != null && !bind.isEmpty()) {
                tt = String.format("%s ($col[255,255,0]{%s})", tip, bind.shortcut());
            }
            return RichText.render(tt, 0);
        }

        @Override
        public void click() {
            action.run(ui.gui);
        }
    }

    public class MainMenu extends Widget {
        public MainMenu() {
            super(menubg.sz());
            add(new MenuButton2("rbtn-inv", "Inventory", TOGGLE_INVENTORY) , 0, 0);
            add(new MenuButton2("rbtn-equ", "Equipment", TOGGLE_EQUIPMENT),0, 0);
            add(new MenuButton2("rbtn-chr", "Character Sheet", TOGGLE_CHARACTER), 0, 0);
            add(new MenuButton2("rbtn-bud", "Kith & Kin", TOGGLE_KIN_LIST), 0, 0);
            add(new MenuButton2("rbtn-opt", "Options", TOGGLE_OPTIONS), 0, 0);
            add(new MenuButton2("rbtn-dwn", "Menu Search",TOGGLE_SEARCH),0,0);
        }

        public void draw(GOut g) {
            g.image(menubg, Coord.z);
            super.draw(g);
        }
    }

    public void toggleGobs(){
        Config.showboundingboxes = !Config.showboundingboxes;
        Utils.setprefb("showboundingboxes", Config.showboundingboxes);
        if (map != null)
            map.refreshGobsAll();
    }

    public void toggleMute(){
        if(Audio.volume > 0) {
            BotUtils.sysMsg("Audio muted.",Color.white);
            Audio.volume = 0;
        }
        else {
            Audio.volume = Double.parseDouble(Utils.getpref("sfxvol", "1.0"));
            BotUtils.sysMsg("Audio un-muted.",Color.white);
        }
    }

    public void logout(){
        if(Discord.jdalogin != null)
            DiscordToggle();
        act("lo");
    }
    public void logoutChar() {
        if(Discord.jdalogin != null)
            DiscordToggle();
        act("lo", "cs");
    }

    public void toggleTreeStage(){
        Config.showplantgrowstage = !Config.showplantgrowstage;
        Utils.setprefb("showplantgrowstage", Config.showplantgrowstage);
        if (!Config.showplantgrowstage && map != null)
            map.removeCustomSprites(Sprite.GROWTH_STAGE_ID);
        if (map != null)
            map.refreshGobsGrowthStages();
    }

    public void toggleSearch(){
    if (menuSearch.show(!menuSearch.visible)) {
       menuSearch.raise();
       fitwdg(menuSearch);
        setfocus(menuSearch);
    }
    }

    public void toggleDaylight(){
        Config.daylight = !Config.daylight;
        Utils.setprefb("daylight", Config.daylight);
    }

    public void toggleUI(){
        TexGL.disableall = !TexGL.disableall;
    }
    public void harvestForageable(){
        Thread t = new Thread(new PickForageable(this), "PickForageable");
        t.start();
    }

    public void toggleDangerRadius(){
        Config.showminerad = !Config.showminerad;
        Utils.setprefb("showminerad", Config.showminerad);
    }

    public void toggleSafeRadius(){
        Config.showfarmrad = !Config.showfarmrad;
        Utils.setprefb("showfarmrad", Config.showfarmrad);
    }

    public void toggleStatusWidget(){
        if (Config.statuswdgvisible) {
            if (statuswindow != null)
                statuswindow.reqdestroy();
            Config.statuswdgvisible = false;
            Utils.setprefb("statuswdgvisible", false);
        } else {
            statuswindow = new StatusWdg();
            add(statuswindow, new Coord(HavenPanel.w / 2 + 80, 10));
            Config.statuswdgvisible = true;
            Utils.setprefb("statuswdgvisible", true);
        }
    }

    public void toggleHide(){
        Config.hidegobs = !Config.hidegobs;
        Utils.setprefb("hidegobs", Config.hidegobs);
        if (map != null)
            map.refreshGobsHidable();
    }

    public void toggleGridCentering(){
        Config.tilecenter = !Config.tilecenter;
        Utils.setprefb("tilecenter", Config.tilecenter);
        msg("Tile centering is now turned " + (Config.tilecenter ? "on." : "off."), Color.WHITE);
    }


    public void togglePathfinding(){
        Config.pf = !Config.pf;
        msg("Pathfinding is now turned " + (Config.pf ? "on" : "off"), Color.WHITE);
    }

    public void aggroClosest(){
        if (map != null)
            map.aggroclosest();
    }


    public void rightHand(){
        quickslots.drop(QuickSlotsWdg.lc, Coord.z);
        quickslots.simulateclick(QuickSlotsWdg.lc);
    }

    public void leftHand(){
        quickslots.drop(QuickSlotsWdg.rc, Coord.z);
        quickslots.simulateclick(QuickSlotsWdg.rc);
    }

    public void Drink(){
        Thread i = new Thread(new BeltDrink(this), "BeltDrink");
        i.start();
    }


    public void crawlSpeed(){
        speedget.set(0);
    }

    public void walkSpeed(){
        speedget.set(1);
    }

    public void runSpeed(){
        speedget.set(2);
    }

    public void sprintSpeed(){
        speedget.set(3);
    }

  /*  public boolean globtype(char key, KeyEvent ev) {
        if (key == ':') {
            entercmd();
            return (true);
        } else if((Config.screenurl != null) && (Character.toUpperCase(key) == 'S') && ((ev.getModifiersEx() & (KeyEvent.META_DOWN_MASK | KeyEvent.ALT_DOWN_MASK)) != 0)) {
            Screenshooter.take(this, Config.screenurl);
            return(true);
        } else if (key == 3) {
            if (chat.visible && !chat.hasfocus) {
                setfocus(chat);
            } else {
                if (chat.targeth == 0) {
                    chat.sresize(chat.savedh);
                    setfocus(chat);
                } else {
                    chat.sresize(0);
                }
            }
            Utils.setprefb("chatvis", chat.targeth != 0);
        } else if ((key == 27) && (map != null) && !map.hasfocus) {
            setfocus(map);
            return (true);
        } else if (ev.isControlDown() && ev.getKeyCode() == KeyEvent.VK_G) {
            if (map != null)
                map.togglegrid();
            return true;
        } else if (ev.isControlDown() && ev.getKeyCode() == KeyEvent.VK_M) {
            if (Config.statuswdgvisible) {
                if (statuswindow != null)
                    statuswindow.reqdestroy();
                Config.statuswdgvisible = false;
                Utils.setprefb("statuswdgvisible", false);
            } else {
                statuswindow = new StatusWdg();
                add(statuswindow, new Coord(HavenPanel.w / 2 + 80, 10));
                Config.statuswdgvisible = true;
                Utils.setprefb("statuswdgvisible", true);
            }
            return true;
        } else if (ev.isAltDown() && ev.getKeyCode() == Config.zkey) {
            quickslots.drop(QuickSlotsWdg.lc, Coord.z);
            quickslots.simulateclick(QuickSlotsWdg.lc);
            return true;
        } else if (ev.isAltDown() && ev.getKeyCode() == KeyEvent.VK_X) {
            quickslots.drop(QuickSlotsWdg.rc, Coord.z);
            quickslots.simulateclick(QuickSlotsWdg.rc);
            return true;
        } else if (ev.isControlDown() && ev.getKeyCode() == KeyEvent.VK_S) {
            HavenPanel.needtotakescreenshot = true;
            return true;
        } else if (ev.isControlDown() && ev.getKeyCode() == KeyEvent.VK_H) {
            Config.hidegobs = !Config.hidegobs;
            Utils.setprefb("hidegobs", Config.hidegobs);
            if (map != null)
                map.refreshGobsHidable();
            return true;
        } else if (ev.isShiftDown() && ev.getKeyCode() == KeyEvent.VK_TAB) {
            if (map != null)
                map.aggroclosest();
            return true;
        } else if (ev.isShiftDown() && ev.getKeyCode() == KeyEvent.VK_I) {
            Config.resinfo = !Config.resinfo;
            Utils.setprefb("resinfo", Config.resinfo);
            msg("Resource info on shift/shift+ctrl is now turned " + (Config.resinfo ? "on" : "off"), Color.WHITE);
            return true;
        } else if (ev.isShiftDown() && ev.getKeyCode() == KeyEvent.VK_B) {
            Config.showboundingboxes = !Config.showboundingboxes;
            Utils.setprefb("showboundingboxes", Config.showboundingboxes);
            if (map != null)
                map.refreshGobsAll();
            return true;
        } else if (ev.isControlDown() && ev.getKeyCode() == Config.zkey) {
            Config.pf = !Config.pf;
            msg("Pathfinding is now turned " + (Config.pf ? "on" : "off"), Color.WHITE);
            return true;
        } else if (ev.isControlDown() && ev.getKeyCode() == KeyEvent.VK_N) {
            Config.daylight = !Config.daylight;
            Utils.setprefb("daylight", Config.daylight);
        } else if (ev.isControlDown() && ev.getKeyCode() == KeyEvent.VK_P) {
            Config.showplantgrowstage = !Config.showplantgrowstage;
            Utils.setprefb("showplantgrowstage", Config.showplantgrowstage);
            if (!Config.showplantgrowstage && map != null)
                map.removeCustomSprites(Sprite.GROWTH_STAGE_ID);
            if (map != null)
                map.refreshGobsGrowthStages();
        } else if (ev.isControlDown() && ev.getKeyCode() == KeyEvent.VK_X) {
            Config.tilecenter = !Config.tilecenter;
            Utils.setprefb("tilecenter", Config.tilecenter);
            msg("Tile centering is now turned " + (Config.tilecenter ? "on." : "off."), Color.WHITE);
        } else if (ev.isControlDown() && ev.getKeyCode() == KeyEvent.VK_D) {
            Config.showminerad = !Config.showminerad;
            Utils.setprefb("showminerad", Config.showminerad);
            return true;
        } else if (ev.isShiftDown() && ev.getKeyCode() == KeyEvent.VK_D) {
            Config.showfarmrad = !Config.showfarmrad;
            Utils.setprefb("showfarmrad", Config.showfarmrad);
            return true;
        } else if (!Config.disabledrinkhotkey && (ev.getKeyCode() == KeyEvent.VK_BACK_QUOTE || (Config.iswindows && Utils.getScancode(ev) == 41))) {
            Thread i = new Thread(new BeltDrink(this), "BeltDrink");
            i.start();
           // maininv.drink(100);
            return true;
        } else if (ev.isControlDown() && ev.getKeyCode() == KeyEvent.VK_A) {
            if (mapfile != null && mapfile.show(!mapfile.visible)) {
                mapfile.raise();
                fitwdg(mapfile);
            }
            return true;
        } else if (!ev.isShiftDown() && ev.getKeyCode() == KeyEvent.VK_Q) {
            Thread t = new Thread(new PickForageable(this), "PickForageable");
            t.start();
            return true;
        } else if (ev.isControlDown() && ev.getKeyCode() == KeyEvent.VK_U) {
            TexGL.disableall = !TexGL.disableall;
            return true;
        }
        return (super.globtype(key, ev));
    }*/

    public boolean globtype(char key, KeyEvent ev) {
        if (key == ':') {
            entercmd();
            return true;
        } else if ((key == 27) && (map != null) && !map.hasfocus) {
            setfocus(map);
            return true;
        } else if (chat.hasfocus && chat.visible) {
            return true;
        }  else if (ev.isControlDown() && ev.getKeyCode() == KeyEvent.VK_G) {
            if (map != null)
            map.togglegrid();
            return true;
        }
        else
         return KeyBinder.handle(ui, ev) || (super.globtype(key, ev));
    }


    public boolean mousedown(Coord c, int button) {
        return (super.mousedown(c, button));
    }

    public void resize(Coord sz) {
        this.sz = sz;
        chat.resize(Config.chatsz);
        chat.move(new Coord(0, sz.y));
        if (!Utils.getprefb("chatvis", true))
            chat.sresize(0);
        if (map != null)
            map.resize(sz);
        beltwdg.c = new Coord(blpw + 10, sz.y - beltwdg.sz.y - 5);
        if (statuswindow != null)
            statuswindow.c = new Coord(HavenPanel.w / 2 + 80, 10);
        super.resize(sz);
    }

    public void presize() {
        resize(parent.sz);
    }

    public void msg(String msg, Color color, Color logcol) {
        msgtime = Utils.rtime();
        msg = Resource.getLocString(Resource.BUNDLE_MSG, msg);
        lastmsg = msgfoundry.render(msg, color);
        syslog.append(msg, logcol);
        if (color == Color.WHITE)
            Audio.play(msgsfx);
    }

    public void msg(String msg, Color color) {
        msg(msg, color, color);
    }

    private static final Resource errsfx = Resource.local().loadwait("sfx/error");
    private static final Resource msgsfx = Resource.local().loadwait("sfx/msg");

    private double lasterrsfx = 0;
    public void error(String msg) {
        msg(msg, new Color(192, 0, 0), new Color(255, 0, 0));
        if (errmsgcb != null)
            errmsgcb.notifyErrMsg(msg);
        double now = Utils.rtime();
        if(now - lasterrsfx > 0.1 && Config.errorsounds){
            Audio.play(errsfx);
            lasterrsfx = now;
        }

    }

    public void msgnosfx(String msg) {
        msg(msg, new Color(255, 255, 254), Color.WHITE);
    }

    private static final String charterMsg = "The name of this charterstone is \"";

    public void msg(String msg) {
        if (msg.startsWith(charterMsg))
            CharterList.addCharter(msg.substring(charterMsg.length(), msg.length() - 2));

        msg(msg, Color.WHITE, Color.WHITE);
    }

    public void act(String... args) {
        wdgmsg("act", (Object[]) args);
    }

    public void act(int mods, Coord mc, Gob gob, String... args) {
        int n = args.length;
        Object[] al = new Object[n];
        System.arraycopy(args, 0, al, 0, n);
        if (mc != null) {
            al = Utils.extend(al, al.length + 2);
            al[n++] = mods;
            al[n++] = mc;
            if (gob != null) {
                al = Utils.extend(al, al.length + 2);
                al[n++] = (int) gob.id;
                al[n++] = gob.rc;
            }
        }
        wdgmsg("act", al);
    }

    public Window getwnd(String cap) {
        for (Widget w = lchild; w != null; w = w.prev) {
            if (w instanceof Window) {
                Window wnd = (Window) w;
                if (wnd.cap != null && cap.equals(wnd.origcap))
                    return wnd;
            }
        }
        return null;
    }


    private static final int WND_WAIT_SLEEP = 8;
    public Window waitfForWnd(String cap, int timeout) {
        int t  = 0;
        while (t < timeout) {
            Window wnd = getwnd(cap);
            if (wnd != null)
                return wnd;
            t += WND_WAIT_SLEEP;
            try {
                Thread.sleep(WND_WAIT_SLEEP);
            } catch (InterruptedException e) {
                return null;
            }
        }
        return null;
    }

    public List<IMeter.Meter> getmeters(String name) {
        for (Widget meter : meters) {
            if (meter instanceof IMeter) {
                IMeter im = (IMeter) meter;
                try {
                    Resource res = im.bg.get();
                    if (res != null && res.basename().equals(name))
                        return im.meters;
                } catch (Loading l) {
                }
            }
        }
        return null;
    }

    public IMeter.Meter getmeter(String name, int midx) {
        List<IMeter.Meter> meters = getmeters(name);
        if (meters != null && midx < meters.size())
            return meters.get(midx);
        return null;
    }

    public Equipory getequipory() {
        if (equwnd != null) {
            for (Widget w = equwnd.lchild; w != null; w = w.prev) {
                if (w instanceof Equipory)
                    return (Equipory) w;
            }
        }
        return null;
    }

    private static final Tex nkeybg = Resource.loadtex("gfx/hud/hb-main");

    public class NKeyBelt extends Belt implements DTarget, DropTarget {
        public int curbelt = 0;
        final Coord pagoff = new Coord(5, 25);

        public NKeyBelt() {
            super(nkeybg.sz());
            adda(new IButton("gfx/hud/hb-btn-chat", "", "-d", "-h") {
                Tex glow;
                {
                    this.tooltip = RichText.render(Resource.getLocString(Resource.BUNDLE_LABEL, "Chat ($col[255,255,0]{Ctrl+C})"), 0);
                    glow = new TexI(PUtils.rasterimg(PUtils.blurmask(up.getRaster(), 2, 2, Color.WHITE)));
                    KeyBinder.add(KeyEvent.VK_C, CTRL, TOGGLE_CHAT);
                }

                public void click() {
                    if (chat.targeth == 0) {
                        chat.sresize(chat.savedh);
                        setfocus(chat);
                    } else {
                        chat.sresize(0);
                    }
                    Utils.setprefb("chatvis", chat.targeth != 0);
                }

                @Override
                public Object tooltip(Coord c, Widget prev) {
                    if(!checkhit(c)) {
                        return null;
                    }
                    KeyBinder.KeyBind bind = KeyBinder.get(TOGGLE_CHAT);
                    String tt = "Chat";
                    if(bind != null && !bind.isEmpty()) {
                        tt = String.format("%s ($col[255,255,0]{%s})", tt, bind.shortcut());
                    }
                    return RichText.render(tt, 0);
                }

                public void draw(GOut g) {
                    super.draw(g);
                    Color urg = chat.urgcols[chat.urgency];
                    if (urg != null) {
                        GOut g2 = g.reclipl(new Coord(-2, -2), g.sz.add(4, 4));
                        g2.chcolor(urg.getRed(), urg.getGreen(), urg.getBlue(), 128);
                        g2.image(glow, Coord.z);
                    }
                }
            }, sz, 1, 1);
        }

        private Coord beltc(int i) {
            return (pagoff.add(((invsq.sz().x + 2) * i) + (10 * (i / 5)), 0));
        }

        private int beltslot(Coord c) {
            for (int i = 0; i < 10; i++) {
                if (c.isect(beltc(i), invsq.sz()))
                    return (i + (curbelt * 12));
            }
            return (-1);
        }

        public void draw(GOut g) {
            g.image(nkeybg, Coord.z);
            for (int i = 0; i < 10; i++) {
                int slot = i + (curbelt * 12);
                Coord c = beltc(i);
                g.image(invsq, beltc(i));
                try {
                    if (belt[slot] != null) {
                        Resource.Image img = belt[slot].get().layer(Resource.imgc);
                        if (img == null)
                            throw (new NullPointerException("No image in " + belt[slot].get().name));
                        g.image(img.tex(), c.add(1, 1));
                    }
                } catch (Loading e) {
                }
                g.chcolor(FBelt.keysClr);
                FastText.aprint(g, new Coord(c.x + invsq.sz().x - 2, c.y + invsq.sz().y), 1, 1, "" + (i + 1));
                g.chcolor();
            }
            super.draw(g);
        }

        public boolean mousedown(Coord c, int button) {
            int slot = beltslot(c);
            if (slot != -1) {
                if (button == 1)
                    GameUI.this.wdgmsg("belt", slot, 1, ui.modflags());
                if (button == 3)
                    GameUI.this.wdgmsg("setbelt", slot, 1);
                return (true);
            }
            return (super.mousedown(c, button));
        }

        public boolean globtype(char key, KeyEvent ev) {
            if(key != 0)
                return(false);
            int c = ev.getKeyCode();
            if((c < KeyEvent.VK_0) || (c > KeyEvent.VK_9))
                return (false);

            int i = Utils.floormod(c - KeyEvent.VK_0 - 1, 10);
            boolean M = (ev.getModifiersEx() & (KeyEvent.META_DOWN_MASK | KeyEvent.ALT_DOWN_MASK)) != 0;
            if (M) {
                curbelt = i;
            } else {
                keyact(i + (curbelt * 12));
            }
            return (true);
        }

        public boolean drop(Coord c, Coord ul) {
            int slot = beltslot(c);
            if (slot != -1) {
                GameUI.this.wdgmsg("setbelt", slot, 0);
                return (true);
            }
            return (false);
        }

        public boolean iteminteract(Coord c, Coord ul) {
            return (false);
        }

        public boolean dropthing(Coord c, Object thing) {
            int slot = beltslot(c);
            if (slot != -1) {
                if (thing instanceof Resource) {
                    Resource res = (Resource) thing;
                    if (res.layer(Resource.action) != null) {
                        GameUI.this.wdgmsg("setbelt", slot, res.name);
                        return (true);
                    }
                }
            }
            return (false);
        }
    }

    private Map<String, Console.Command> cmdmap = new TreeMap<String, Console.Command>();
    {
        cmdmap.put("afk", (cons, args) -> {
            afk = true;
            wdgmsg("afk");
        });
        cmdmap.put("act", (cons, args) -> {
            Object[] ad = new Object[args.length - 1];
            System.arraycopy(args, 1, ad, 0, ad.length);
            wdgmsg("act", ad);
        });
        cmdmap.put("chrmap", new Console.Command() {
            public void run(Console cons, String[] args) {
                Utils.setpref("mapfile/" + chrid, args[1]);
            }
        });
        cmdmap.put("tool", (cons, args) -> add(gettype(args[1]).create(ui, new Object[0]), 200, 200));
        cmdmap.put("help", (cons, args) -> {
            cons.out.println("Available console commands:");
            cons.findcmds().forEach((s, cmd) -> cons.out.println(s));
        });
        cmdmap.put("savemap", (cons, args) -> {
            new Thread(() -> mapfile.view.dumpTiles(), "MapDumper").start();
        });
        cmdmap.put("baseq", (cons, args) -> {
            FoodInfo.showbaseq = Utils.parsebool(args[1]);
            msg("q10 FEP values in tooltips are now " + (FoodInfo.showbaseq ? "enabled" : "disabled"));
        });
    }
    public void registerItemCallback(ItemClickCallback itemClickCallback) {
        this.itemClickCallback = itemClickCallback;
    }

    public void unregisterItemCallback() {
        this.itemClickCallback = null;
    }
    public Map<String, Console.Command> findcmds() {
        return (cmdmap);
    }

    public void registerErrMsg(ErrorSysMsgCallback callback) {
        this.errmsgcb = callback;
    }
}
