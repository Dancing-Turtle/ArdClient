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

import static haven.MCache.cmaps;
import static haven.MCache.tilesz;

import java.awt.Color;
import java.awt.event.KeyEvent;
import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;

import haven.BuddyWnd.GroupSelector;
import haven.MapFile.Marker;
import haven.MapFile.PMarker;
import haven.MapFile.SMarker;
import haven.MapFileWidget.Locator;
import haven.MapFileWidget.MapLocator;
import haven.MapFileWidget.SpecLocator;
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;
import haven.sloth.gob.Type;

public class MapWnd extends Window {
    public static final Resource markcurs = Resource.local().loadwait("gfx/hud/curs/flag");
    public static final Tex party = Resource.loadtex("custom/mm/pl/party");

    public final MapFileWidget view;
    public final MapView mv;
    public final MarkerList list;
    private final Locator player;
    private final Widget toolbar;
    public final Widget zoombar;
    private final Frame viewf, listf, fdropf;
    private final Dropbox<Pair<String, String>> fdrop;
    private TextEntry namesel;
    private GroupSelector colsel;
    private Button mremove;
    private Comparator<Marker> mcmp = namecmp;
    private List<Marker> markers = Collections.emptyList();
    private int markerseq = -1;
    private boolean domark = false;
    public Tex zoomtex = null;
    private final Collection<Runnable> deferred = new LinkedList<>();
    private static final Tex plx = Text.renderstroked("\u2716",  Color.red, Color.BLACK, Text.num12boldFnd).tex();
    private  Predicate<Marker> filter = (m -> true);
    private final static Comparator<Marker> namecmp = ((a, b) -> a.nm.compareTo(b.nm));
    private Map<Color, Tex> xmap = new HashMap<Color, Tex>(6);
    private Map<Long, Tex> namemap = new HashMap<>(50);
    private Map<Coord, Coord> questlinemap = new HashMap<>();


    public MapWnd(MapFile file, MapView mv, Coord sz, String title) {
        super(sz, title,title, true);
        this.mv = mv;
        this.player = new MapLocator(mv);
        viewf = add(new Frame(Coord.z, true));
        view = viewf.add(new View(file));
        recenter();
        toolbar = add(new Widget(Coord.z));
        toolbar.add(new Img(Resource.loadtex("gfx/hud/mmap/fgwdg")), Coord.z);
        toolbar.add(new IButton("gfx/hud/mmap/home", "", "-d", "-h") {
            {
                tooltip = RichText.render("Follow ($col[255,255,0]{Home})", 0);
            }

            public void click() {
                questlinemap.clear();
                recenter();
            }
        }, Coord.z);
        toolbar.add(new IButton("gfx/hud/mmap/mark", "", "-d", "-h") {
            {
                tooltip = RichText.render("Add marker", 0);
            }

            public void click() {
                domark = true;
            }
        }, Coord.z);
        toolbar.pack();
        zoombar = add(new ZoomBar());

        fdrop = add(markersFilter());
        fdropf = Frame.around(this, Collections.singletonList(fdrop));

        listf = add(new Frame(new Coord(200, 200), false));
        list = listf.add(new MarkerList(listf.inner().x, 0));

        resize(sz);
    }

    public class ZoomBar extends Widget {
        private final static int btnsz = 21;
        public ZoomBar() {
            super(new Coord(btnsz * 2 + 20, btnsz));
            add(new IButton("gfx/hud/worldmap/minus", "", "", "") {
                @Override
                public void click() {
                    if (MapFileWidget.zoom < 4) {
                        zoomtex = null;
                        Coord tc = view.curloc.tc.mul(MapFileWidget.scalef());
                        MapFileWidget.zoom++;
                        tc = tc.div(MapFileWidget.scalef());
                        view.curloc.tc.x = tc.x;
                        view.curloc.tc.y = tc.y;
                    }
                }
            }, Coord.z);
            add(new IButton("gfx/hud/worldmap/plus", "", "", "") {
                @Override
                public void click() {
                    if (MapFileWidget.zoom > 0) {
                        zoomtex = null;
                        Coord tc = view.curloc.tc.mul(MapFileWidget.scalef());
                        MapFileWidget.zoom--;
                        tc = tc.div(MapFileWidget.scalef());
                        view.curloc.tc.x = tc.x;
                        view.curloc.tc.y = tc.y;
                    }
                }
            }, new Coord(btnsz + 20, 0));
        }

        @Override
        public void draw(GOut g) {
            super.draw(g);
            g.image(renderz(), new Coord((btnsz * 2 + 20) / 2 - zoomtex.sz().x / 2, btnsz / 2 - zoomtex.sz().y / 2));
        }

        private Tex renderz() {
            if (zoomtex == null)
                zoomtex = Text.renderstroked((5 - MapFileWidget.zoom) + "", Color.WHITE, Color.BLACK).tex();
            return zoomtex;
        }
    }

    private class View extends MapFileWidget {
        View(MapFile file) {
            super(file, Coord.z);
        }

        public boolean clickmarker(DisplayMarker mark, int button) {
            if (button == 1) {
                list.change2(mark.m);
                list.display(mark.m);
                return (true);
            }
            return (false);
        }

        public boolean clickloc(Location loc, int button) {
            if (domark && (button == 1)) {
                Marker nm = new PMarker(loc.seg.id, loc.tc, "New marker", BuddyWnd.gc[new Random().nextInt(BuddyWnd.gc.length)]);
                file.add(nm);
                list.change2(nm);
                list.display(nm);
                domark = false;
                return (true);
            }
            return (false);
        }

        public boolean mousedown(Coord c, int button) {
            if (domark && (button == 3)) {
                domark = false;
                return (true);
            }
            return (super.mousedown(c, button));
        }



        private Set<Long> drawparty(GOut g, final Location ploc) {
            final Set<Long> ignore = new HashSet<>();
            final Coord pc = new Coord2d(mv.getcc()).floor(tilesz);
            double angle;
            try {
                synchronized (ui.sess.glob.party) {
                    final Coord psz = party.sz();
                    for (Party.Member m : ui.sess.glob.party.memb.values()) {
                        Coord2d ppc = m.getc();

                        if (ppc == null) // chars are located in different worlds
                            continue;

                        if(ui.sess.glob.party.memb.size() == 1) //don't do anything if you don't have a party
                            continue;


                            final Coord mc = new Coord2d(ppc).floor(tilesz);
                            final Coord gc = xlate(new Location(ploc.seg, ploc.tc.add(mc.sub(pc).div(MapFileWidget.scalef()))));
                            ignore.add(m.gobid);
                            if (gc != null) {
                                Gob gob = m.getgob();

                                if (gob == null){//party member not in draw distance, draw a party colored X instead.
                                    Tex tex = xmap.get(m.col);
                                    if(tex == null){
                                        tex = Text.renderstroked("\u2716",  m.col, m.col, Text.num12boldFnd).tex();
                                        xmap.put(m.col, tex);
                                    }
                                    g.chcolor(m.col);
                                    g.image(tex, gc.sub(psz.div(2)), psz);
                                    Tex nametex = namemap.get(m.gobid);
                                    if(nametex != null) { //if we have a nametex for this gobid because we've seen them before, go ahead and apply it
                                        g.chcolor(Color.WHITE);
                                        g.image(nametex, gc.sub(psz.div(2).add(new Coord(-5,-10))));
                                    }
                                    continue;
                                }
                                angle = gob.geta();
                                final Coord front = new Coord(8, 0).rotate(angle).add(gc);
                                final Coord right = new Coord(-5, 5).rotate(angle).add(gc);
                                final Coord left = new Coord(-5, -5).rotate(angle).add(gc);
                                final Coord notch = new Coord(-2, 0).rotate(angle).add(gc);
                                KinInfo kin = gob.getattr(KinInfo.class);

                                    Tex tex = namemap.get(m.gobid);
                                    if (tex == null && kin != null) { //if we don't already have this nametex in memory, set one up.
                                        System.out.println("tex null kin not null");
                                        tex = Text.renderstroked(kin.name, Color.WHITE, Color.BLACK, Text.delfnd2).tex();
                                       // tex = kin.rendered();
                                        namemap.put(m.gobid, tex);
                                    }
                                    if(tex != null) { //apply texture if it's been successfully setup.
                                        g.chcolor(Color.WHITE);
                                        g.image(tex, gc.sub(psz.div(2).add(new Coord(-5,-10))));
                                    }


                                g.chcolor(m.col);
                                g.poly(front, right, notch, left);
                                g.chcolor(Color.BLACK);
                                g.polyline(1, front, right, notch, left);
                                g.chcolor();
                            }

                    }
                }
            } catch (Loading l) {
                //Fail silently
            }
            return ignore;
        }

        /**
         * Ideally this will be a line -> X -> line -> X
         * Where X is some icon for destinations
         * Start at map.moveto
         * Then follow map.movequeue
         * XXX: does it need an icon?
         */
        private void drawmovement(GOut g, final Location ploc) {
            final Coord pc = new Coord2d(mv.getcc()).floor(tilesz);
            final Coord2d movingto = mv.movingto();
            final Iterator<Coord2d> queue = mv.movequeue();
            Coord last;
            if (movingto != null) {
                //Make the line first
                g.chcolor(Color.MAGENTA);
                final Coord cloc = xlate(ploc);
                last = xlate(new Location(ploc.seg, ploc.tc.add(movingto.floor(tilesz).sub(pc).div(scalef()))));
                if(last != null && cloc != null) {
                    g.dottedline(cloc, last, 2);
                    if (queue.hasNext()) {
                        while (queue.hasNext()) {
                            final Coord next = xlate(new Location(ploc.seg, ploc.tc.add(queue.next().floor(tilesz).sub(pc).div(scalef()))));
                            if(next != null) {
                                g.dottedline(last, next, 2);
                                last = next;
                            } else {
                                break;
                            }
                        }
                    }
                }
            }
        }


        private void questgiverLines(GOut g, final Location ploc){
            List<Coord2d> questQueue = new ArrayList<>();
            final Coord pc = new Coord2d(mv.getcc()).floor(tilesz);
            final double dist = 90000.0D;
            questQueue.addAll(mv.questQueue());
            try {
                if (questQueue.size() > 0) {
                    for (Coord2d coord : questQueue) {
                        PBotAPI.gui.mapfile.view.follow = false;
                        final Gob player = PBotAPI.gui.map.player();
                        double angle = PBotAPI.gui.map.player().rc.angle(coord);
                        final Coord mc = new Coord2d(player.rc).floor(tilesz);
                        final Coord lc = mc.add((int) (Math.cos(angle) * dist), (int) (Math.sin(angle) * dist));
                        final Coord gc = xlate(new Location(ploc.seg, ploc.tc.add(mc.sub(pc))));
                        final Coord mlc = xlate(new Location(ploc.seg, ploc.tc.add(lc.sub(pc))));
                        questlinemap.put(gc, mlc);
                    }
                    questQueue.clear();
                    mv.questQueue().clear();
                }
                if(questlinemap.size() > 0){
                    for(Map.Entry<Coord, Coord> entry : questlinemap.entrySet()) {
                        g.chcolor(Color.MAGENTA);
                        g.dottedline(entry.getKey(), entry.getValue(), 2);
                        g.chcolor();
                    }
                }
            }catch(Exception e){e.printStackTrace();}
        }


        public void draw(GOut g) {
            g.chcolor(0, 0, 0, 128);
            g.frect(Coord.z, sz);
            g.chcolor();
            super.draw(g);
            try {
                final Location loc = resolve(player);
                Coord ploc = xlate(resolve(player));
                if (ploc != null) {
                    g.chcolor(255, 0, 0, 255);
                    g.image(plx, ploc.sub(plx.sz().div(2)));
                    final Set<Long> ignore;
                    if(Config.mapdrawparty)
                        ignore = drawparty(g, loc);
                    g.chcolor();
                    drawmovement(g.reclip(view.c, view.sz), loc);
                    questgiverLines(g.reclip(view.c, view.sz), loc);

                }
            } catch (Loading l) {
            }
        }

        public Resource getcurs(Coord c) {
            if (domark)
                return (markcurs);
            return (super.getcurs(c));
        }
    }

    public void resolveNames(){//used to load name textures even while the map is closed
        try {
            synchronized (ui.sess.glob.party) {
                for (Party.Member m : ui.sess.glob.party.memb.values()) {
                    Coord2d ppc = m.getc();
                    if (ppc == null) // chars are located in different worlds
                        continue;
                    if(ui.sess.glob.party.memb.size() == 1) //don't do anything if you don't have a party
                        continue;
                    Gob gob = m.getgob();
                    if (gob != null){
                        KinInfo kin = gob.getattr(KinInfo.class);
                        Tex tex = namemap.get(m.gobid);
                        if (tex == null && kin != null) { //if we don't already have this nametex in memory, set one up.
                            tex = Text.renderstroked(kin.name, Color.WHITE, Color.BLACK, Text.delfnd2).tex();
                            namemap.put(m.gobid, tex);
                        }
                    }
                }
            }
        } catch (Loading l) {
            //Fail silently
        }
    }

    public void tick(double dt) {
        super.tick(dt);
        if(Config.mapdrawparty)
            resolveNames();
        synchronized (deferred) {
            for (Iterator<Runnable> i = deferred.iterator(); i.hasNext(); ) {
                Runnable task = i.next();
                try {
                    task.run();
                } catch (Loading l) {
                    continue;
                }
                i.remove();
            }
        }
        if (visible && (markerseq != view.file.markerseq)) {
            if (view.file.lock.readLock().tryLock()) {
                try {
                    List<Marker> markers = view.file.markers.stream().filter(filter).collect(java.util.stream.Collectors.toList());
                    markers.sort(mcmp);
                    this.markers = markers;
                    markerseq = view.file.markerseq;
                } finally {
                    view.file.lock.readLock().unlock();
                }
            }
        }
    }

    public void selectMarker(String name){
        if(markers != null && markers.size() > 0) {
            for (Marker marker : markers) {
                if (marker.nm.equals(name)) {
                    list.change2(marker);
                    view.center(new SpecLocator(marker.seg, marker.tc));
                }
            }
        }
    }

    public static final Color every = new Color(255, 255, 255, 16), other = new Color(255, 255, 255, 32);

    private static final Pair[] filters = new Pair[] {
            new Pair<>("-- All --", null),
            new Pair<>("--- Custom ---", "flg"),
            new Pair<>("Abyssal Chasm", "abyssalchasm"),
            new Pair<>("Ancient Windthrow", "windthrow"),
            new Pair<>("Fairy Stone", "fairystone"),
            new Pair<>("Lily Pad Lotus", "lilypadlotus"),
            new Pair<>("Clay Pit", "claypit"),
            new Pair<>("Crystal Rock", "crystalpatch"),
            new Pair<>("Geyser", "geyser"),
            new Pair<>("Great Cave Organ", "caveorgan"),
            new Pair<>("Guano Pile", "guanopile"),
            new Pair<>("Headwaters", "headwaters"),
            new Pair<>("Heart of the Woods", "woodheart"),
            new Pair<>("Ice Spire", "icespire"),
            new Pair<>("Jotun Mussel", "jotunmussel"),
            new Pair<>("Quest Givers", "qst"),
            new Pair<>("Salt Basin", "saltbasin"),
            new Pair<>("Swirling Vortex", "watervortex")
    };

    @SuppressWarnings("unchecked")
    private Dropbox<Pair<String, String>> markersFilter() {
        Dropbox<Pair<String, String>> modes = new Dropbox<Pair<String, String>>(195, filters.length, Math.max(Text.render(filters[0].a.toString()).sz().y, 20)) {
            @Override
            protected Pair<String, String> listitem(int i) {
                return filters[i];
            }

            @Override
            protected int listitems() {
                return filters.length;
            }

            @Override
            protected void drawitem(GOut g, Pair<String, String> item, int i) {
                g.text(item.a, Dropbox.itemtextc);
            }

            @Override
            public void change(Pair<String, String> item) {
                super.change(item);
                if (item.b == null)
                    filter = (m -> true);
                else if (item.b.equals("flg"))
                    filter = (m -> m instanceof PMarker);
                else if (item.b.equals("qst"))
                    filter = (m -> m instanceof SMarker && ((SMarker)m).res.name.startsWith("gfx/invobjs/small"));
                else
                    filter = (m -> m instanceof SMarker && ((SMarker)m).res.name.endsWith(item.b));
                markerseq = -1;
                // reset scrollbar
                if (list != null)
                    list.sb.val = list.sb.min;
            }
        };
        modes.change(filters[0]);
        return modes;
    }

    public class MarkerList extends Listbox<Marker> {
        private final Text.Foundry fnd = CharWnd.attrf;

        public Marker listitem(int idx) {
            return (markers.get(idx));
        }

        public int listitems() {
            return (markers.size());
        }

        public MarkerList(int w, int n) {
            super(w, n, 20);
        }

        private Function<String, Text> names = new CachedFunction<>(500, nm -> fnd.render(nm));

        protected void drawbg(GOut g) {
        }

        public void drawitem(GOut g, Marker mark, int idx) {
            g.chcolor(((idx % 2) == 0) ? every : other);
            g.frect(Coord.z, g.sz);
            if (mark instanceof PMarker)
                g.chcolor(((PMarker) mark).color);
            else
                g.chcolor();
            g.aimage(names.apply(mark.nm).tex(), new Coord(5, itemh / 2), 0, 0.5);
        }

        public void change(Marker mark) {
            change2(mark);
            if (mark != null)
                view.center(new SpecLocator(mark.seg, mark.tc));
        }

        public void change2(Marker mark) {
            this.sel = mark;

            if (namesel != null) {
                ui.destroy(namesel);
                namesel = null;
                if (colsel != null) {
                    ui.destroy(colsel);
                    colsel = null;
                }
                if (mremove != null) {
                    ui.destroy(mremove);
                    mremove = null;
                }
            }

            if (mark != null) {
                if (namesel == null) {
                    namesel = MapWnd.this.add(new TextEntry(200, "") {
                        {
                            dshow = true;
                        }

                        public void activate(String text) {
                            mark.nm = text;
                            view.file.update(mark);
                            commit();
                            change2(null);
                        }
                    });
                }
                namesel.settext(mark.nm);
                namesel.buf.point = mark.nm.length();
                namesel.commit();
                if (mark instanceof PMarker) {
                    PMarker pm = (PMarker) mark;
                    colsel = MapWnd.this.add(new GroupSelector(0) {
                        public void changed(int group) {
                            this.group = group;
                            pm.color = BuddyWnd.gc[group];
                            view.file.update(mark);
                        }
                    });
                    if ((colsel.group = Utils.index(BuddyWnd.gc, pm.color)) < 0)
                        colsel.group = 0;
                }
                mremove = MapWnd.this.add(new Button(200, "Remove", false) {
                    public void click() {
                        view.file.remove(mark);
                        change2(null);
                    }
                });
                MapWnd.this.resize(asz);
            }
        }
    }

    public void resize(Coord sz) {
        super.resize(sz);

        fdropf.c = new Coord(sz.x - 200, 0);
        fdrop.c = new Coord(fdropf.c.x + 5, fdropf.c.y + 5);

        listf.resize(listf.sz.x, sz.y - 120);
        listf.c = new Coord(sz.x - listf.sz.x, fdropf.sz.y);
        list.resize(listf.inner());

        if (namesel != null) {
            namesel.c = listf.c.add(0, listf.sz.y + 10);
            if (colsel != null)
                colsel.c = namesel.c.add(0, namesel.sz.y + 10);
            mremove.c = new Coord(namesel.c.x, sz.y - mremove.sz.y);
        }
        viewf.resize(new Coord(sz.x - listf.sz.x - 10, sz.y));
        view.resize(viewf.inner());
        toolbar.c = viewf.c.add(0, viewf.sz.y - toolbar.sz.y).add(2, -2);
        zoombar.c = viewf.c.add(viewf.sz.x - zoombar.sz.x, viewf.sz.y - zoombar.sz.y).sub(7, 7);
    }

    public void recenter() {
        view.follow(player);
    }

    private static final Tex sizer = Resource.loadtex("gfx/hud/wnd/sizer");

    protected void drawframe(GOut g) {
        g.image(sizer, ctl.add(csz).sub(sizer.sz()));
        super.drawframe(g);
    }

    public boolean keydown(KeyEvent ev) {
        if (super.keydown(ev))
            return (true);
        if (ev.getKeyCode() == KeyEvent.VK_HOME) {
            questlinemap.clear();
            recenter();
            return (true);
        }
        return (false);
    }

    private UI.Grab drag;
    private Coord dragc;

    public boolean mousedown(Coord c, int button) {
        Coord cc = c.sub(ctl);
        if ((button == 1) && (cc.x < csz.x) && (cc.y < csz.y) && (cc.y >= csz.y - 25 + (csz.x - cc.x))) {
            if (drag == null) {
                drag = ui.grabmouse(this);
                dragc = asz.sub(c);
                return (true);
            }
        }
        return (super.mousedown(c, button));
    }

    public void mousemove(Coord c) {
        if (drag != null) {
            Coord nsz = c.add(dragc);
            nsz.x = Math.max(nsz.x, 300);
            nsz.y = Math.max(nsz.y, 150);
            resize(nsz);
        }
        super.mousemove(c);
    }

    public boolean mouseup(Coord c, int button) {
        if ((button == 1) && (drag != null)) {
            drag.remove();
            drag = null;
            return (true);
        }
        return (super.mouseup(c, button));
    }

    public void markobj(long gobid, long oid, Indir<Resource> resid, String nm) {
        synchronized (deferred) {
            deferred.add(new Runnable() {
                double f = 0;

                public void run() {
                    Resource res = resid.get();
                    String rnm = nm;
                    if (rnm == null) {
                        Resource.Tooltip tt = res.layer(Resource.tooltip);
                        if (tt == null)
                            return;
                        rnm = tt.t;
                    }
                    double now = Utils.rtime();
                    if (f == 0)
                        f = now;
                    Gob gob = ui.sess.glob.oc.getgob(gobid);
                    if (gob == null) {
                        if (now - f < 1.0)
                            throw (new Loading());
                        return;
                    }
                    Coord tc = gob.rc.floor(tilesz);
                    MCache.Grid obg = ui.sess.glob.map.getgrid(tc.div(cmaps));
                    if (!view.file.lock.writeLock().tryLock())
                        throw (new Loading());
                    try {
                        MapFile.GridInfo info = view.file.gridinfo.get(obg.id);
                        if (info == null)
                            throw (new Loading());
                        Coord sc = tc.add(info.sc.sub(obg.gc).mul(cmaps));
                        SMarker prev = view.file.smarkers.get(oid);
                        if (prev == null) {
                            view.file.add(new SMarker(info.seg, sc, rnm, oid, new Resource.Spec(Resource.remote(), res.name, res.ver)));
                        } else {
                            if ((prev.seg != info.seg) || !prev.tc.equals(sc)) {
                                prev.seg = info.seg;
                                prev.tc = sc;
                                view.file.update(prev);
                            }
                        }
                    } finally {
                        view.file.lock.writeLock().unlock();
                    }
                }
            });
        }
    }

    @Override
    public void close(){
        show(false);
        mv.questQueue().clear();
        questlinemap.clear();
    }

    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (sender == cbtn) {
            show(false);
        }
        else
            super.wdgmsg(sender, msg, args);
    }

    @Override
    public boolean type(char key, KeyEvent ev) {
        if (key == 27) {
            if (cbtn.visible) {
                show(false);
            }
            return true;
        }
        return super.type(key, ev);
    }
}
