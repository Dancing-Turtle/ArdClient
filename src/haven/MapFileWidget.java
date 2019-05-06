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

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.Map;
import java.util.Objects;

import javax.imageio.ImageIO;

import haven.MapFile.Grid;
import haven.MapFile.GridInfo;
import haven.MapFile.Marker;
import haven.MapFile.PMarker;
import haven.MapFile.SMarker;
import haven.MapFile.Segment;
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;

public class MapFileWidget extends Widget {
    public final MapFile file;
    public Location curloc;
    private Locator setloc;
    public boolean follow;
    private Area dext;
    private Segment dseg;
    private DisplayGrid[] display;
    private Collection<DisplayMarker> markers = null;
    private int markerseq = -1;
    private UI.Grab drag;
    private boolean dragging;
    private Coord dsc, dmc;
    public static int zoom = 0;
    private static final double[] scaleFactors = new double[]{1, 100 / 75.0, 100 / 50.0, 100 / 25.0, 100 / 15.0, 100 / 8.0};

    public MapFileWidget(MapFile file, Coord sz) {
        super();
        this.file = file;
    }

    public static class Location {
        public final Segment seg;
        public final Coord tc;

        public Location(Segment seg, Coord tc) {
            Objects.requireNonNull(seg);
            Objects.requireNonNull(tc);
            this.seg = seg; this.tc = tc;
        }
    }

    public interface Locator {
        Location locate(MapFile file) throws Loading;
    }

    public static class MapLocator implements Locator {
        public final MapView mv;

        public MapLocator(MapView mv) {this.mv = mv;}

        public Location locate(MapFile file) {
            Coord mc = new Coord2d(mv.getcc()).floor(MCache.tilesz);
            if(mc == null)
                throw(new Loading("Waiting for initial location"));
            MCache.Grid plg = mv.ui.sess.glob.map.getgrid(mc.div(cmaps));
            GridInfo info = file.gridinfo.get(plg.id);
            if(info == null)
                throw(new Loading("No grid info, probably coming soon"));
            Segment seg = file.segments.get(info.seg);
            if(seg == null)
                throw(new Loading("No segment info, probably coming soon"));
            return(new Location(seg, info.sc.mul(cmaps.div(scalef())).add(mc.sub(plg.ul).div(scalef()))));
        }
    }

    public static class SpecLocator implements Locator {
        public final long seg;
        public final Coord tc;

        public SpecLocator(long seg, Coord tc) {this.seg = seg; this.tc = tc;}

        public Location locate(MapFile file) {
            Segment seg = file.segments.get(this.seg);
            if(seg == null)
                return(null);
            return(new Location(seg, tc.div(scalef())));
        }
    }

    public void center(Location loc) {
        curloc = loc;
    }

    public Location resolve(Locator loc) {
        if(!file.lock.readLock().tryLock())
            throw(new Loading("Map file is busy"));
        try {
            return(loc.locate(file));
        } finally {
            file.lock.readLock().unlock();
        }
    }

    public void tick(double dt) {
        if(setloc != null) {
            try {
                Location loc = resolve(setloc);
                center(loc);
                if(!follow)
                    setloc = null;
            } catch(Loading l) {
            }
        }
    }

    public static class DisplayGrid {
        public final Segment seg;
        public final Coord sc;
        public final Indir<Grid> gref;
        private Grid cgrid = null;
        private Defer.Future<Tex> img = null;

        public DisplayGrid(Segment seg, Coord sc, Indir<Grid> gref) {
            this.seg = seg;
            this.sc = sc;
            this.gref = gref;
        }

        public Tex img() {
            Grid grid = gref.get();
            if(grid != cgrid) {
                if(img != null)
                    img.cancel();
                img = Defer.later(() -> new TexI(grid.render(sc.mul(cmaps.div(scalef())))));
                cgrid = grid;
            }
            return((img == null)?null:img.get());
        }
    }

    public static class DisplayMarker {
        public static final Resource.Image flagbg, flagfg;
        public static final Coord flagcc;
        public final Marker m;
        public final Text tip;
        public Area hit;
        private Resource.Image img;
        private Coord cc;

        static {
            Resource flag = Resource.local().loadwait("gfx/hud/mmap/flag");
            flagbg = flag.layer(Resource.imgc, 1);
            flagfg = flag.layer(Resource.imgc, 0);
            flagcc = flag.layer(Resource.negc).cc;
        }

        public DisplayMarker(Marker marker) {
            this.m = marker;
            this.tip = Text.render(m.nm);
            if(marker instanceof PMarker)
                this.hit = Area.sized(flagcc.inv(), flagbg.sz);
        }

        public void draw(GOut g, Coord c) {
            if(m instanceof PMarker) {
                Coord ul = c.sub(flagcc);
                g.chcolor(((PMarker)m).color);
                g.image(flagfg, ul);
                g.chcolor();
                g.image(flagbg, ul);
                if (Config.mapdrawflags) {
                   Tex tex = Text.renderstroked(m.nm, Color.white, Color.BLACK, Text.num12boldFnd).tex();
                   if(tex!=null)
                     g.image(tex, ul.add(flagfg.sz.x / 2, -20).sub(tex.sz().x / 2, 0));
                }
            } else if(m instanceof SMarker) {
                SMarker sm = (SMarker)m;
                try {
                    if(cc == null) {
                        Resource res = MapFile.loadsaved(Resource.remote(), sm.res);
                        img = res.layer(Resource.imgc);
                        Resource.Neg neg = res.layer(Resource.negc);
                        cc = (neg != null)?neg.cc:img.sz.div(2);
                        if(hit == null)
                            hit = Area.sized(cc.inv(), img.sz);
                    }
                } catch(Loading l) {
                } catch(Exception e) {
                    cc = Coord.z;
                }
                if(img != null) {
                    //((SMarker)m).res.name.startsWith("gfx/invobjs/small"));
                    if (Config.mapdrawquests) {
                    if (sm.res != null && sm.res.name.startsWith("gfx/invobjs/small")) {
                        Tex tex = Text.renderstroked(sm.nm, Color.white, Color.BLACK, Text.num12boldFnd).tex();
                        g.image(tex, c.sub(cc).add(img.sz.x / 2, -20).sub(tex.sz().x / 2, 0));
                    }
                    }
                    g.image(img, c.sub(cc));
                }
            }
        }
    }

    private void remark(Location loc, Area ext) {
        if(file.lock.readLock().tryLock()) {
            try {
                Collection<DisplayMarker> marks = new ArrayList<>();
                Area mext = ext.margin(cmaps);
                for(Marker mark : file.markers) {
                    if((mark.seg == loc.seg.id) && mext.contains(mark.tc.div(cmaps)))
                        marks.add(new DisplayMarker(mark));
                }
                markers = marks;
                markerseq = file.markerseq;
            } finally {
                file.lock.readLock().unlock();
            }
        }
    }

    private void redisplay(Location loc) {
        Coord hsz = sz.div(2);
        Area next = Area.sized(loc.tc.sub(hsz).div(cmaps.div(scalef())),
                sz.add(cmaps.div(scalef())).sub(1, 1).div(cmaps.div(scalef())).add(1, 1));
        if((display == null) || (loc.seg != dseg) || !next.equals(dext)) {
            DisplayGrid[] nd = new DisplayGrid[next.rsz()];
            if((display != null) && (loc.seg == dseg)) {
                for(Coord c : dext) {
                    if(next.contains(c))
                        nd[next.ri(c)] = display[dext.ri(c)];
                }
            }
            display = nd;
            dseg = loc.seg;
            dext = next;
            markers = null;
        }
    }

    public Coord xlate(Location loc) {
        Location curloc = this.curloc;
        if((curloc == null) || (curloc.seg != loc.seg))
            return(null);
        return(loc.tc.add(sz.div(2)).sub(curloc.tc));
    }

    public void draw(GOut g) {
        Location loc = this.curloc;
        if(loc == null)
            return;
        Coord hsz = sz.div(2);
        redisplay(loc);
        if(file.lock.readLock().tryLock()) {
            try {
                for(Coord c : dext) {
                    if(display[dext.ri(c)] == null)
                        display[dext.ri(c)] = new DisplayGrid(loc.seg, c, loc.seg.grid(c));
                }
            } finally {
                file.lock.readLock().unlock();
            }
        }
        for(Coord c : dext) {
            Tex img;
            try {
                DisplayGrid disp = display[dext.ri(c)];
                if((disp == null) || ((img = disp.img()) == null))
                    continue;
            } catch(Loading l) {
                continue;
            }
            Coord ul = hsz.add(c.mul(cmaps.div(scalef()))).sub(loc.tc);
            g.image(img, ul, cmaps.div(scalef()));
        }
        if((markers == null) || (file.markerseq != markerseq))
            remark(loc, dext);
        if(markers != null) {
            for(DisplayMarker mark : markers) {
                mark.draw(g, hsz.sub(loc.tc).add(mark.m.tc.div(scalef())));
            }
        }
    }

    public void dumpTiles() {
        gameui().msg("Dumping map. Please wait...");

        Location loc = this.curloc;
        if (loc == null)
            return;

        LinkedList<DisplayGrid> grids = new LinkedList<>();
        if(file.lock.readLock().tryLock()) {
            try {
                for (Map.Entry<Coord, Long> entry: loc.seg.map.entrySet())
                    grids.add(new DisplayGrid(loc.seg, entry.getKey(), loc.seg.grid(entry.getKey())));
            } finally {
                file.lock.readLock().unlock();
            }
        }

        String session = (new SimpleDateFormat("yyyy-MM-dd HH.mm.ss")).format(new Date(System.currentTimeMillis()));
        (new File("map/" + session)).mkdirs();

        int c = 50;

        BufferedWriter ids = null;
        try {
            ids = new BufferedWriter(new FileWriter(String.format("map/%s/ids.txt", session), true));

            while (grids.size() > 0) {
                // just a fail-safe
                if (c-- == 0) {
                    gameui().error("WARNING: map dumper timed out");
                    break;
                }

                ListIterator<DisplayGrid> iter = grids.listIterator();
                while (iter.hasNext()) {
                    DisplayGrid disp = iter.next();
                    try {
                        Grid grid = disp.gref.get();
                        if (grid != null) {
                            BufferedImage img = grid.render(disp.sc.mul(cmaps));
                            File tilefile = new File(String.format("map/%s/tile_%d_%d.png", session, disp.sc.x, disp.sc.y));
                            ImageIO.write(img, "png", tilefile);
                            ids.write(String.format("%d,%d,%d\n", disp.sc.x, disp.sc.y, grid.id));
                        } else {
                            continue;
                        }
                    } catch (Loading l) {
                        continue;
                    }
                    iter.remove();
                }
            }
        } catch (IOException e) {
            gameui().error("ERROR: map dumper failure. See console for more info.");
            e.printStackTrace();
            return;
        } finally {
            if (ids != null) {
                try {
                    ids.close();
                } catch (IOException e) {
                }
            }
        }

        gameui().msg("Finished dumping map");
    }

    public void center(Locator loc) {
        setloc = loc;
        follow = false;
    }

    public void follow(Locator loc) {
        setloc = loc;
        follow = true;
    }

    public boolean clickloc(Location loc, int button) {
        return(false);
    }

    public boolean clickmarker(DisplayMarker mark, int button) {
        return(false);
    }

    private DisplayMarker markerat(Coord tc) {
        if(markers != null) {
            for(DisplayMarker mark : markers) {
                if((mark.hit != null) && mark.hit.contains(tc.sub(mark.m.tc.div(scalef()))))
                    return(mark);
            }
        }
        return(null);
    }

    public boolean mousedown(Coord c, int button) {
        Coord tc = null;
        if (curloc != null)
            tc = c.sub(sz.div(2)).add(curloc.tc);
        if (tc != null) {
            DisplayMarker mark = markerat(tc);
            if ((mark != null) && clickmarker(mark, button))
                return (true);
            if (clickloc(new Location(curloc.seg, tc.mul(scalef())), button))
                return (true);
            if (button == 1 && (ui.modctrl || ui.modmeta)) {
                //Only works if we're on the same map segment as our player
                try {
                 //   tc = c.sub(sz.div(2)).add(curloc.tc);
                    final Location pl = resolve(new MapLocator(ui.gui.map));
                    if (curloc != null && curloc.seg == pl.seg) {
                        final Coord2d plc = new Coord2d(ui.sess.glob.oc.getgob(ui.gui.map.plgob).getc());
                        //Offset in terms of loftar map coordinates
                        //XXX: Previous worlds had randomized north/south/east/west directions, still the case? Assuming not for now.
                        final Coord2d offset = new Coord2d(pl.tc.sub(tc));
                        //Translate this to real map units and add to current map position
                        final Coord2d mc = plc.sub(offset.mul(MCache.tilesz).mul(scalef()));
                        if (ui.modmeta) {
                            ui.gui.map.queuemove(mc);
                        } else {
                            ui.gui.map.moveto(mc);
                        }
                    }
                } catch (Exception e) {
                    PBotUtils.sysLogAppend("Failed to resolve player location with map move", "white");
                }
                return true;
            }
	}
	if(button == 1 && ui.modflags() == 0) {
                Location loc = curloc;
                if ((drag == null) && (loc != null)) {
                    drag = ui.grabmouse(this);
                    dsc = c;
                    dmc = loc.tc;
                    dragging = false;
                }
                return (true);

        }
            return (super.mousedown(c, button));
        }

    public void mousemove(Coord c) {
        if(drag != null) {
            if(dragging) {
                setloc = null;
                follow = false;
                curloc = new Location(curloc.seg, dmc.add(dsc.sub(c)));
            } else if(c.dist(dsc) > 5) {
                dragging = true;
            }
        }
        super.mousemove(c);
    }

    public boolean mouseup(Coord c, int button) {
        if((drag != null) && (button == 1)) {
            drag.remove();
            drag = null;
        }
        return(super.mouseup(c, button));
    }

    public boolean mousewheel(Coord c, int amount) {
        if(amount > 0) {
            if (MapFileWidget.zoom < 4) {
                PBotAPI.gui.mapfile.zoomtex = null;
                Coord tc = curloc.tc.mul(MapFileWidget.scalef());
                MapFileWidget.zoom++;
                tc = tc.div(MapFileWidget.scalef());
                curloc.tc.x = tc.x;
                curloc.tc.y = tc.y;
            }
        } else {
            if (MapFileWidget.zoom > 0) {
                PBotAPI.gui.mapfile.zoomtex = null;
                Coord tc = curloc.tc.mul(MapFileWidget.scalef());
                MapFileWidget.zoom--;
                tc = tc.div(MapFileWidget.scalef());
                curloc.tc.x = tc.x;
                curloc.tc.y = tc.y;
            }
        }
        return(true);
    }

    public Object tooltip(Coord c, Widget prev) {
        if(curloc != null) {
            Coord tc = c.sub(sz.div(2)).add(curloc.tc);
            DisplayMarker mark = markerat(tc);
            if(mark != null) {
                return(mark.tip);
            }
        }
        return(super.tooltip(c, prev));
    }

    public static double scalef() {
        return scaleFactors[zoom];
    }
}
