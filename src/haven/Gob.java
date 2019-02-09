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

import haven.purus.BotUtils;
import haven.purus.pbot.PBotAPI;
import haven.resutil.BPRadSprite;

import java.awt.*;
import java.util.*;
import java.util.List;

public class Gob implements Sprite.Owner, Skeleton.ModOwner, Rendered {
    public Coord2d rc;
    public Coord sc;
    public Coord3f sczu;
    public double a;
    public boolean virtual = false;
    public int printdelay;
    public long id;
    public int frame;
    public final Glob glob;
    Map<Class<? extends GAttrib>, GAttrib> attr = new HashMap<Class<? extends GAttrib>, GAttrib>();
    public Collection<Overlay> ols = new LinkedList<Overlay>() {
        public boolean add(Overlay item) {
	        /* XXX: Remove me once local code is changed to use addol(). */
            if(glob.oc.getgob(id) != null) {
                // FIXME: extend ols with a method for adding sprites without triggering changed.
                if (item.id != Sprite.GROWTH_STAGE_ID && item != animalradius)
                    glob.oc.changed(Gob.this);
            }
            return(super.add(item));
        }
    };

    private final Collection<ResAttr.Cell<?>> rdata = new LinkedList<ResAttr.Cell<?>>();
    private final Collection<ResAttr.Load> lrdata = new LinkedList<ResAttr.Load>();
    public int cropstgmaxval = 0;
    private Overlay gobpath = null;
    private Overlay bowvector = null;
    private static final Material.Colors dframeEmpty = new Material.Colors(new Color(0, 255, 0, 200));
    private static final Material.Colors cRackEmpty = new Material.Colors(new Color(0, 255, 0, 255));
    private static final Material.Colors cRackFull = new Material.Colors(new Color(255, 0, 0, 255));
    private static final Material.Colors coopMissing = new Material.Colors(new Color(255, 0, 255, 255));
    private static final Material.Colors dframeDone = new Material.Colors(new Color(255, 0, 0, 200));
    private static final Material.Colors cupboardfull = new Material.Colors(new Color(255, 0, 0, 175));
    private static final Material.Colors cupboardempty = new Material.Colors(new Color(0, 255, 0, 175));
    private static final Material.Colors dframeWater = new Material.Colors(new Color(0, 0, 255, 200));
    private static final Material.Colors dframeBark = new Material.Colors(new Color(165, 42, 42, 200));
    private static final Material.Colors potDOne = new Material.Colors(new Color(0, 0, 0, 255));
    private static Gob.Overlay animalradius = new Gob.Overlay(new BPRadSprite(100.0F, -10.0F, BPRadSprite.smatDanger));
    private static final Map<Gob, Gob.Overlay> playerhighlight = new HashMap<Gob, Gob.Overlay>();
    public Boolean knocked = null;  // knocked will be null if pose update request hasn't been received yet
    public Type type = null;

    public enum Type {
        OTHER(0), DFRAME(1), TREE(2), BUSH(3), BOULDER(4), PLAYER(5), SIEGE_MACHINE(6), MAMMOTH(7), OLDTRUNK(9), GARDENPOT(10), MUSSEL(11), LOC_RESOURCE(12), FU_YE_CURIO(13),
        CLAY(14), EAGLE(15), PLANT(16), MULTISTAGE_PLANT(17), CHEESERACK(18), CUPBOARD(19), TANTUB(20), COOP(21), HUTCH(22), MOTH(23), DUNGEON(24), EYEBALL(25), DUNGKEY(26), ROAD(27), NIDBANE(28), DUNGEONDOOR(29),
        ROADENDPOINT(30), LIVESTOCK(42),
        MOB(32), SNAKE(33),BEAR(34), LYNX(35),SLIME(36), SEAL(37), TROLL(38), WALRUS(39), BAT(40), //anything from 33-63 will work for MOB aggro circles
        WOODEN_SUPPORT(64), STONE_SUPPORT(65), METAL_SUPPORT(66), TROUGH(67), BEEHIVE(68), WAGON(600), WALL(602), DREAMCATCHER(603), HOUSE(604);

        public final int value;

        Type(int value) {
            this.value = value;
        }

        boolean has(Type g) {
            if (g == null)
                return false;
            return (value & g.value) != 0;
        }
    }

    public static class Overlay implements Rendered {
        public Indir<Resource> res;
        public MessageBuf sdt;
        public Sprite spr;
        public int id;
        public boolean delign = false;

        public Overlay(int id, Indir<Resource> res, Message sdt) {
            this.id = id;
            this.res = res;
            this.sdt = new MessageBuf(sdt);
            spr = null;
        }

        public Overlay(Sprite spr) {
            this.id = -1;
            this.res = null;
            this.sdt = null;
            this.spr = spr;
        }

        public Overlay(int id, Sprite spr) {
            this.id = id;
            this.res = null;
            this.sdt = null;
            this.spr = spr;
        }

        public static interface CDel {
            public void delete();
        }

        public static interface CUpd {
            public void update(Message sdt);
        }

        public static interface SetupMod {
            public void setupgob(GLState.Buffer buf);

            public void setupmain(RenderList rl);
        }

        public void draw(GOut g) {
        }

        public boolean setup(RenderList rl) {
            if (spr != null)
                rl.add(spr, null);
            return (false);
        }

        public Object staticp() {
            return((spr == null)?null:spr.staticp());
        }
    }

    /* XXX: This whole thing didn't turn out quite as nice as I had
     * hoped, but hopefully it can at least serve as a source of
     * inspiration to redo attributes properly in the future. There
     * have already long been arguments for remaking GAttribs as
     * well. */
    public static class ResAttr {
        public boolean update(Message dat) {
            return (false);
        }

        public void dispose() {
        }

        public static class Cell<T extends ResAttr> {
            final Class<T> clsid;
            Indir<Resource> resid = null;
            MessageBuf odat;
            public T attr = null;

            public Cell(Class<T> clsid) {
                this.clsid = clsid;
            }

            public void set(ResAttr attr) {
                if (this.attr != null)
                    this.attr.dispose();
                this.attr = clsid.cast(attr);
            }
        }

        private static class Load {
            final Indir<Resource> resid;
            final MessageBuf dat;

            Load(Indir<Resource> resid, Message dat) {
                this.resid = resid;
                this.dat = new MessageBuf(dat);
            }
        }

        @Resource.PublishedCode(name = "gattr", instancer = FactMaker.class)
        public static interface Factory {
            public ResAttr mkattr(Gob gob, Message dat);
        }

        public static class FactMaker implements Resource.PublishedCode.Instancer {
            public Factory make(Class<?> cl) throws InstantiationException, IllegalAccessException {
                if (Factory.class.isAssignableFrom(cl))
                    return (cl.asSubclass(Factory.class).newInstance());
                if (ResAttr.class.isAssignableFrom(cl)) {
                    try {
                        final java.lang.reflect.Constructor<? extends ResAttr> cons = cl.asSubclass(ResAttr.class).getConstructor(Gob.class, Message.class);
                        return (new Factory() {
                            public ResAttr mkattr(Gob gob, Message dat) {
                                return (Utils.construct(cons, gob, dat));
                            }
                        });
                    } catch (NoSuchMethodException e) {
                    }
                }
                return (null);
            }
        }
    }

    public static class Static {}
    public static class SemiStatic {}

    public Gob(Glob glob, Coord2d c, long id, int frame) {
        this.glob = glob;
        this.rc = c;
        this.id = id;
        this.frame = frame;
        loc.tick();
    }

    public Gob(Glob glob, Coord2d c) {
        this(glob, c, -1, 0);
    }

    public static interface ANotif<T extends GAttrib> {
        public void ch(T n);
    }

    public void ctick(int dt) {
        for (GAttrib a : attr.values())
            a.ctick(dt);
        for (Iterator<Overlay> i = ols.iterator(); i.hasNext(); ) {
            Overlay ol = i.next();
            if (ol.spr == null) {
                try {
                    ol.spr = Sprite.create(this, ol.res.get(), ol.sdt.clone());
                } catch (Loading e) {
                }
            } else {
                boolean done = ol.spr.tick(dt);
                if ((!ol.delign || (ol.spr instanceof Overlay.CDel)) && done)
                    i.remove();
            }
        }
        if (virtual && ols.isEmpty())
            glob.oc.remove(id);
    }

    /* Intended for local code. Server changes are handled via OCache. */
    public void addol(Overlay ol) {
        ols.add(ol);
    }
    public void addol(Sprite ol) {
        addol(new Overlay(ol));
    }

    public Overlay findol(int id) {
        for (Overlay ol : ols) {
            if (ol.id == id)
                return (ol);
        }
        return (null);
    }




    public void tick() {
        for (GAttrib a : attr.values())
            a.tick();
        loadrattr();
    }

    public void dispose() {
        for (GAttrib a : attr.values())
            a.dispose();
        for (ResAttr.Cell rd : rdata) {
            if (rd.attr != null)
                rd.attr.dispose();
        }
    }

    public void move(Coord2d c, double a) {
        Moving m = getattr(Moving.class);
        if (m != null)
            m.move(c);
        this.rc = c;
        this.a = a;
    }

    public Coord3f getc() {
        Moving m = getattr(Moving.class);
        Coord3f ret = (m != null) ? m.getc() : getrc();
        DrawOffset df = getattr(DrawOffset.class);
        if (df != null)
            ret = ret.add(df.off);
        return (ret);
    }

    public Coord3f getrc() {
        return(glob.map.getzp(rc));
    }

    public double geta() {
        return a;
    }

    private Class<? extends GAttrib> attrclass(Class<? extends GAttrib> cl) {
        while (true) {
            Class<?> p = cl.getSuperclass();
            if (p == GAttrib.class)
                return (cl);
            cl = p.asSubclass(GAttrib.class);
        }
    }

    public void setattr(GAttrib a) {
        Class<? extends GAttrib> ac = attrclass(a.getClass());
        attr.put(ac, a);

        if (Config.showplayerpaths && gobpath == null && a instanceof LinMove) {
            Gob pl = glob.oc.getgob(MapView.plgob);
            if (pl != null) {
                Following follow = pl.getattr(Following.class);
                if (pl == this ||
                        (follow != null && follow.tgt() == this)) {
                    gobpath = new Overlay(new GobPath(this));
                    ols.add(gobpath);
                }
            }
        }
    }

    public <C extends GAttrib> C getattr(Class<C> c) {
        GAttrib attr = this.attr.get(attrclass(c));
        if (!c.isInstance(attr))
            return (null);
        return (c.cast(attr));
    }

    public void delattr(Class<? extends GAttrib> c) {
        attr.remove(attrclass(c));
        if (attrclass(c) == Moving.class && gobpath != null) {
            ols.remove(gobpath);
            gobpath = null;
            MapView.pllastcc = null;
        }
    }

    private Class<? extends ResAttr> rattrclass(Class<? extends ResAttr> cl) {
        while (true) {
            Class<?> p = cl.getSuperclass();
            if (p == ResAttr.class)
                return (cl);
            cl = p.asSubclass(ResAttr.class);
        }
    }

    @SuppressWarnings("unchecked")
    public <T extends ResAttr> ResAttr.Cell<T> getrattr(Class<T> c) {
        for (ResAttr.Cell<?> rd : rdata) {
            if (rd.clsid == c)
                return ((ResAttr.Cell<T>) rd);
        }
        ResAttr.Cell<T> rd = new ResAttr.Cell<T>(c);
        rdata.add(rd);
        return (rd);
    }

    public static <T extends ResAttr> ResAttr.Cell<T> getrattr(Object obj, Class<T> c) {
        if (!(obj instanceof Gob))
            return (new ResAttr.Cell<T>(c));
        return (((Gob) obj).getrattr(c));
    }

    private void loadrattr() {
        boolean upd = false;
        for (Iterator<ResAttr.Load> i = lrdata.iterator(); i.hasNext(); ) {
            ResAttr.Load rd = i.next();
            ResAttr attr;
            try {
                attr = rd.resid.get().getcode(ResAttr.Factory.class, true).mkattr(this, rd.dat.clone());
            } catch (Loading l) {
                continue;
            }
            ResAttr.Cell<?> rc = getrattr(rattrclass(attr.getClass()));
            if (rc.resid == null)
                rc.resid = rd.resid;
            else if (rc.resid != rd.resid)
                throw (new RuntimeException("Conflicting resattr resource IDs on " + rc.clsid + ": " + rc.resid + " -> " + rd.resid));
            rc.odat = rd.dat;
            rc.set(attr);
            i.remove();
            upd = true;
        }
        if(upd) {
            if(glob.oc.getgob(id) != null)
                glob.oc.changed(this);
        }
    }

    public void setrattr(Indir<Resource> resid, Message dat) {
        for (Iterator<ResAttr.Cell<?>> i = rdata.iterator(); i.hasNext(); ) {
            ResAttr.Cell<?> rd = i.next();
            if (rd.resid == resid) {
                if (dat.equals(rd.odat))
                    return;
                if ((rd.attr != null) && rd.attr.update(dat))
                    return;
                break;
            }
        }
        for (Iterator<ResAttr.Load> i = lrdata.iterator(); i.hasNext(); ) {
            ResAttr.Load rd = i.next();
            if (rd.resid == resid) {
                i.remove();
                break;
            }
        }
        lrdata.add(new ResAttr.Load(resid, dat));
        loadrattr();
    }

    public void delrattr(Indir<Resource> resid) {
        for (Iterator<ResAttr.Cell<?>> i = rdata.iterator(); i.hasNext(); ) {
            ResAttr.Cell<?> rd = i.next();
            if (rd.resid == resid) {
                i.remove();
                rd.attr.dispose();
                break;
            }
        }
        for (Iterator<ResAttr.Load> i = lrdata.iterator(); i.hasNext(); ) {
            ResAttr.Load rd = i.next();
            if (rd.resid == resid) {
                i.remove();
                break;
            }
        }
    }

    @Override
    public void draw(GOut g) {
    }

    public void determineType(String name) {
        if (name.startsWith("gfx/terobjs/trees") && !name.endsWith("log") && !name.endsWith("oldtrunk"))
            type = Type.TREE;
        else if (name.endsWith("oldtrunk"))
            type = Type.OLDTRUNK;
        else if (name.startsWith("gfx/terobjs/ttub"))
            type = Type.TANTUB;
        else if(name.startsWith("gfx/kritter/nidbane/nidbane"))
            type = Type.NIDBANE;
        else if (name.startsWith("gfx/terobjs/chicken"))
            type = Type.COOP;
        else if(name.startsWith("gfx/terobjs/rabbit"))
            type = Type.HUTCH;
        else if(name.startsWith("gfx/kritter/cavemoth"))
            type = Type.MOTH;
        else if(name.startsWith("gfx/terobjs/dng/batcave") || name.startsWith("gfx/terobjs/dng/antdungeon") || name.startsWith("gfx/terobjs/beaverdam"))
            type = Type.DUNGEON;
        else if(name.startsWith("gfx/terobjs/dng/antdoor"))
            type = Type.DUNGEONDOOR;
        else if (name.endsWith("terobjs/plants/carrot") || name.endsWith("terobjs/plants/hemp"))
            type = Type.MULTISTAGE_PLANT;
        else if (name.startsWith("gfx/terobjs/plants") && !name.endsWith("trellis"))
            type = Type.PLANT;
        else if (name.startsWith("gfx/terobjs/bushes"))
            type = Type.BUSH;
        else if (name.equals("gfx/borka/body"))
            type = Type.PLAYER;
        else if (name.startsWith("gfx/terobjs/bumlings"))
            type = Type.BOULDER;
        else  if (name.endsWith("vehicle/bram") || name.endsWith("vehicle/catapult") || name.endsWith("vehicle/wreckingball"))
            type = Type.SIEGE_MACHINE;
        else if (name.endsWith("/bear"))
            type = Type.BEAR;
        else if (name.endsWith("/adder"))
            type = Type.SNAKE;
        else if (name.endsWith("/ooze"))
            type = Type.SLIME;
        else if (name.endsWith("/lynx"))
            type = Type.LYNX;
        else if (name.endsWith("/walrus"))
            type = Type.WALRUS;
        else if (name.endsWith("/greyseal"))
            type = Type.SEAL;
        else if (name.endsWith("/mammoth"))
            type = Type.MAMMOTH;
        else if (name.endsWith("/troll"))
            type = Type.TROLL;
        else if(name.endsWith("/bat") && Config.batcircle)
            type = Type.MOB;
        else if(name.endsWith("/ooze") && Config.slimecircle)
            type = Type.MOB;
        else if (name.endsWith("/boar") || name.endsWith("/badger") || name.endsWith("/wolverine") || name.endsWith("/wildgoat") || name.endsWith("/wolf"))
            type = Type.MOB;
        else if (name.endsWith("/minesupport") || name.endsWith("/ladder"))
            type = Type.WOODEN_SUPPORT;
        else if (name.endsWith("/column"))
            type = Type.STONE_SUPPORT;
        else if (name.endsWith("/minebeam"))
            type = Type.METAL_SUPPORT;
        else if (name.endsWith("/trough"))
            type = Type.TROUGH;
        else if (name.endsWith("/beehive"))
            type = Type.BEEHIVE;
        else if (name.endsWith("/dframe"))
            type = Type.DFRAME;
        else if (name.endsWith("/gardenpot"))
            type = Type.GARDENPOT;
        else if (name.endsWith("/mussels"))
            type = Type.MUSSEL;
        else if(name.endsWith("/clay-gray"))
            type = Type.CLAY;
        else if(name.endsWith("/cupboard"))
            type = Type.CUPBOARD;
        else if (name.endsWith("/goldeneagle"))
            type = Type.EAGLE;
        else if (name.endsWith("/wagon"))
        	type = Type.WAGON;
        else if (name.endsWith("/cheeserack"))
            type = Type.CHEESERACK;
        else if(name.endsWith("/stonemansion")||name.endsWith("/logcabin")||name.endsWith("/greathall")||name.endsWith("/stonestead")||name.endsWith("/timberhouse")||name.endsWith("stonetower"))
        	type = Type.HOUSE;
        else if(name.endsWith("dreca"))
        	type = Type.DREAMCATCHER;
        else if(name.startsWith("gfx/terobjs/road/milestone")) {
            if(name.endsWith("m"))
            type = Type.ROAD;
            else if (name.endsWith("e"))
                type = Type.ROADENDPOINT;
        }
        else if (name.startsWith("gfx/terobjs/arch/pali") && !name.equals("gfx/terobjs/arch/palisadegate") &&
   			 !name.equals("gfx/terobjs/arch/palisadebiggate") || name.startsWith("gfx/terobjs/arch/brick")
   			 && !name.equals("gfx/terobjs/arch/brickwallgate") &&!name.equals("gfx/terobjs/arch/brickbiggate")
   			 || name.startsWith("gfx/terobjs/arch/pole") && !name.equals("gfx/terobjs/arch/polegate") &&
!name.equals("gfx/terobjs/arch/polebiggate")) // Excludes gates
        	type = Type.WALL;
        else if (Config.alarmitems.containsKey(name) && Config.alarmitems.get(name).selected)
            type = Type.FU_YE_CURIO;
        else if (Config.locres.contains(name))
            type = Type.LOC_RESOURCE;
        else if(name.startsWith("gfx/terobjs/road/roadball"))
            type = Type.EYEBALL;
        else if(name.startsWith("gfx/terobjs/items/antkeynew"))
            type = Type.DUNGKEY;
        else
            type = Type.OTHER;
    }

    public boolean setup(RenderList rl) {
        loc.tick();
        for (Overlay ol : ols)
            rl.add(ol, null);
        for (Overlay ol : ols) {
            if (ol.spr instanceof Overlay.SetupMod)
                ((Overlay.SetupMod) ol.spr).setupmain(rl);
        }
        final GobHealth hlt = getattr(GobHealth.class);
        if (hlt != null)
            rl.prepc(hlt.getfx());

        if (MapView.markedGobs.contains(id))
            rl.prepc(MapView.markedFx);
    if(Config.showrackstatus && type == Type.CHEESERACK){
            if (ols.size() == 3)
                rl.prepc(cRackFull);
           if (ols.size() > 0 && ols.size() < 3 && Config.cRackmissing)
                rl.prepc(BPRadSprite.cRackMissing);
            else
                rl.prepc(cRackEmpty);
    }
    if(Config.showcupboardstatus && type == Type.CUPBOARD){
        int stage = getattr(ResDrawable.class).sdt.peekrbuf(0);
       // BotUtils.sysLogAppend("Stage : "+stage,"white");
       // BotUtils.sysLogAppend("Stage : "+stage,"white");
        if (stage == 30 || stage == 29)
            rl.prepc(cupboardfull);
        if(stage == 1 || stage == 2)
                rl.prepc(cupboardempty);
        //if(ols.size()>0)
          //  rl.prepc(cupboardfull);
    }
    if(Config.showdframestatus && type == Type.TANTUB){
        int stage = getattr(ResDrawable.class).sdt.peekrbuf(0);
       // BotUtils.sysLogAppend("Sprite num : "+stage,"white");
        if (stage == 2)
            rl.prepc(dframeEmpty);
        if(stage == 10 || stage == 9 || stage == 8)
            rl.prepc(dframeDone);
        if(stage == 0 || stage == 1 || stage == 4 || stage == 5)
            rl.prepc(dframeWater);
        }
        if(Config.showcoopstatus && type == Type.COOP){
            int stage = getattr(ResDrawable.class).sdt.peekrbuf(0);
            if (stage == 0)
                rl.prepc(cRackFull);
            if(stage == 1)
                rl.prepc(coopMissing);
            if(stage == 2)
                rl.prepc(dframeWater);
        }
        if(Config.showhutchstatus && type == Type.HUTCH){
          /*  no rabbits -stage 2 = no food or water
            stage 1  = no food or water doors open
            stage 6  = water no food
            stage 5 = water no food doors open
            stage 62 = food water doors closed
            stage 61 = food water doors open
            stage 58 = food no water
            stage 57 = food no water door open

            1-11 rabbits
            stage 125 food and water doors open
            stage 126 food and water doors closed
            stage 69 no food water doors open
            stage 70 no food water doors closed
            stage 66 no food no water doors closed
            stage 65 no food no water doors open
            stage 122 food no water doors closed
            stage 121 food no water doors open

            12 rabbits - stage -2 = food and water doors closed
            stage -3  = food and water doors open
            stage -6 = food no water doors closed
            stage -7 = food no water doors open
            stage -62 no food no water doors closed
            stage -63 no food no water doors open
            stage -58 no food water doors closed
            stage -59 no food water doors open*/
            int stage = getattr(ResDrawable.class).sdt.peekrbuf(0);
            if (stage == 2 || stage == 1 || stage == -62 || stage == -63 || stage == 66 || stage == 65)
                rl.prepc(cRackFull);
            if (stage == 6 || stage == 5 || stage == -58 || stage == -59 || stage == 69 || stage == 70 || stage == -51 || stage == -50)
                rl.prepc(coopMissing);
            if(stage == -38 || stage == 58 || stage == 57 || stage == -6 || stage == -7 || stage == 122 || stage == 121)
                rl.prepc(dframeWater);
        }

        if (Config.showdframestatus && type == Type.DFRAME) {
            boolean done = true;
            boolean empty = true;
            for (Overlay ol : ols) {
                try {
                    Indir<Resource> olires = ol.res;
                    if (olires != null) {
                        empty = false;
                        Resource olres = olires.get();
                        if (olres != null) {
                            if (olres.name.endsWith("-blood") || olres.name.endsWith("-windweed") || olres.name.endsWith("fishraw")) {
                                done = false;
                                break;
                            }
                        }
                    }
                } catch (Loading l) {
                }
            }
            if (done && !empty && type != Type.TANTUB)
                rl.prepc(dframeDone);
            else if (empty && type != Type.TANTUB)
                rl.prepc(dframeEmpty);
        }

        if (Config.highlightpots && type == Type.GARDENPOT && ols.size() == 2)
            rl.prepc(potDOne);

        GobHighlight highlight = getattr(GobHighlight.class);
        if (highlight != null) {
            if (highlight.cycle <= 0)
                delattr(GobHighlight.class);
            else
                rl.prepc(highlight.getfx());
        }

        Drawable d = getattr(Drawable.class);
        if (d != null) {

        	// Replace hide stuff with Purus Pasta hide
            try {
                if(Config.hidemoths && type!= null && type == Type.MOTH) {
                //do nothing since it's a moth
                }
                else if (Config.hidegobs) {
                    if (Config.hideTrees && type!= null && type == Type.TREE) {
                        GobHitbox.BBox bbox = GobHitbox.getBBox(this, true);
                        if (bbox != null && Config.showoverlay) {
                            rl.add(new Overlay(new GobHitbox(this, bbox.a, bbox.b, true)), null);
                        }
                    }
                   // else if (Config.hideCrops && (type == Type.PLANT || type == Type.MULTISTAGE_PLANT)) {
                        // Crops don't have bounding boxes
                       // if (Config.showoverlay) {
                          //  rl.add(new Overlay(new GobHitbox(this, new Coord(-5, -5), new Coord(5, 5), true)), null);
                       // }
                   // }
                    else if (Config.hideWalls && type!= null && type == Type.WALL) {
                        GobHitbox.BBox bbox = GobHitbox.getBBox(this, true);
                        if (bbox != null && Config.showoverlay) {
                            rl.add(new Overlay(new GobHitbox(this, bbox.a, bbox.b, true)), null);
                        }
                    }
                    else if (Config.hideanimals && type != null && this.getres().name.contains("kritter")) {
                        GobHitbox.BBox bbox = GobHitbox.getBBox(this, true);
                        if (bbox != null && Config.showoverlay) {
                            rl.add(new Overlay(new GobHitbox(this, bbox.a, bbox.b, true)), null);
                        }
                    }
                    else if (Config.hideDCatchers && type!= null && type == Type.DREAMCATCHER) {
                        GobHitbox.BBox bbox = GobHitbox.getBBox(this, true);
                        if (bbox != null && Config.showoverlay) {
                            rl.add(new Overlay(new GobHitbox(this, bbox.a, bbox.b, true)), null);
                        }
                    }
                   else if (Config.hideBushes && type!= null && type == Type.BUSH) {
                        GobHitbox.BBox bbox = GobHitbox.getBBox(this, true);
                        if (bbox != null && Config.showoverlay) {
                            rl.add(new Overlay(new GobHitbox(this, bbox.a, bbox.b, true)), null);
                        }
                    }
                   else if (Config.hideDFrames && type!= null && type == Type.DFRAME) {
                        GobHitbox.BBox bbox = GobHitbox.getBBox(this, true);
                        if (bbox != null && Config.showoverlay) {
                            rl.add(new Overlay(new GobHitbox(this, bbox.a, bbox.b, true)), null);
                        }
                    }
                    else if (Config.hideWagons && type!= null && type == Type.WAGON) {
                        GobHitbox.BBox bbox = GobHitbox.getBBox(this, true);
                        if (bbox != null && Config.showoverlay) {
                            rl.add(new Overlay(new GobHitbox(this, bbox.a, bbox.b, true)), null);
                        }
                    }
                   else if (Config.hideHouses && type!= null && type == Type.HOUSE) {
                        GobHitbox.BBox bbox = GobHitbox.getBBox(this, true);
                        if (bbox != null && Config.showoverlay) {
                            rl.add(new Overlay(new GobHitbox(this, bbox.a, bbox.b, true)), null);
                        }
                    }else
                        d.setup(rl);
                } else
                    d.setup(rl);
            }catch(NullPointerException q){}
          /*if (Config.hidegobs && (type == Type.TREE || type == Type.BUSH)) {
                GobHitbox.BBox bbox = GobHitbox.getBBox(this, true);
                if (bbox != null) {
                    rl.add(new Overlay(new GobHitbox(this, bbox.a, bbox.b, true)), null);
                }
            } else {
                d.setup(rl);
            }*/

            if (Config.showboundingboxes) {
                GobHitbox.BBox bbox = GobHitbox.getBBox(this, true);
                if (bbox != null)
                    rl.add(new Overlay(new GobHitbox(this, bbox.a, bbox.b, false)), null);
            }

            if (Config.showplantgrowstage) {
                if (type != null && type == Type.PLANT || type != null && type == Type.MULTISTAGE_PLANT) {
                    int stage = getattr(ResDrawable.class).sdt.peekrbuf(0);
                    if (cropstgmaxval == 0) {
                        for (FastMesh.MeshRes layer : getres().layers(FastMesh.MeshRes.class)) {
                            int stg = layer.id / 10;
                            if (stg > cropstgmaxval)
                                cropstgmaxval = stg;
                        }
                    }
                    Overlay ol = findol(Sprite.GROWTH_STAGE_ID);
                    if (ol == null && (stage == cropstgmaxval || stage > 0 && stage < 6)) {
                        addol(new Gob.Overlay(Sprite.GROWTH_STAGE_ID, new PlantStageSprite(stage, cropstgmaxval, type == Type.MULTISTAGE_PLANT)));
                    } else if (stage <= 0 || (stage != cropstgmaxval && stage >= 6)) {
                        ols.remove(ol);
                    } else if (((PlantStageSprite) ol.spr).stg != stage) {
                        ((PlantStageSprite) ol.spr).update(stage, cropstgmaxval);
                    }
                }

                if (type == Type.TREE || type == Type.BUSH) {
                    ResDrawable rd = getattr(ResDrawable.class);
                    if (rd != null && !rd.sdt.eom()) {
                        final int stage = rd.sdt.peekrbuf(0);
                        if (stage >= 0 && stage < 100) {
                            Overlay ol = findol(Sprite.GROWTH_STAGE_ID);
                            if (ol == null) {
                                addol(new Gob.Overlay(Sprite.GROWTH_STAGE_ID, new TreeStageSprite(stage)));
                            } else if (((TreeStageSprite)ol.spr).val != stage) {
                                ((TreeStageSprite)ol.spr).update(stage);
                            }
                        }
                    }
                }
            }

            if (Config.showanimalrad && Type.MOB.has(type) && Config.showanimalrad) {
                boolean hasradius = ols.contains(animalradius);
                if ((knocked == null || knocked == Boolean.FALSE) && !hasradius)
                    ols.add(animalradius);
                else if (knocked == Boolean.TRUE && hasradius)
                    ols.remove(animalradius);
            }

            if (Config.showarchvector && type == Type.PLAYER && d instanceof Composite) {
                boolean targetting = false;

                Gob followGob = null;
                Moving moving = getattr(Moving.class);
                if (moving != null && moving instanceof Following)
                    followGob = ((Following)moving).tgt();

                for (Composited.ED ed : ((Composite) d).comp.cequ) {
                    try {
                        Resource res = ed.res.res.get();
                        if (res != null && res.name.endsWith("huntersbow") && ed.res.sdt.peekrbuf(0) == 5) {
                            targetting = true;
                            if (bowvector == null) {
                                bowvector = new Overlay(new GobArcheryVector(this, followGob));
                                ols.add(bowvector);
                            }
                            break;
                        }
                    } catch (Loading l) {
                    }
                }

                if (!targetting && bowvector != null) {
                    ols.remove(bowvector);
                    bowvector = null;
                }
            }
        }
        Speaking sp = getattr(Speaking.class);
        if (sp != null)
            rl.add(sp.fx, null);
        KinInfo ki = getattr(KinInfo.class);
        if (ki != null) {
            rl.add(ki.fx, null);
            if(!playerhighlight.containsKey(this) && Config.partycircles) {
            	Resource res = getres();
            	if (res != null && res.name.contains("body") && !isplayer()) {
                  //  BotUtils.sysLogAppend("Player : "+ki.name+" spotted","white");
		            Overlay overlay = new Gob.Overlay(new PartyMemberOutline(this, BuddyWnd.gc[ki.group]));
		            ols.add(overlay);
		            playerhighlight.put(this, overlay);
            	}
            }
        }
        return (false);
    }


    private static final Object DYNAMIC = new Object();
    private Object seq = null;
    public Object staticp() {
        if(seq == null) {
            int rs = 0;
            for(GAttrib attr : attr.values()) {
                Object as = attr.staticp();
                if(as == Rendered.CONSTANS) {
                } else if(as instanceof Static) {
                } else if(as == SemiStatic.class) {
                    rs = Math.max(rs, 1);
                } else {
                    rs = 2;
                    break;
                }
            }
            for(Overlay ol : ols) {
                Object os = ol.staticp();
                if(os == Rendered.CONSTANS) {
                } else if(os instanceof Static) {
                } else if(os == SemiStatic.class) {
                    rs = Math.max(rs, 1);
                } else {
                    rs = 2;
                    break;
                }
            }
            switch(rs) {
                case 0: seq = new Static(); break;
                case 1: seq = new SemiStatic(); break;
                default: seq = null; break;
            }
        }
        return((seq == DYNAMIC)?null:seq);
    }

    void changed() {
        seq = null;
    }

    public Random mkrandoom() {
        return (Utils.mkrandoom(id));
    }

    public Resource getres() {
        Drawable d = getattr(Drawable.class);
        if (d != null)
            return (d.getres());
        return (null);
    }

    private static final ClassResolver<Gob> ctxr = new ClassResolver<Gob>()
            .add(Glob.class, g -> g.glob)
            .add(Session.class, g -> g.glob.sess);
    public <T> T context(Class<T> cl) {return(ctxr.context(cl, this));}

    @Deprecated
    public Glob glob() {return(context(Glob.class));}

    /* Because generic functions are too nice a thing for Java. */
    public double getv() {
        Moving m = getattr(Moving.class);
        if (m == null)
            return (0);
        return (m.getv());
    }

    public final GLState olmod = new GLState() {
        public void apply(GOut g) {
        }

        public void unapply(GOut g) {
        }

        public void prep(Buffer buf) {
            for (Overlay ol : ols) {
                if (ol.spr instanceof Overlay.SetupMod) {
                    ((Overlay.SetupMod) ol.spr).setupgob(buf);
                }
            }
        }
    };

    public class Save extends GLState.Abstract {
        public Matrix4f cam = new Matrix4f(), wxf = new Matrix4f(),
                mv = new Matrix4f();
        public Projection proj = null;
        boolean debug = false;

        public void prep(Buffer buf) {
            mv.load(cam.load(buf.get(PView.cam).fin(Matrix4f.id))).mul1(wxf.load(buf.get(PView.loc).fin(Matrix4f.id)));
            Projection proj = buf.get(PView.proj);
            PView.RenderState wnd = buf.get(PView.wnd);
            Coord3f s = proj.toscreen(mv.mul4(Coord3f.o), wnd.sz());
            Gob.this.sc = new Coord(s);
            Gob.this.sczu = proj.toscreen(mv.mul4(Coord3f.zu), wnd.sz()).sub(s);
            this.proj = proj;
        }
    }

    public final Save save = new Save();

    public class GobLocation extends GLState.Abstract {
        private Coord3f c = null;
        private double a = 0.0;
        private Matrix4f update = null;
        private final Location xl = new Location(Matrix4f.id, "gobx"), rot = new Location(Matrix4f.id, "gob");

        public void tick() {
            try {
                Coord3f c = getc();
                c.y = -c.y;
                if (Config.disableelev)
                    c.z = 0;
                if ((this.c == null) || !c.equals(this.c))
                    xl.update(Transform.makexlate(new Matrix4f(), this.c = c));
                if (this.a != Gob.this.a)
                    rot.update(Transform.makerot(new Matrix4f(), Coord3f.zu, (float) -(this.a = Gob.this.a)));
            } catch (Loading l) {
            }
        }

        public void prep(Buffer buf) {
            xl.prep(buf);
            rot.prep(buf);
        }
    }

    public final GobLocation loc = new GobLocation();

    public boolean isplayer() {
        return MapView.plgob == id;
    }

    public boolean isMoving() {
        if (getattr(LinMove.class) != null)
            return true;

        Following follow = getattr(Following.class);
        if (follow != null && follow.tgt().getattr(LinMove.class) != null)
            return true;

        return false;
    }

    public LinMove getLinMove() {
        LinMove lm = getattr(LinMove.class);
        if (lm != null)
            return lm;

        Following follow = getattr(Following.class);
        if (follow != null)
            return follow.tgt().getattr(LinMove.class);

        return null;
    }

    public boolean isFriend() {
        synchronized (glob.party.memb) {
            for (Party.Member m : glob.party.memb.values()) {
                if (m.gobid == id)
                    return true;
            }
        }

        KinInfo kininfo = getattr(KinInfo.class);
        if (kininfo == null || kininfo.group == 2 /*red*/)
            return false;

        return true;
    }

    public int getStage() {
        Resource res = getres();
        if (res != null && res.name.startsWith("gfx/terobjs/plants") && !res.name.endsWith("trellis")) {
	    	GAttrib rd = getattr(ResDrawable.class);
	    	final int stage = ((ResDrawable) rd).sdt.peekrbuf(0);
	        return stage;
        } else
        	return -1;
    }
}
