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

import haven.resutil.BPRadSprite;
import haven.resutil.WaterTile;
import haven.sloth.gfx.HitboxMesh;
import haven.sloth.gob.*;
import haven.sloth.io.HighlightData;
import haven.sloth.script.pathfinding.Hitbox;
import integrations.mapv4.MappingClient;

import java.awt.*;
import java.util.List;
import java.util.*;
import java.util.function.Consumer;

public class Gob implements Sprite.Owner, Skeleton.ModOwner, Rendered {
    public int cropstgmaxval = 0;
    private Overlay gobpath = null;
    private Overlay bowvector = null;
    public static final Text.Foundry gobhpf = new Text.Foundry(Text.serif, 14).aa(true);
    private static final Material.Colors dframeEmpty = new Material.Colors(new Color(0, 255, 0, 200));
    private static final Material.Colors cRackEmpty = new Material.Colors(new Color(0, 255, 0, 255));
    private static final Material.Colors cRackFull = new Material.Colors(new Color(255, 0, 0, 255));
    private static final Material.Colors coopMissing = new Material.Colors(new Color(255, 0, 255, 255));
    private static final Material.Colors dframeDone = new Material.Colors(new Color(255, 0, 0, 200));
    private static final Material.Colors cupboardfull = new Material.Colors(new Color(255, 0, 0, 175));
    private static final Material.Colors cupboardempty = new Material.Colors(new Color(0, 255, 0, 175));
    private static final Material.Colors dframeWater = new Material.Colors(new Color(0, 0, 255, 200));
    private static final Material.Colors dframeBark = new Material.Colors(new Color(165, 42, 42, 200));
    private static final Material.Colors potDOne = new Material.Colors(DefSettings.GARDENPOTDONECOLOR.get());
    public static Gob.Overlay animalradius = new Gob.Overlay(new BPRadSprite(100.0F, -10.0F, BPRadSprite.smatDanger));
    public static Gob.Overlay doubleanimalradius = new Gob.Overlay(new BPRadSprite(200.0F, -20.0F, BPRadSprite.smatDanger));

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

        public Overlay(int id, Sprite spr) {
            this.id = id;
            this.res = null;
            this.sdt = null;
            this.spr = spr;
        }

        public Overlay(Sprite spr) {
            this.id = -1;
            this.res = null;
            this.sdt = null;
            this.spr = spr;
        }

        public String name() {
            try {
                if(res != null)
                    return res.get().name;
                else
                    return "";
            } catch (Loading l) {
                return "";
            }
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


    public static interface ANotif<T extends GAttrib> {
        public void ch(T n);
    }

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

    public class GobLocation extends GLState.Abstract {
        private Coord3f c = null;
        private double a = 0.0;
        private Matrix4f update = null;
        private final Location xl = new Location(Matrix4f.id, "gobx"), rot = new Location(Matrix4f.id, "gob");


        public void tick() {
            try {
                Coord3f c = getc();
                if(Config.disableelev)
                    c.z = 0;
                if(type == Type.WALLSEG && Config.flatwalls) {
                    c.z = c.z - 10;
                }
                c.y = -c.y;
                if(type == Type.ANIMAL || type == Type.DANGANIMAL) {
                    Tiler tl = glob.map.tiler(glob.map.gettile_safe(rc.floor(MCache.tilesz)));
                    if (tl instanceof WaterTile)
                        c.z += 5;
                }
                if((this.c == null) || !c.equals(this.c))
                    xl.update(Transform.makexlate(new Matrix4f(), this.c = c));
                if(this.a != Gob.this.a)
                    rot.update(Transform.makerot(new Matrix4f(), Coord3f.zu, (float)-(this.a = Gob.this.a)));
            } catch(Loading l) {}
        }

        public void prep(Buffer buf) {
            xl.prep(buf);
            rot.prep(buf);
        }
    }

    public static class Static {}
    public static final Static STATIC = new Static();
    public static class SemiStatic {}
    public static final SemiStatic SEMISTATIC = new SemiStatic();

    public final Save save = new Save();
    public final GobLocation loc = new GobLocation();
    public final GLState olmod = new GLState() {
        public void apply(GOut g) {}
        public void unapply(GOut g) {}
        public void prep(Buffer buf) {
            synchronized (ols) {
                for(Overlay ol : ols) {
                    if(ol.spr instanceof Overlay.SetupMod) {
                        ((Overlay.SetupMod)ol.spr).setupgob(buf);
                    }
                }
            }
        }
    };

    public Coord2d rc;
    public Coord sc;
    public Coord3f sczu;
    public double a;
    public boolean virtual = false;
    int clprio = 0;
    public long id;
    public int frame;
    public final Glob glob;
    public int quality = 0;
    Map<Class<? extends GAttrib>, GAttrib> attr = new HashMap<Class<? extends GAttrib>, GAttrib>();
    private final Set<haven.sloth.gob.Rendered> renderedattrs = new HashSet<>();
    public Collection<Overlay> ols = new LinkedList<Overlay>() {
        public boolean add(Overlay item) {
            /* XXX: Remove me once local code is changed to use addol(). */
            if(glob.oc.getgob(id) != null) {
                // FIXME: extend ols with a method for adding sprites without triggering changed.
                if (item.id != Sprite.GROWTH_STAGE_ID && item != animalradius && item != doubleanimalradius)
                    glob.oc.changed(Gob.this);
            }
            return(super.add(item));
        }
    };
    private List<Overlay> dols = new ArrayList<>();
    private List<Pair<GAttrib, Consumer<Gob>>> dattrs = new ArrayList<>();

    private final Collection<ResAttr.Cell<?>> rdata = new LinkedList<ResAttr.Cell<?>>();
    private final Collection<ResAttr.Load> lrdata = new LinkedList<ResAttr.Load>();
    private HitboxMesh hitboxmesh;
    private boolean pathfinding_blackout = false;
    private List<Coord> hitboxcoords;

    private boolean discovered = false;
    public Type type;

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

    /**
     * This method is called once as soon as its res name is accessible
     * @param name The res name
     */
    private void discovered(final String name) {
        //Don't try to discover anything until we know who the plgob is.
        final UI ui = glob.ui.get();
        if(ui != null && ui.gui != null && ui.gui.map != null && ui.gui.map.plgob != -1) {
            //Before we do anything make sure we care about this
            if (!Deleted.isDeleted(name)) {
                //Gobs we care about
                //Figure out our type first
                type = Type.getType(name);
                //checks for mannequins and changes their type to prevent unknown alarms
                if (type == Type.HUMAN && attr.containsKey(GobHealth.class))
                    type = Type.UNKNOWN;
                if(name.endsWith("stump"))
                    type = Type.TREE;
                //Check for any special attributes we should attach
                Alerted.checkAlert(name, this);

                if (Movable.isMovable(name)) {
                    setattr(new Movable(this));
                }
                if (Hidden.isHidden(name)) {
                    setattr(new Hidden(this));
                }
                if (HighlightData.isHighlighted(name)) {
                    mark(-1);
                }
                if(type == Type.HUMAN) {
                    setattr(new Halo(this));
                }

                res().ifPresent((res) -> { //should always be present once name is discovered
                    final Hitbox hitbox = Hitbox.hbfor(this, true);
                    if (hitbox != null) {
                        hitboxmesh = HitboxMesh.makehb(hitbox.size(), hitbox.offset());
                        updateHitmap();
                    }
                });
            } else {
                //We don't care about these gobs, tell OCache to start the removal process
                dispose();
                glob.oc.remove(id);
            }
            discovered = true;
        }
    }

    public boolean isDiscovered() {
        return discovered;
    }

    public void updateHitmap() {
        synchronized (glob.gobhitmap) {
            if (hitboxcoords != null) {
                glob.gobhitmap.rem(this, hitboxcoords);
                hitboxcoords = null;
            }
            //don't want objects being held to be on the hitmap
            final UI ui = glob.ui.get();
            if(getattr(HeldBy.class) == null &&
                    (getattr(Holding.class) == null || ui == null || getattr(Holding.class).held.id != ui.gui.map.plgob) &&
                    !pathfinding_blackout) {
                hitboxcoords = glob.gobhitmap.add(this);
            }
        }
    }

    public void updatePathfindingBlackout(final boolean val) {
        this.pathfinding_blackout = val;
        updateHitmap();
    }

    public void mark(final int life) {
        if(findol(Mark.id) == null) {
            daddol(Mark.id, new Mark(life));
        } else {
            ((Mark) (findol(Mark.id).spr)).setLife(life);
        }
    }

    public void unmark() {
        if(findol(Mark.id) != null) {
            ((Mark) (findol(Mark.id).spr)).revoke();
        }
    }

    public void ctick(int dt) {
        if(!discovered) {
            resname().ifPresent(this::discovered);
        }

        for (GAttrib a : attr.values())
            a.ctick(dt);
        final Iterator<Pair<GAttrib, Consumer<Gob>>> ditr = dattrs.iterator();
        while(ditr.hasNext()) {
            final Pair<GAttrib, Consumer<Gob>> pair = ditr.next();
            setattr(pair.a);
            pair.b.accept(this);
            ditr.remove();
        }

        synchronized (ols) {
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
            for(Iterator<Overlay> i = dols.iterator(); i.hasNext();) {
                Overlay ol = i.next();
                ols.add(ol);
                i.remove();
            }
            if (virtual && ols.isEmpty())
                glob.oc.remove(id);
        }
    }

    public String details() {
        StringBuilder sb = new StringBuilder();
        sb.append("Res: " ); sb.append(resname().orElse(""));
        sb.append(" ["); sb.append(id); sb.append("]\n");
        sb.append("Type: " );sb.append(type); sb.append("\n");
        sb.append("staticp: "); sb.append(staticp() != null ? "static" : "dynamic"); sb.append("\n");
        final Holding holding = getattr(Holding.class);
        if(holding != null) {
            sb.append("Holding: ");
            sb.append(holding.held.id);
            sb.append(" - ");
            sb.append(holding.held.resname().orElse("Unknown"));
            sb.append("\n");
        } else {
            final HeldBy heldby = getattr(HeldBy.class);
            if (heldby != null) {
                sb.append("Held By: ");
                sb.append(heldby.holder.id);
                sb.append(" - ");
                sb.append(heldby.holder.resname().orElse("Unknown"));
                sb.append("\n");
            }
        }
        ResDrawable dw = getattr(ResDrawable.class);
        if (dw != null) {
            sb.append("sdt: "); sb.append(dw.sdtnum()); sb.append("\n");
            sb.append("Angle: "); sb.append(Math.toDegrees(a)); sb.append("\n");
        } else {
            Composite comp = getattr(Composite.class);
            if (comp != null) {
                sb.append(eq());
                sb.append("\n");
            }
        }
        sb.append("Position: "); sb.append(getc()); sb.append("\n");
        return sb.toString();
    }

    public String rnm(Indir<Resource> r) {
        try {
            if(r != null && r.get() != null)
                return r.get().name;
            else
                return "";
        } catch(Exception e) {
            return "";
        }
    }

    public boolean isDead() {
        Drawable d = getattr(Drawable.class);
        if(d instanceof Composite) {
            Composite comp = (Composite)d;
            if(comp.oldposes != null) {
                for(ResData res : comp.oldposes) {
                    final String nm = rnm(res.res).toLowerCase();
                    if(nm.endsWith("knock") || nm.endsWith("dead")) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    public String eq() {
        Drawable d = getattr(Drawable.class);
        if(d instanceof Composite) {
            Composite comp = (Composite)d;

            final StringBuilder sb = new StringBuilder();
            sb.append("Equipment:");
            if(comp.lastnequ != null)
                for(Composited.ED eq : comp.lastnequ) {
                    sb.append("\nEqu: "); sb.append(rnm(eq.res.res)); sb.append(" @ "); sb.append(eq.at);
                }

            if(comp.nmod != null)
                for(Composited.MD md : comp.nmod) {
                    sb.append("\nMod: ");
                    sb.append(rnm(md.mod));
                    for (ResData rd : md.tex) {
                        sb.append("\n  Tex: ");
                        sb.append(rnm(rd.res));
                    }
                }

            sb.append("\nPoses:");
            if(comp.oldposes != null) {
                for (ResData res : comp.oldposes) {
                    sb.append("\nPose: ");
                    sb.append(rnm(res.res));
                }
            }
            if(comp.oldtposes != null) {
                for (ResData res : comp.oldtposes) {
                    sb.append("\nTPose: ");
                    sb.append(rnm(res.res));
                }
            }
            return sb.toString();
        }
        return "";
    }

    /* Intended for local code. Server changes are handled via OCache. */
    public void addol(Overlay ol) {
        synchronized (ols) {
            ols.add(ol);
        }
    }

    public void addol(Sprite ol) {
        addol(new Overlay(ol));
    }
    public Overlay daddol(final Overlay ol) {
        dols.add(ol);
        return ol;
    }
    public Overlay daddol(int id, Sprite spr) {
        final Overlay ol = new Overlay(id, spr);
        daddol(ol);
        return ol;
    }
    public Overlay findol(int id) {
        synchronized (ols) {
            for (Overlay ol : ols) {
                if (ol.id == id)
                    return (ol);
            }
            for(Overlay ol : dols) {
                if(ol.id == id)
                    return ol;
            }
        }
        return (null);
    }




    public void tick() {
        for (GAttrib a : attr.values())
            a.tick();
        loadrattr();
    }

    public void dispose() {
        if(hitboxcoords != null) {
            synchronized (glob.gobhitmap) {
                glob.gobhitmap.rem(this, hitboxcoords);
                hitboxcoords = null;
            }
        }
        for (GAttrib a : attr.values())
            a.dispose();
        for (ResAttr.Cell rd : rdata) {
            if (rd.attr != null)
                rd.attr.dispose();
        }
    }

    public void updsdt() {
        resname().ifPresent(name -> {
            if (name.endsWith("gate") || name.endsWith("/pow")) {
                updateHitmap();
            }
        });
    }

    public boolean moving() {
        return getattr(Moving.class) != null;
    }

    public void move(Coord2d c, double a) {
        Moving m = getattr(Moving.class);
        if (m != null)
            m.move(c);
        synchronized (glob.gobhitmap) {
            if (hitboxcoords != null) {
                glob.gobhitmap.rem(this, hitboxcoords);
                hitboxcoords = null;
            }
            this.rc = c;

            if(isplayer()){
                if(Config.vendanMapv4) MappingClient.getInstance().CheckGridCoord(c);
            }
            this.a = a;
            if(glob.ui != null) {
                final UI ui = glob.ui.get();
                if (discovered) {
                    if (getattr(HeldBy.class) == null &&
                            (getattr(Holding.class) == null || ui == null || getattr(Holding.class).held.id != ui.gui.map.plgob) &&
                            !pathfinding_blackout) {
                        hitboxcoords = glob.gobhitmap.add(this);
                    }
                }
            }
        }
    }

    public Coord3f getc() {
        Moving m = getattr(Moving.class);
        Coord3f ret = (m != null) ? m.getc() : getrc();
        DrawOffset df = getattr(DrawOffset.class);
        if (df != null)
            ret = ret.add(df.off);
        return (ret);
    }

    public Coord3f getc_old() {
        Moving m = getattr(Moving.class);
        Coord3f ret = (m != null) ? m.getc() : getrc_old();
        DrawOffset df = getattr(DrawOffset.class);
        if (df != null)
            ret = ret.add(df.off);
        return (ret);
    }

    public Coord3f getrc() {
        return(glob.map.getzp(rc));
    }

    public Coord3f getrc_old() {
        return(glob.map.getzp_old(rc));
    }//only exists because follow cam hates the new getz


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
        if(a instanceof haven.sloth.gob.Rendered)
            renderedattrs.add((haven.sloth.gob.Rendered)a);
        Class<? extends GAttrib> ac = attrclass(a.getClass());
        attr.put(ac, a);
        if (DefSettings.SHOWPLAYERPATH.get() && gobpath == null && a instanceof LinMove) {
            final UI ui = glob.ui.get();
            if (ui != null) {
                try {
                    Gob pl = glob.oc.getgob(ui.gui.map.plgob);
                    if (pl != null) {
                        Following follow = pl.getattr(Following.class);
                        if (pl == this ||
                                (follow != null && follow.tgt() == this)) {
                            gobpath = new Overlay(new GobPath(this));
                            ols.add(gobpath);
                        }
                    }
                }catch(Exception e){}//ignore, this is just a draw a line on player movement. Not critical.
            }
        }
    }

    public void delayedsetattr(GAttrib a, Consumer<Gob> cb) {
        dattrs.add(new Pair<GAttrib, Consumer<Gob>>(a, cb));
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

    public int sdt() {
        ResDrawable dw = getattr(ResDrawable.class);
        if(dw != null)
            return dw.sdtnum();
        return 0;
    }

    public void draw(GOut g) { }

    public boolean setup(RenderList rl) {
        loc.tick();
        final Hidden hid = getattr(Hidden.class);
        if(hid != null && Config.hideuniquegobs){
            if(Config.showoverlay) {
                hid.setup(rl);
            }
        }
        else {
            synchronized (ols) {
                for (Overlay ol : ols)
                    rl.add(ol, null);
                for (Overlay ol : ols) {
                    if (ol.spr instanceof Overlay.SetupMod)
                        ((Overlay.SetupMod) ol.spr).setupmain(rl);
                }
            }

            final GobHealth hlt = getattr(GobHealth.class);
            if (hlt != null)
                rl.prepc(hlt.getfx());

            final GobQuality qlty = getattr(GobQuality.class);
            if (qlty != null)
                rl.prepc(qlty.getfx());

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
            if(Config.showshedstatus && type == Type.SHED){
                int stage = getattr(ResDrawable.class).sdt.peekrbuf(0);

                if (stage == 30 || stage == 29)
                    rl.prepc(cupboardfull);
                if(stage == 1 || stage == 2)
                    rl.prepc(cupboardempty);
                //while open : empty == 1, 1 item to half items = 5, half full = 13, full 29
                //while closed : empty = 2, 1 item to half items = 6, half full= 14, full 30
            }

            if (MapView.markedGobs.contains(id))
                rl.prepc(MapView.markedFx);

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


            for (final haven.sloth.gob.Rendered attr : renderedattrs) {
                attr.setup(rl);
            }

            GobHighlight highlight = getattr(GobHighlight.class);
            if (highlight != null) {
                if (highlight.cycle <= 0)
                    delattr(GobHighlight.class);
                else
                    rl.prepc(highlight.getfx());
            }


            Drawable d = getattr(Drawable.class);
            try {
                if (d != null) {
                    if (Config.hidegobs && type == Type.TREE && Config.hideTrees) {
                        GobHitbox.BBox bbox = GobHitbox.getBBox(this);
                        if (bbox != null && Config.showoverlay) {
                            rl.add(new Overlay(new GobHitbox(this, bbox.a, bbox.b, true)), null);
                        }
                    } else if (Config.hidegobs && type == Type.BUSH && Config.hideBushes) { //bushes
                        GobHitbox.BBox bbox = GobHitbox.getBBox(this);
                        if (bbox != null && Config.showoverlay)
                            rl.add(new Overlay(new GobHitbox(this, bbox.a, bbox.b, true)), null);
                    }else if(Config.hidegobs && type == Type.BOULDER && Config.hideboulders){
                        GobHitbox.BBox bbox = GobHitbox.getBBox(this);
                        if (bbox != null && Config.showoverlay)
                            rl.add(new Overlay(new GobHitbox(this, bbox.a, bbox.b, true)), null);
                    } else {
                        d.setup(rl);
                    }
                    if (Config.showarchvector && type == Type.HUMAN && d instanceof Composite) {
                        boolean targetting = false;
                        Gob followGob = null;
                        Moving moving = getattr(Moving.class);
                        if (moving != null && moving instanceof Following)
                            followGob = ((Following)moving).tgt();
                        for (Composited.ED ed : ((Composite) d).comp.cequ) {
                            try {
                                Resource res = ed.res.res.get();
                                if (res != null && (res.name.endsWith("huntersbow") || res.name.endsWith("rangersbow")) && ed.res.sdt.peekrbuf(0) == 5) {
                                    targetting = true;
                                    if (bowvector == null) {
                                        bowvector = new Overlay(new GobArcheryVector(this, followGob));
                                        ols.add(bowvector);
                                    }
                                    break;
                                }
                            } catch (Loading l) { }
                        }
                        if (!targetting && bowvector != null) {
                            ols.remove(bowvector);
                            bowvector = null;
                        }
                    }

                }
            } catch (Exception e) {
                //TODO: This is a weird issue that can pop up on startup, need to look into it
                return false;
            }
            if (Config.showboundingboxes) {
                GobHitbox.BBox bbox = GobHitbox.getBBox(this);
                if (bbox != null)
                    rl.add(new Overlay(new GobHitbox(this, bbox.a, bbox.b, false)), null);
            }
            if (Config.showplantgrowstage) {
                if ((type != null && type == Type.PLANT) || (type != null && type == Type.MULTISTAGE_PLANT)) {
                    int stage = getattr(ResDrawable.class).sdt.peekrbuf(0);
                    if (cropstgmaxval == 0) {
                        for (FastMesh.MeshRes layer : getres().layers(FastMesh.MeshRes.class)) {
                            int stg = layer.id / 10;
                            if (stg > cropstgmaxval)
                                cropstgmaxval = stg;
                        }
                    }
                    Overlay ol = findol(Sprite.GROWTH_STAGE_ID);
                    Resource res = getres();
                    if (ol == null && (stage == cropstgmaxval || (Config.showfreshcropstage ? stage >= 0 : stage > 0) && stage < 6)) {
                        addol(new Gob.Overlay(Sprite.GROWTH_STAGE_ID, new PlantStageSprite(stage, cropstgmaxval, type == Type.MULTISTAGE_PLANT,(res!= null && res.basename().contains("turnip") || res!= null && res.basename().contains("leek")))));
                    } else if (!Config.showfreshcropstage && stage == 0 || (stage != cropstgmaxval && stage >= 6)) {
                        ols.remove(ol);
                    } else if (((PlantStageSprite) ol.spr).stg != stage) {
                        ((PlantStageSprite) ol.spr).update(stage, cropstgmaxval);
                    }
                }

                if (type == Type.TREE || type == Type.BUSH) {
                    ResDrawable rd = getattr(ResDrawable.class);
                    if (rd != null) {
                        int fscale = rd.sdt.peekrbuf(1);
                        if (fscale != -1) {
                            Overlay ol = findol(Sprite.GROWTH_STAGE_ID);

                            /*
                            if (ol == null) {
                                addol(new Gob.Overlay(Sprite.GROWTH_STAGE_ID, new TreeStageSprite(fscale)));
                            } else if (((TreeStageSprite) ol.spr).val != fscale) {
                                ((TreeStageSprite) ol.spr).update(fscale);
                            }
                            */

                            int minStage = (type == Type.TREE ? 10 : 30);
                            int growPercents = (int)Math.ceil( (float)(fscale - minStage) / (float)(100 - minStage) * 100f);
                            if (ol == null) {
                                addol(new Gob.Overlay(Sprite.GROWTH_STAGE_ID, new TreeStageSprite(growPercents)));
                            } else if (((TreeStageSprite) ol.spr).val != growPercents) {
                                ((TreeStageSprite) ol.spr).update(growPercents);
                            }
                        }
                    }
                }
            }
            if(Config.stranglevinecircle && type == Type.STRANGLEVINE){
                if(ols.size() > 0)
                    return (false);
                else
                    ols.add(new Gob.Overlay(new PartyMemberOutline(this,new Color(0,255,0, 255))));
            }
            if (Config.showanimalrad && type == Type.DANGANIMAL && Config.showanimalrad && !Config.doubleradius) {
                boolean hasradius = ols.contains(animalradius);
                if ((!isDead()) && !hasradius)
                    ols.add(animalradius);
                else if (isDead() && hasradius)
                    ols.remove(animalradius);
            }else if (Config.showanimalrad && type == Type.DANGANIMAL && Config.showanimalrad && Config.doubleradius){
                boolean hasradius = ols.contains(doubleanimalradius);
                if ((!isDead()) && !hasradius)
                    ols.add(doubleanimalradius);
                else if (isDead() && hasradius)
                    ols.remove(doubleanimalradius);
            }


            Speaking sp = getattr(Speaking.class);
            if (sp != null)
                rl.add(sp.fx, null);
            KinInfo ki = getattr(KinInfo.class);
            if (ki != null)
                rl.add(ki.fx, null);

            if(DefSettings.SHOWHITBOX.get() && hitboxmesh != null)
                rl.add(hitboxmesh, null);

        }
        return (false);
    }


    private static final Object DYNAMIC = new Object();
    private Object seq = null;
    public Object staticp() {
        if (type == Type.HUMAN)
            seq = DYNAMIC;

        if (seq != null) {
            return ((seq == DYNAMIC) ? null : seq);
        } else if (getattr(Hidden.class) == null) {
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
            synchronized (ols) {
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
            }
            if(getattr(KinInfo.class) != null) {
                rs = 2; //I want to see the names above fires/players without it being screwed up
            }
            switch(rs) {
                case 0:
                    seq = new Static();
                    break;
                case 1:
                    seq = new SemiStatic();
                    break;
                default:
                    seq = null;
                    break;
            }
            return((seq == DYNAMIC)?null:seq);
        } else {
            //New hidden gob
            return seq = getattr(Moving.class) == null ? STATIC : DYNAMIC;
        }
    }

    void changed() {
        seq = null;
    }

    public Random mkrandoom() {
        return (Utils.mkrandoom(id));
    }

    public Optional<String> resname() {
        return res().map((res) -> res.name);
    }

    public String name() {
        return resname().orElse("");
    }

    public Optional<Resource> res() {
        final Drawable d = getattr(Drawable.class);
        try {
            return d != null ? Optional.of(d.getres()) : Optional.empty();
        } catch (Exception e) {
            return Optional.empty();
        }
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

    public boolean isplayer() {
        try { //not clean but when multi-sessioning client can crash here when second client is booting.
            final UI ui = glob.ui.get();
            return ui.gui.map.plgob == id;
        }catch(Exception e){
            return false;
        }
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
