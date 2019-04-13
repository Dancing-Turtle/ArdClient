package haven.sloth.script.pathfinding;

import haven.*;
import haven.sloth.util.ResHashMap;

import java.util.Optional;

/**
 * Gob Hitbox
 */
public class Hitbox {
    private static final ResHashMap<Hitbox> hitboxes = new ResHashMap<>();
    private static final Hitbox NOHIT = new Hitbox();
    private static final int BUFFER_SIZE = 2;
    static {
        hitboxes.put("gfx/terobjs/herbs", NOHIT);
        hitboxes.put("gfx/terobjs/items", NOHIT);
        hitboxes.put("gfx/terobjs/plants", NOHIT);

        //misc
        hitboxes.put("gfx/terobjs/consobj", new Hitbox(new Coord(-4, -4), new Coord(8, 8)));
        hitboxes.put("gfx/terobjs/skeleton", new Hitbox(new Coord(-4, -4), new Coord(8, 8)));
        hitboxes.put("gfx/terobjs/clue", NOHIT);
        hitboxes.put("gfx/terobjs/boostspeed", NOHIT);
        hitboxes.put("gfx/kritter/jellyfish/jellyfish", NOHIT);
        //Knarrs seem to just take the hitbox of a player?
        hitboxes.put("gfx/terobjs/vehicle/knarr", new Hitbox(new Coord(-4, -4), new Coord(8, 8)));

        //stone This looks wrong...
        hitboxes.put("gfx/terobjs/bumlings", new Hitbox(new Coord(8, 8), new Coord(-16, -16)));

        //walls
        //XXX: loftar's real hitbox size for this is certainly a decimal..
        final Hitbox wallseg = new Hitbox(new Coord(-5, -5), new Coord(11, 11));
        final Hitbox gate = new Hitbox(new Coord(-5, -10), new Coord(11, 22));
        final Hitbox biggate = new Hitbox(new Coord(-5, -16), new Coord(11, 33));
        hitboxes.put("gfx/terobjs/arch/brickwallcp", wallseg);
        hitboxes.put("gfx/terobjs/arch/brickwallseg", wallseg);
        hitboxes.put("gfx/terobjs/arch/brickwallgate", gate);
        hitboxes.put("gfx/terobjs/arch/brickwallbiggate", biggate);
        hitboxes.put("gfx/terobjs/arch/palisadecp", wallseg);
        hitboxes.put("gfx/terobjs/arch/palisadeseg", wallseg);
        hitboxes.put("gfx/terobjs/arch/palisadegate", gate);
        hitboxes.put("gfx/terobjs/arch/palisadebiggate", biggate);
        hitboxes.put("gfx/terobjs/arch/poleseg", wallseg);
        hitboxes.put("gfx/terobjs/arch/polecp", wallseg);
        hitboxes.put("gfx/terobjs/arch/polegate", gate);
        hitboxes.put("gfx/terobjs/arch/polebiggate", biggate);
        hitboxes.put("gfx/terobjs/arch/drystonewallseg", wallseg);
        hitboxes.put("gfx/terobjs/arch/drystonewallcp", wallseg);
        hitboxes.put("gfx/terobjs/arch/drystonewallgate", gate);
        hitboxes.put("gfx/terobjs/arch/drystonewallbiggate", biggate);
        hitboxes.put("gfx/terobjs/arch/hwall", new Hitbox(new Coord(-1, 0), new Coord(1, 11)));

        //animals
        hitboxes.put("gfx/kritter/horse", new Hitbox(new Coord(-8, -4), new Coord(16, 8)));
        hitboxes.put("gfx/kritter/cattle/calf", new Hitbox(new Coord(-9, -3), new Coord(18, 6)));
        hitboxes.put("gfx/kritter/cattle/cattle", new Hitbox(new Coord(-12, -4), new Coord(24, 8)));
        hitboxes.put("gfx/kritter/pig", new Hitbox(new Coord(-6, -3), new Coord(12, 6)));
        hitboxes.put("gfx/kritter/goat", new Hitbox(new Coord(-6, -2), new Coord(12, 4)));
        hitboxes.put("gfx/kritter/sheep/lamb", new Hitbox(new Coord(-6, -2), new Coord(12, 4)));
    }

    //Offset and Size with a "buffer" around it to avoid clipping
    //After floating point movement changes and "pushing" effect this might not be needed outside of
    //cliffs and cave walls
    private final Coord off;
    private final Coord sz;

    private final boolean hitable;

    public Hitbox(final Coord off, final Coord sz, boolean hitable, boolean buffer) {
        this.off = !buffer ? off : off.add(BUFFER_SIZE, BUFFER_SIZE);
        this.sz = !buffer ? sz : sz.add(BUFFER_SIZE*2, BUFFER_SIZE*2);
        this.hitable = hitable;
    }

    public Hitbox(final Coord off, final Coord sz) {
        this(off, sz, true, false);
    }

    public Hitbox() {
        this(Coord.z, Coord.z, false, false);
    }


    public Coord offset() { return off; }
    public Coord size() { return sz; }
    boolean canHit() { return hitable; }


    private static Hitbox loadHitboxFromRes(final Resource res) {
        final Resource.Neg neg = res.layer(Resource.negc);
        if(neg != null) {
            Coord hsz = new Coord(Math.abs(neg.bc.x) + Math.abs(neg.bs.x) + 1,
                    Math.abs(neg.bc.y) + Math.abs(neg.bs.y) + 1);
            Coord hoff = neg.bc;
            final Hitbox hb = new Hitbox(hoff, hsz, true, false);
            hitboxes.put(res.name, hb);
            return hb;
        } else {
            for(RenderLink.Res link : res.layers(RenderLink.Res.class)) {
                final Optional<Resource> meshres = link.mesh();
                if(meshres.isPresent()) {
                    final Resource.Neg meshneg = meshres.get().layer(Resource.negc);
                    if(meshneg != null) {
                        Coord hsz = new Coord(Math.abs(meshneg.bc.x) + Math.abs(meshneg.bs.x) + 1,
                                Math.abs(meshneg.bc.y) + Math.abs(meshneg.bs.y) + 1);
                        Coord hoff = meshneg.bc;
                        final Hitbox hb = new Hitbox(hoff, hsz, true, false);
                        hitboxes.put(res.name, hb);
                        return hb;
                    }
                }
            }
            return null;
        }
    }


    public static Hitbox hbfor(final String res) {
        return hitboxes.get(res).orElse(null);
    }


    public static Hitbox hbfor(final Gob g) {
        return hbfor(g, false);
    }

    public static Hitbox hbfor(final Gob g, final boolean force) {
        final Optional<Resource> res = g.res();
        if(res.isPresent()) {
            if(!force) {
                if (!res.get().name.endsWith("gate") && !res.get().name.endsWith("/pow")) {
                    return hitboxes.get(res.get().name).orElse(loadHitboxFromRes(res.get()));
                } else if (res.get().name.endsWith("gate") && res.get().name.startsWith("gfx/terobjs/arch")) {
                    ResDrawable rd = g.getattr(ResDrawable.class);
                    if (rd != null && (rd.sdtnum() == 1)) {
                        return NOHIT;
                    } else {
                        return hitboxes.get(res.get().name).orElse(loadHitboxFromRes(res.get()));
                    }
                } else if (res.get().name.endsWith("/pow")) {
                    ResDrawable rd = g.getattr(ResDrawable.class);
                    if (rd != null && (rd.sdtnum() == 17 || rd.sdtnum() == 33)) {
                        return NOHIT;
                    } else {
                        return hitboxes.get(res.get().name).orElse(loadHitboxFromRes(res.get()));
                    }
                } else {
                    return hitboxes.get(res.get().name).orElse(loadHitboxFromRes(res.get()));
                }
            } else {
                return hitboxes.get(res.get().name).orElse(loadHitboxFromRes(res.get()));
            }
        } else {
            return null;
        }
    }
}
