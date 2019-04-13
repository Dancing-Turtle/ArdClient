package haven.sloth.script.pathfinding;

import haven.*;

public class Move {
    private static final Coord fake = new Coord(1, 1);
    private final Coord2d dest;
    public Move(final Coord2d c) {
	dest = c;
    }

    public void apply(MapView mv) {
        mv.wdgmsg("click", fake, dest.floor(OCache.posres), 1, 0);
    }

    public Coord2d dest() {
        return dest;
    }

    public static class ClickInfo {
        final int olclicked;
        final Gob gob;
        final int olid;
        final int meshid;

	/**
	 * inf comes in as:
	 * { 0, gob, 0, -1 } if no ol or fastmesh clicked
	 * { 1, gob, ol.id, -1 } if ol clicked
	 * { 0, gob, 0, FastMesh.id } if fastmesh clicked
	 * { 1, gob, ol.id, FastMesh.id } if ol clicked and fastmesh clicked
	 */
        public ClickInfo(final Object[] inf) {
            olclicked = (Integer)inf[0];
            gob = (Gob)inf[1];
            olid = (Integer)inf[2];
            meshid = (Integer)inf[3];
	}
    }

    public static class Interact extends Move {
	final ClickInfo inf;
	final int cb, flags;
	public Interact(final ClickInfo inf, int clickb, int flags) {
	    super(inf.gob.rc);
	    this.inf = inf;
	    this.flags = flags;
	    cb = clickb;
	}

	public void apply(MapView mv) {
	    if(inf.olclicked == 0) {
                mv.wdgmsg("click", fake, inf.gob.rc.floor(OCache.posres), cb, flags, 0, (int) inf.gob.id, inf.gob.rc.floor(OCache.posres), 0, inf.meshid);
	    } else {
                mv.wdgmsg("click", fake, inf.gob.rc.floor(OCache.posres), cb, flags, 1, (int) inf.gob.id, inf.gob.rc.floor(OCache.posres), inf.olid, inf.meshid);
	    }

	}
    }
}
