package haven.sloth.script.pathfinding;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.sloth.gob.HeldBy;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Pathfinder {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    protected static final Coord[][] dirs = new Coord[1][8];
    protected static Hitbox plhb;
    static {
	plhb = Hitbox.hbfor("gfx/borka/body");

	//perfect
	dirs[0][0] = new Coord(1, 0);
	dirs[0][1] = new Coord(-1, 0);
	dirs[0][2] = new Coord(0, 1);
	dirs[0][3] = new Coord(0, -1);
	dirs[0][4] = new Coord(1, 1);
	dirs[0][5] = new Coord(1, -1);
	dirs[0][6] = new Coord(-1, -1);
	dirs[0][7] = new Coord(-1, 1);
    }

    @FunctionalInterface
    interface HitFun {
	boolean check(final Coord mc);
    }

    @FunctionalInterface
    interface HeuristicFun {
        double distance(final Coord c, final Coord goal);
    }

    protected final UI ui;
    private final HitFun hitfun;
    final HeuristicFun heuristic;

    Pathfinder(final UI ui) {
	this.ui = ui;
	//Check to see if we're boating
	hitfun = areWeBoating() ? this::hitOnBoat : this::hitOnLand;
	heuristic = this::manhattanDistance;
    }

    //D and D2 can scale based off terrain/speed we can run, future something to look at maybe.
    private static final double D = 1;
    private static final double D2 = Math.sqrt(2);
    private double diagonalDistance(final Coord c, final Coord goal) {
	final double dx = Math.abs(c.x - goal.x);
	final double dy = Math.abs(c.y - goal.y);
	return D * (dx + dy) + (D2 - 2 * D) * Math.min(dx, dy);
    }

    private double manhattanDistance(final Coord c, final Coord goal) {
	final double dx = Math.abs(c.x - goal.x);
	final double dy = Math.abs(c.y - goal.y);
	return D * (dx + dy);
    }

    private boolean areWeBoating() {
	final Gob me = ui.sess.glob.oc.getgob(ui.gui.map.plgob);
	if(me != null) {
	    return me.getattr(HeldBy.class) != null;
	} else {
	    return false;
	}
    }

    /**
     * Did we hit a bad spot at this coordinate?
     * Good spots are null or PLAYER
     *
     * This is two checks
     * Are we on a bad tile and is our gob hitbox overlapping another
     */
    private boolean hitGob(final Coord mc) {
	final Coord c = mc.add(plhb.offset());
	final Coord br = c.add(plhb.size());

	Coord xy = new Coord(0, 0);
	for(xy.x = c.x; xy.x < br.x; ++xy.x)
	    for(xy.y = c.y; xy.y < br.y; ++xy.y)
		if (ui.sess.glob.gobhitmap.checkHit(xy))
		    return true;
	return false;
    }

    /**
     * In this case water tiles are safe, everything else = no no
     *
     * TODO: plhb is problem slightly too big for this since tiles will let you usually overlap a bit
     */
    private boolean hitOnBoat(final Coord mc) {
	final Coord c = mc.add(plhb.offset());
	final Coord br = c.add(plhb.size());

	Coord xy = new Coord(0, 0);
	for(xy.x = c.x; xy.x < br.x; ++xy.x)
	    for(xy.y = c.y; xy.y < br.y; ++xy.y) {
		final Tile t = ui.sess.glob.map.gethitmap(xy.div(MCache.tilesz2));
		if (t != Tile.DEEPWATER && t != Tile.SHALLOWWATER)
		    return true;
	    }
	return false;
    }

    /**
     * In this case everything is safe except for water and cave walls and ridges
     *
     * TODO: plhb is problem slightly too big for this since tiles will let you usually overlap a bit
     *       Especially the case in caves, not so much with ridges...
     */
    private boolean hitOnLand(final Coord mc) {
	final Coord c = mc.add(plhb.offset());
	final Coord br = c.add(plhb.size());

	Coord xy = new Coord(0, 0);
	for(xy.x = c.x; xy.x < br.x; ++xy.x)
	    for(xy.y = c.y; xy.y < br.y; ++xy.y)
		if(ui.sess.glob.map.gethitmap(xy.div(MCache.tilesz2)) != null)
		    return true;
	return false;
    }

    final boolean checkHit(final Coord mc) {
	return hitGob(mc) || hitfun.check(mc);
    }

    /**
     * Walks a path between two points to see if we'll hit anything
     */
    final protected boolean walk(final Coord start, final Coord end) {
	if(end.x - start.x != 0) {
	    final double slope = (double)(end.y-start.y)/(double)(end.x-start.x);
	    final double b = -(slope*start.x)+start.y;
	    double dx = end.x - start.x;
	    double dy = end.y - start.y;
	    if(Math.abs(dy) > Math.abs(dx)) {
		dy = dy < 0 ? -1 : dy > 0 ? 1 : 0;
		int x, y;
		for(y = start.y; y != end.y; y += dy) {
		    x = (int) ((y - b)/slope);
		    if(checkHit(new Coord(x,y)))
			return false;
		}
	    } else {
		dx = dx < 0 ? -1 : dx > 0 ? 1 : 0;
		int x, y;
		for(x = start.x; x != end.x; x += dx) {
		    y = (int) (slope * x + b);
		    if(checkHit(new Coord(x,y)))
			return false;
		}
	    }
	} else {
	    //our x's are the same
	    //walking north/south
	    int dy = end.y - start.y;
	    dy = Integer.compare(dy, 0);
	    int y;
	    for(y = start.y; y != end.y; y += dy) {
		if(checkHit(new Coord(start.x,y)))
		    return false;
	    }
	}
	return true;
    }

    final List<Coord> collect(final Node end) {
        final ArrayList<Coord> moves = new ArrayList<>();
        moves.add(end.c);
        for(Node next = end.parent; next != null; next = next.parent) {
            moves.add(next.c);
	}
        //reverse start -> finish
	Collections.reverse(moves);
        return moves;
    }


    /**
     * Reduce the nodes we have into lines the end points will be our clicks
     * to walk the path
     *
     * TODO: this could be improved by trying farthest away first rather than closest. Even binary search
     *
     * In a way this tries to improve our result since it operates with the assumption that our List<Coord>
     *     may not be as optimal as we think or not optimal in the sense of how many clicks we have to do
     *     more clicks -> Slowdown -> bad and we'd rather have long straight lines rather than many short
     */
    final ArrayList<Move> advreduce(List<Coord> lines) {
        if(lines != null) {
	    final ArrayList<Move> blines = new ArrayList<>(lines.size());
	    Coord cur, next;
	    Coord best = null;
	    int i, j, besti = 0;
	    for (i = 0; i < lines.size(); ++i) {
		cur = lines.get(i);
		for (j = i + 1; j < lines.size(); ++j) {
		    next = lines.get(j);
		    if (walk(cur, next)) {
			best = next;
			besti = j;
		    }
		}
		if (best != null) {
		    blines.add(new Move(new Coord2d(best)));
		    i = besti;
		    best = null;
		} else {
		    blines.add(new Move(new Coord2d(cur)));
		}
	    }
	    return blines;
	} else {
            return null;
	}
    }

}
