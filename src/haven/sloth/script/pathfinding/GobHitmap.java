package haven.sloth.script.pathfinding;

import com.google.common.flogger.FluentLogger;
import haven.Coord;
import haven.Gob;
import haven.OCache;
import haven.UI;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.*;

/**
 * A hitmap for Gobs
 * Conflicts are resolves by keeping a list of gobs that are on a given tile
 * Player gob is ignored
 */
public class GobHitmap {
    private final static HitTile nulltile = new HitTile(null);
    private static class HitTile {
        private Tile tile;
        private Set<Long> ownedby = new HashSet<>();

        private HitTile(final Tile t) {
            this.tile = t;
	}

	public void add(Gob g) {
            ownedby.add(g.id);
	}

	public void rem(Gob g) {
            ownedby.remove(g.id);
	}

	public boolean isEmpty() { return ownedby.isEmpty(); }
    }

    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private final Map<Coord, HitTile> map = new HashMap<>();

    public synchronized BufferedImage debug2(final Coord tl, final Coord br) {
        //Update tl/br if needed
        for (final Coord c : map.keySet()) {
            if (c.x < tl.x)
                tl.x = c.x;
            else if (c.x > br.x)
                br.x = c.x;

            if (c.y < tl.y)
                tl.y = c.y;
            else if (c.y > br.y)
                br.y = c.y;
        }
        final BufferedImage buf = new BufferedImage(br.x - tl.x + 1, br.y - tl.y + 1, BufferedImage.TYPE_INT_RGB);

        //Render our hitmap
        final HashMap<Tile, Color> tmap = new HashMap<>();
        tmap.put(Tile.GOB, Color.RED);
        tmap.put(Tile.DEEPWATER, Color.BLUE);
        tmap.put(Tile.SHALLOWWATER, Color.CYAN);
        tmap.put(Tile.CAVE, Color.GRAY);
        tmap.put(Tile.RIDGE, Color.YELLOW);
        tmap.put(Tile.PLAYER, Color.GREEN);

        for (final Coord c : map.keySet()) {
            final Coord offset = c.sub(tl);
            buf.setRGB(offset.x, offset.y, tmap.get(map.get(c).tile).getRGB());
        }

        return buf;
    }

    public synchronized void debug() {
        if(map.size() > 0) {
	    //find our boundaries
	    Coord tl = new Coord(map.keySet().iterator().next());
	    Coord br = new Coord(tl);
	    for (final Coord c : map.keySet()) {
		if (c.x < tl.x)
		    tl.x = c.x;
		else if (c.x > br.x)
		    br.x = c.x;

		if (c.y < tl.y)
		    tl.y = c.y;
		else if (c.y > br.y)
		    br.y = c.y;
	    }
            final BufferedImage buf = new BufferedImage(br.x - tl.x + 1, br.y - tl.y + 1, BufferedImage.TYPE_INT_RGB);
	    final HashMap<Tile, Color> tmap = new HashMap<>();
	    tmap.put(Tile.GOB, Color.RED);
	    tmap.put(Tile.DEEPWATER, Color.BLUE);
	    tmap.put(Tile.SHALLOWWATER, Color.CYAN);
	    tmap.put(Tile.CAVE, Color.GRAY);
	    tmap.put(Tile.RIDGE, Color.YELLOW);
	    tmap.put(Tile.PLAYER, Color.GREEN);

	    for (final Coord c : map.keySet()) {
		final Coord offset = c.sub(tl);
		buf.setRGB(offset.x, offset.y, tmap.get(map.get(c).tile).getRGB());
	    }

	    try {
		javax.imageio.ImageIO.write(buf, "png", new File("hitmap4.png"));
	    } catch (Exception e) {
		e.printStackTrace();
	    }
	}
    }

    public synchronized boolean checkHit(final Coord c) {
        return map.containsKey(c);
    }

    public synchronized List<Coord> add(final Gob g) {
        final UI ui = g.glob.ui.get();
        if (ui != null && g.id != ui.gui.map.plgob && !(g instanceof OCache.Virtual) && g.id >= 0) {
	    return fill(g);
	} else {
            return null;
	}
    }

    public synchronized void rem(final Gob g, final List<Coord> coords) {
        if(coords != null) {
	    for (final Coord c : coords) {
	        final HitTile tile = map.get(c);
	        if(tile != null) {
		    tile.rem(g);
		    if (tile.isEmpty()) {
			map.remove(c);
		    }
		}
	    }
	}
    }

    //Some modified Bresenham's line alg for steep slopes from a berkeley slide
    private void drawline(Coord c1, Coord c2, final Gob g, final List<Coord> coords) {
        drawline(c1.x, c1.y, c2.x, c2.y, g, coords);
    }

    /**
     * Drawing the outline of our gob
     */
    private void drawline(int x0, int y0, int x1, int y1, final Gob g, final List<Coord> coords) {
	boolean steep = Math.abs(y1-y0) > Math.abs(x1-x0);
	if(steep) {
	    //Reverse the axis
	    int t = x0;
	    x0 = y0;
	    y0 = t;

	    t = x1;
	    x1 = y1;
	    y1 = t;
	}
	if(x0 > x1) {
	    int t = x0;
	    x0 = x1;
	    x1 = t;

	    t = y0;
	    y0 = y1;
	    y1 = t;
	}

	float
		slope = ((float) y1-y0) / ((float) x1-x0),
		dErr = Math.abs(slope);
	int yStep = y0 > y1 ? -1 : 1;
	float err = 0.0f;
	int y = y0;

	for(int x = x0; x <= x1; ++x) {
	    final Coord c;
	    if(steep) {
		//Reverse the axis
	        c = new Coord(y, x);
	    } else {
	        c = new Coord(x, y);
	    }
	    map.put(c, map.getOrDefault(c, new HitTile(Tile.GOB)));
	    map.get(c).add(g);
	    coords.add(c);

	    err += dErr;
	    if(err >= 0.5f) {
		y += yStep;
		err -= 1.0;
	    }
	}
    }

    /**
     * Filling in the inner space of a gob that is at an angle.
     */
    private void fillspace(Coord start, final Gob g, final List<Coord> coords) {
	ArrayDeque<Coord> queue = new ArrayDeque<>();
	queue.push(start);

	Coord c;
	while(queue.size() > 0) {
	    c = queue.pop();
	    if(map.getOrDefault(c, nulltile).tile != Tile.GOB) {
		map.put(c, map.getOrDefault(c, new HitTile(Tile.GOB)));
		map.get(c).add(g);
		coords.add(c);
		queue.add(c.add(1,0));
		queue.add(c.add(0,1));
		queue.add(c.add(-1,0));
		queue.add(c.add(0,-1));
	    }
	}
    }

    private List<Coord> fill(final Gob g) {
        final List<Coord> coords = new ArrayList<>();
	final Hitbox hb = Hitbox.hbfor(g);
	if(hb != null) {
	    if(hb.canHit()) {
		final Coord hoff = new Coord(hb.offset());
		final Coord hsz = new Coord(hb.size());
		final double gd = Math.toDegrees(g.a);
		if(gd == 90 || gd == 270) {
		    //Easy case, simple rotations
		    //Reverse the axis
		    int tmp = hoff.x;
		    hoff.x = hoff.y;
		    hoff.y = tmp;
		    tmp = hsz.x;
		    hsz.x = hsz.y;
		    hsz.y = tmp;
		} else if(gd != 0 && gd != 180) {
		    //some angle that's not trival
		    //idea: calculate the four corner points
		    //draw line between these four corner points
		    //then do a fill within these 4 lines that will fill any
		    //point not filled within it already and check for more until
		    //no more are found

		    //Only problem is making sure that initial point within the four lines
		    // is actually within (half-way point between two opposite corners to get
		    // center of rectangle hitbox)
		    Coord
			    tl = hoff,
			    tr = hoff.add(hsz.x, 0),
			    bl = hoff.add(0, hsz.y),
			    br = hoff.add(hsz);
		    //rotate
		    tl = tl.rot((float)g.a);
		    tr = tr.rot((float)g.a);
		    bl = bl.rot((float)g.a);
		    br = br.rot((float)g.a);

		    //translate back
                    final Coord gc = g.getc().round();
		    tl = gc.add(tl);
		    tr = gc.add(tr);
		    bl = gc.add(bl);
		    br = gc.add(br);

		    //Draw lines
		    drawline(tl,tr,g,coords);
		    drawline(tr,br,g,coords);
		    drawline(bl,br,g,coords);
		    drawline(tl,bl,g,coords);

		    //Fill from center
		    final Coord center = new Coord( (tl.x + br.x)/2.0f, (tl.y+br.y)/2.0f);
		    fillspace(center,g,coords);
		    return coords;
		}

		//Handle gd = 0, 90, 180 or 270
                Coord off = g.getc().round().add(hoff);
		Coord br = off.add(hsz);

		int x, y;
		for(x = off.x; x < br.x; ++x)
		    for(y = off.y; y < br.y; ++y) {
			final Coord c = new Coord(x, y);
			map.put(c, map.getOrDefault(c, new HitTile(Tile.GOB)));
			map.get(c).add(g);
			coords.add(c);
		    }
	    }
	} else {
	    logger.atFine().log("No hitbox found for %s", g.resname());
	}
	return coords;
    }
}
