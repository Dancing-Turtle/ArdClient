package haven.sloth.script.pathfinding;

import haven.Coord;

import java.util.ArrayList;
import java.util.List;

/**
 * A path in our pathfinding solution.
 */
public class Path implements Comparable<Path> {
    //The coordinate we're on in this path
    public final Coord c;
    //The path we came from
    private Path parent;
    //The depth of this path in our solution tree
    private int depth;
    //The value of this path, in our case distance from the goal
    final double gval;
    //The heuristic value of this path, depth + gval;
    final double hval;

    public Path(final Coord c, final Path parent, final Coord goal) {
        this.c = c;
        this.parent = parent;
        if(this.parent != null) {
            this.depth = this.parent.depth + 1;
	} else {
            this.depth = 0;
	}
        this.gval = c.dist(goal);
        this.hval = depth + gval;
    }

    public Path(final Coord c, final Coord goal) {
        this(c, null, goal);
    }

    /**
     * List of all coordinates in our path starting with the first one and ending with our final coordinate
     */
    List<Coord> fullpath() {
	final List<Coord> paths = new ArrayList<>();
	Path cur = this.parent;
	paths.add(c);

	while(cur != null) {
	    paths.add(0,cur.c);
	    cur = cur.parent;
	}

	return paths;
    }

    @Override
    public int compareTo(Path o) {
	return Double.compare(hval, o.hval);
    }

    @Override
    public String toString() {
	return String.format("%s -> %f", c, gval);
    }
}
