package haven.sloth.script.pathfinding;

import haven.Coord;

import java.util.Objects;

public class Node implements Comparable<Node> {
    //Actual coordinate of this node
    public final Coord c;
    //Our current length g(n)
    public final double g;
    //Our Heuristic value h(n)
    public final double h;
    //Computed length f(n) = g(n) + h(n)
    public final double f;
    //Parent node
    public final Node parent;

    Node(final Node parent, final Coord c, final double g, final double h) {
        this.parent = parent;
        this.c = c;
        this.g = g;
        this.h = h;
        this.f = g + h;
    }

    @Override
    public int hashCode() {
        return Objects.hash(c.x, c.y);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof Node && ((Node) obj).c.equals(c);
    }

    @Override
    public int compareTo(Node o) {
        //for tie breaking look at h(n)
        return f != o.f ? Double.compare(f, o.f) : Double.compare(h, o.h);
    }
}
