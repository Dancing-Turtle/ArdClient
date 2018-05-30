package haven.pathfinder;

import java.util.HashSet;
import java.util.Set;

public class Vertex {
    public final int x;
    public final int y;
    public final Set<Edge> edges = new HashSet<Edge>();

    public Vertex(int x, int y) {
        this.x = x;
        this.y = y;
    }
}