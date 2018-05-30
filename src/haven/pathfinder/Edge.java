package haven.pathfinder;

public class Edge {
    public final Vertex src;
    public final Vertex dest;
    public double weight;

    public Edge(Vertex src, Vertex dest, double w) {
        this.src = src;
        this.dest = dest;
        this.weight = w;
    }
}