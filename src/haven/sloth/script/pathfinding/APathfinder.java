package haven.sloth.script.pathfinding;

import haven.Coord;
import haven.UI;
import haven.DefSettings;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.*;
import java.util.List;

/**
 * TODO: Impossible path detection. Right now it goes into an infinite loop when it can't figure out how to path to the goal
 * due to the goal being impossible
 */
public class APathfinder extends Pathfinder {
    public APathfinder(final UI ui) {
        super(ui);
    }

    private List<Coord> findpath(final Coord st, final Coord goal, final boolean allowbest, final int level) {
        //Steps to attempt
        final PriorityQueue<Node> pq = new PriorityQueue<>();
        //Steps we already tried, no reason to do them again
        final Set<Coord> ignore = new HashSet<>();
        Node best;

        { //init
            final Node stnode = new Node(null, st, 0, heuristic.distance(st, goal));
            best = stnode;
            pq.add(stnode);
            ignore.add(stnode.c);
        }

        while (!pq.isEmpty()) {
            final Node node = pq.poll();

            if (!node.c.equals(goal)) {
                for (Coord step : dirs[level]) {
                    final Coord nc = node.c.add(step);
                    //skip over ones we already did
                    if (!ignore.contains(nc)) {
                        ignore.add(nc);
                        if (!checkHit(nc)) {
                            final Node child = new Node(node, nc, node.g + 1, heuristic.distance(nc, goal));
                            pq.add(child);
                            if (child.f < best.f) {
                                best = child;
                            }
                        }
                    }
                }
            } else {
                //Done, we hit our goal
                return collect(node);
            }
        }

        //never hit our goal, but we can go with our closest one
        if (allowbest) {
            return collect(best);
        }
        return null;
    }

    private List<Coord> debugpath(final Coord st, final Coord goal, final int level) {
        //Steps to attempt
        final PriorityQueue<Node> pq = new PriorityQueue<>();
        //Steps we already tried, no reason to do them again
        final Set<Coord> ignore = new HashSet<>();
        //Memory
        final Set<Node> nodes = new HashSet<>();
        Coord min = new Coord(0, 0), max = new Coord(0, 0);
        double maxdepth = 1;
        Coord last = null;

        { //init
            final Node stnode = new Node(null, st, 0, heuristic.distance(st, goal));
            pq.add(stnode);
            ignore.add(stnode.c);

            nodes.add(stnode);
            min.x = max.x = st.x;
            min.y = max.y = st.y;
        }

        while (!pq.isEmpty()) {
            final Node node = pq.poll();

            if (!node.c.equals(goal)) {
                for (Coord step : dirs[level]) {
                    final Coord nc = node.c.add(step);
                    //skip over ones we already did
                    if (!ignore.contains(nc)) {
                        ignore.add(nc);
                        if (!checkHit(nc)) {
                            final Node child = new Node(node, nc, node.g + 1, heuristic.distance(nc, goal));
                            pq.add(child);
                            nodes.add(child);
                            maxdepth = Math.max(maxdepth, node.g);
                            min.x = Math.min(min.x, nc.x);
                            max.x = Math.max(max.x, nc.x);
                            min.y = Math.min(min.y, nc.y);
                            max.y = Math.max(max.y, nc.y);
                        }
                    }
                }
            } else {
                //Done, we hit our goal
                last = node.c;
                break;
            }
        }

        //Render our debug image
        final BufferedImage buf = new BufferedImage(max.x - min.x + 1, max.y - min.y + 1, BufferedImage.TYPE_INT_RGB);
        final Color sc = Color.GREEN;
        final Color ec = Color.BLUE;

        for (final Node n : nodes) {
            final Coord tc = n.c.sub(min);
            final double x = Math.min((n.g) / maxdepth, 1);
            buf.setRGB(tc.x, tc.y, new Color(Math.min((int) (x * ec.getRed() + (1 - x) * sc.getRed()), 255),
                    Math.min((int) (x * ec.getGreen() + (1 - x) * sc.getGreen()), 255),
                    Math.min((int) (x * ec.getBlue() + (1 - x) * sc.getBlue()), 255)).getRGB());
        }
        {
            final Coord tc = st.sub(min);
            buf.setRGB(tc.x, tc.y, Color.BLACK.getRGB());
        }
        if (last != null) {
            final Coord tc = last.sub(min);
            buf.setRGB(tc.x, tc.y, Color.WHITE.getRGB());
        }

        try {
            javax.imageio.ImageIO.write(buf, "png", new File("apathfinder.png"));
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public ArrayList<Move> path(final Coord start, final Coord goal) {
        if (!DefSettings.DEBUG.get()) {
            return advreduce(findpath(start, goal, true, 0));
        } else {
            debugpath(start, goal, 0);
            return null;
        }
    }
}
