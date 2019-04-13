package haven.sloth.script.pathfinding;

import haven.Coord;
import haven.UI;
import haven.DefSettings;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.*;
import java.util.List;

public class NBAPathfinder extends Pathfinder {
    private final int level;
    private double best = Double.MAX_VALUE;
    private Coord touched = null;

    public NBAPathfinder(final UI ui) {
        super(ui);
        level = 0;
    }

    /**
     * This executes from both sides simultaneously
     * s is the source for this side
     * t is the destination for this side
     * <p>
     * 1: S = ∅;
     * 2: R = ∅; // S and R are sets
     * 3: M = V ; // M is a shared set of all verts in our map
     * 4: L = ∞; // L is a shared real value
     * ////5: for all v ∈ V do
     * ////6:   g(v) = ∞;
     * ////7: end for
     * 8: g(s) = 0;        //First g(s) is 0
     * 9: f = g(s) + h(s); //First f is g(s) + h(s) -> distance to t
     * 10: while any v ∈ M has g(v) < ∞ do //While we have verts that have g(v) defined
     * 11:   u0 = arg min{g(v) + h(v) | v ∈ M}; // u0 is selected by taking the min vert with g(v) + h(v)
     * 12:   M = M − {u0}; //remove it from the set
     * 13:   if g(u0) + h(u0) − h(t) ≥ L or g(u0) + ˜f − h˜(u0) ≥ L then
     * 14:     R = R + {u0}; // u0 is rejected if
     * 15:   else
     * 16:     S = S + {u0}; // u0 is stabilized, create children
     * 17:     for all edges (u0, v) ∈ E with v ∈ M do           //for each child
     * 18:       g(v) = min(g(v), g(u0) + d(u0, v));              //define g(v)
     * 19:       L = min(L, g(v) + ˜g(v)); //Set L to min of L or hypothetically the distance from start -> v + v -> target
     * 20:     end for
     * 21:   end if
     * 22:   f = min{g(v) + h(v) | v ∈ M};
     * 23: end while
     */
    private List<Coord> findpath(final Coord start, final Coord goal) {
        final PriorityQueue<Node> startpq = new PriorityQueue<>();
        final PriorityQueue<Node> goalpq = new PriorityQueue<>();
        final Map<Coord, Node> startNodes = new HashMap<>();
        final Map<Coord, Node> endNodes = new HashMap<>();
        final Set<Coord> ignore = new HashSet<>();
        double fs;
        double ft;

        {//Init
            /*
             * 8: g(s) = 0;        //First g(s) is 0
             * 9: f = g(s) + h(s); //First f is g(s) + h(s) -> distance to t
             */
            final Node stnode = new Node(null, start, 0, heuristic.distance(start, goal));
            final Node endnode = new Node(null, goal, 0, heuristic.distance(goal, start));
            startpq.add(stnode);
            startNodes.put(start, stnode);
            goalpq.add(endnode);
            endNodes.put(goal, endnode);
            fs = ft = heuristic.distance(start, goal);
        }

        final boolean earlyexit = DefSettings.PATHFINDINGTIER.get() == 2;

        //10: while any v ∈ M has g(v) < ∞ do //While we have verts that have g(v) defined
        while (!startpq.isEmpty() && !goalpq.isEmpty()) {
            //Do it in lock step to simulate it being simultaneous.
            fs = expand(startpq, goal, start, fs, ft, startNodes, endNodes, ignore);
            ft = expand(goalpq, start, goal, ft, fs, endNodes, startNodes, ignore);

            if (!earlyexit || touched == null)
                continue;
            break;
        }

        if (touched != null) {
            //start -> mid
            final List<Coord> fhalf = collect(startNodes.get(touched));
            //goal -> mid
            final List<Coord> lhalf = collect(endNodes.get(touched));
            lhalf.remove(lhalf.size() - 1);
            Collections.reverse(lhalf);
            final List<Coord> combined = new ArrayList<>(fhalf.size() + lhalf.size());
            combined.addAll(fhalf);
            combined.addAll(lhalf);
            if (DefSettings.DEBUG.get())
                dump(startNodes, endNodes);
            return combined;
        } else {
            if (DefSettings.DEBUG.get())
                dump(startNodes, endNodes);
            return null;
        }
    }

    private double expand(final PriorityQueue<Node> M, final Coord target, final Coord source,
                          final double f, final double ftilda,
                          final Map<Coord, Node> myNodes, final Map<Coord, Node> otherNodes,
                          final Set<Coord> rejected) {
        if (!M.isEmpty()) {
            //11:   u0 = arg min{g(v) + h(v) | v ∈ M}; // u0 is selected by taking the min vert with g(v) + h(v)
            final Node node = M.poll();
            //13:   if g(u0) + h(u0) − h(t) ≥ L or g(u0) + ˜f − h˜(u0) ≥ L then // - h(t) -> -0???
            //negate to only consider non-rejected nodes
            if (!(node.f >= best || (node.g + ftilda - heuristic.distance(node.c, source)) >= best)) {
                //16:     S = S + {u0}; // u0 is stabilized, create children
                //17:     for all edges (u0, v) ∈ E with v ∈ M do           //for each child
                for (final Coord dir : dirs[level]) {
                    final Coord nc = node.c.add(dir);
                    //with v ∈ M do
                    if (!rejected.contains(nc)) {
                        //12:   M = M − {u0}; //remove it from the set
                        rejected.add(nc);
                        //Ensure we didn't clip a hitbox or bad tile
                        if (!checkHit(nc)) {
                            //18:       g(v) = min(g(v), g(u0) + d(u0, v));
                            final Node child = new Node(node, nc, node.g + 1, heuristic.distance(nc, target));
                            M.add(child);
                            myNodes.put(nc, child);
                        }
                    } else if (otherNodes.containsKey(nc)) { //19:       L = min(L, g(v) + ˜g(v));
                        final Node child = new Node(node, nc, node.g + 1, heuristic.distance(nc, target));
                        myNodes.put(nc, child);
                        if (child.g + otherNodes.get(nc).g < best) {
                            best = child.g + otherNodes.get(nc).g;
                            touched = nc;
                        }
                    }
                }
            }
        }

        return !M.isEmpty() ? M.peek().f : f;
    }

    private void dump(Map<Coord, Node> start, Map<Coord, Node> end) {
        Coord min = new Coord(start.values().iterator().next().c);
        Coord max = new Coord(min);
        double maxsdepth = 1, maxtdepth = 1;

        for (final Coord c : start.keySet()) {
            min.x = Math.min(min.x, c.x);
            max.x = Math.max(max.x, c.x);
            min.y = Math.min(min.y, c.y);
            max.y = Math.max(max.y, c.y);
            maxsdepth = Math.max(maxsdepth, start.get(c).g);
        }
        for (final Coord c : end.keySet()) {
            min.x = Math.min(min.x, c.x);
            max.x = Math.max(max.x, c.x);
            min.y = Math.min(min.y, c.y);
            max.y = Math.max(max.y, c.y);
            maxtdepth = Math.max(maxtdepth, end.get(c).g);
        }

        //Render our debug image
        final BufferedImage buf = new BufferedImage(max.x - min.x + 1, max.y - min.y + 1, BufferedImage.TYPE_INT_RGB);
        {
            final Color sc = Color.GREEN;
            final Color ec = Color.BLUE;

            for (final Node n : start.values()) {
                final Coord tc = n.c.sub(min);
                final double x = Math.min((n.g) / maxsdepth, 1);
                buf.setRGB(tc.x, tc.y, new Color(Math.min((int) (x * ec.getRed() + (1 - x) * sc.getRed()), 255),
                        Math.min((int) (x * ec.getGreen() + (1 - x) * sc.getGreen()), 255),
                        Math.min((int) (x * ec.getBlue() + (1 - x) * sc.getBlue()), 255)).getRGB());
            }
        }
        {
            final Color sc = Color.RED;
            final Color ec = Color.ORANGE;

            for (final Node n : end.values()) {
                final Coord tc = n.c.sub(min);
                final double x = Math.min((n.g) / maxtdepth, 1);
                buf.setRGB(tc.x, tc.y, new Color(Math.min((int) (x * ec.getRed() + (1 - x) * sc.getRed()), 255),
                        Math.min((int) (x * ec.getGreen() + (1 - x) * sc.getGreen()), 255),
                        Math.min((int) (x * ec.getBlue() + (1 - x) * sc.getBlue()), 255)).getRGB());
            }
        }

        try {
            javax.imageio.ImageIO.write(buf, "png", new File("nbapathfinder.png"));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public ArrayList<Move> path(final Coord start, final Coord goal) {
        return advreduce(findpath(start, goal));
    }
}
