package haven;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.imageio.ImageIO;

import haven.resutil.Ridges;


public class MapGridSave {
    private MCache map;
    private MCache.Grid g;
    public static Coord gul;
    public static Coord mgs;
    private static String session;
    private static Map<Coord, Long> sessionIds = new HashMap<>();

    public MapGridSave(MCache map, MCache.Grid g) {
        this.map = map;
        this.g = g;

        int x = Math.abs(g.gc.x);
        int y = Math.abs(g.gc.y);

        synchronized (MapGridSave.class) {
            if (x == 0 && y == 0 || x == 10 && y == 10 || mgs == null) {
                session = (new SimpleDateFormat("yyyy-MM-dd HH.mm.ss")).format(new Date(System.currentTimeMillis()));
                sessionIds.clear();
                (new File("map/" + session)).mkdirs();
                mgs = g.gc;
                gul = g.ul;
            }

            BufferedImage img = drawmap(MCache.cmaps);
            if (img != null)
                save(img);
        }
    }

    public void save(BufferedImage img) {
        Coord normc = g.gc.sub(mgs);

        Long knownId = sessionIds.get(normc);
        if (knownId == null)
            sessionIds.put(normc, g.id);
        // tiles might arrive out of order, so we defer those until new session has been created
        else if (knownId != g.id)
            throw new Loading();

        String fileName = String.format("map/%s/tile_%d_%d.png", session, normc.x, normc.y);
        try {
            File outputfile = new File(fileName);
            ImageIO.write(img, "png", outputfile);
        } catch (IOException e) {
            return;
        }

        if (knownId == null) {
            BufferedWriter bw = null;
            try {
                bw = new BufferedWriter(new FileWriter(String.format("map/%s/ids.txt", session), true));
                bw.write(String.format("%d,%d,%d\n", normc.x, normc.y, g.id));
            } catch (IOException e) {
                e.printStackTrace();
            } finally {
                if (bw != null) {
                    try {
                        bw.close();
                    } catch (IOException e) {
                    }
                }
            }
        }
    }

    private BufferedImage tileimg(int t, BufferedImage[] texes) {
        BufferedImage img = texes[t];
        if (img == null) {
            Resource r = map.tilesetr(t);
            if (r == null)
                return (null);
            Resource.Image ir = r.layer(Resource.imgc);
            if (ir == null)
                return (null);
            img = ir.img;
            texes[t] = img;
        }
        return (img);
    }

    public BufferedImage drawmap(Coord sz) {
        BufferedImage[] texes = new BufferedImage[256];
        BufferedImage buf = TexI.mkbuf(sz);
        Coord c = new Coord();
        int blackpxs = 0;
        for (c.y = 0; c.y < sz.y; c.y++) {
            for (c.x = 0; c.x < sz.x; c.x++) {
                BufferedImage tex = tileimg(g.gettile(c), texes);
                int rgb = 0;
                if (tex != null)
                    rgb = tex.getRGB(Utils.floormod(c.x, tex.getWidth()),
                            Utils.floormod(c.y, tex.getHeight()));
                if (rgb == 0xFF000000)
                    blackpxs++;
                buf.setRGB(c.x, c.y, rgb);
            }
        }
        
        if (blackpxs >= 9500) // if 95% black
            return null;

        for (c.y = 1; c.y < sz.y - 1; c.y++) {
            for (c.x = 1; c.x < sz.x - 1; c.x++) {
                int t = g.gettile(c);
                Tiler tl = map.tiler(t);
                if (tl instanceof Ridges.RidgeTile) {
                    if (Ridges.brokenp(map, c, g)) {
                        for (int y = c.y - 1; y <= c.y + 1; y++) {
                            for (int x = c.x - 1; x <= c.x + 1; x++) {
                                Color cc = new Color(buf.getRGB(x, y));
                                buf.setRGB(x, y, Utils.blendcol(cc, Color.BLACK, ((x == c.x) && (y == c.y)) ? 1 : 0.1).getRGB());
                            }
                        }
                    }
                }
            }
        }

        for (c.y = 0; c.y < sz.y; c.y++) {
            for (c.x = 0; c.x < sz.x; c.x++) {
                try {
                    int t = g.gettile(c);
                    Coord r = c.add(g.ul);
                    if ((map.gettile(r.add(-1, 0)) > t) ||
                            (map.gettile(r.add(1, 0)) > t) ||
                            (map.gettile(r.add(0, -1)) > t) ||
                            (map.gettile(r.add(0, 1)) > t)) {
                        buf.setRGB(c.x, c.y, Color.BLACK.getRGB());
                    }
                } catch (Exception e) {
                }
            }
        }
        return (buf);
    }
}
