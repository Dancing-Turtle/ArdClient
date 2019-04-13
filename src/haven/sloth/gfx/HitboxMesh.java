package haven.sloth.gfx;

import haven.*;
import haven.DefSettings;

import java.nio.FloatBuffer;
import java.nio.ShortBuffer;
import java.util.concurrent.ConcurrentHashMap;

//TODO:: The four corners should match the heightmap
public class HitboxMesh extends FastMesh {
    private static States.ColState hiddencolor = new States.ColState(DefSettings.HIDDENCOLOR.get());
    //Lots of hidden sprites will be identical, rather than each gob have there own we'll share
    //sprites that have the same sizes
    private static ConcurrentHashMap<String, HitboxMesh> hbs = new ConcurrentHashMap<>();

    private HitboxMesh(VertexBuf buf, ShortBuffer sa) {
	super(buf, sa);
    }

    public boolean setup(RenderList rl) {
	rl.prepo(Material.nofacecull);
	rl.prepo(MapMesh.postmap);
	rl.prepo(States.vertexcolor);
	return super.setup(rl);
    }

    public synchronized static void updateColor(final States.ColState col) {
        hiddencolor = col;
        hbs.forEach((name, mesh) -> mesh.dispose());
        hbs.clear();
    }

    public synchronized static HitboxMesh makehb(Coord rec, Coord off) {
	final String key = rec+","+off;
	HitboxMesh hb = hbs.get(key);
	if(hb != null)
	    return hb;
	else {
            rec = rec.add(off).sub(1, 1);

	    FloatBuffer pa = Utils.mkfbuf(8 * 3);
	    FloatBuffer na = Utils.mkfbuf(8 * 3);
	    FloatBuffer cl = Utils.mkfbuf(8 * 4);
	    ShortBuffer sa = Utils.mksbuf(6 * 5);

	    float h = 2f;
	    float
		    rx = off.x, ry = off.y,
		    lx = rec.x, ly = rec.y;
	    //Top verts
	    pa.put(lx).put(ly).put(h); na.put(lx).put(ly).put(0f);	//0
	    pa.put(lx).put(ry).put(h); na.put(lx).put(ry).put(0f);	//1
	    pa.put(rx).put(ry).put(h); na.put(rx).put(ry).put(0f);	//2
	    pa.put(rx).put(ly).put(h); na.put(rx).put(ly).put(0f);	//3
	    //bottom verts
	    pa.put(lx).put(ly).put(0f); na.put(lx).put(ly).put(0f);	//4 under 0
	    pa.put(lx).put(ry).put(0f); na.put(lx).put(ry).put(0f);	//5 under 1
	    pa.put(rx).put(ry).put(0f); na.put(rx).put(ry).put(0f);	//6 under 2
	    pa.put(rx).put(ly).put(0f); na.put(rx).put(ly).put(0f);	//7 under 3

	    //Each vert is given the same color
	    cl.put(hiddencolor.ca[0]).put(hiddencolor.ca[1]).put(hiddencolor.ca[2]).put(hiddencolor.ca[3]);
	    cl.put(hiddencolor.ca[0]).put(hiddencolor.ca[1]).put(hiddencolor.ca[2]).put(hiddencolor.ca[3]);
	    cl.put(hiddencolor.ca[0]).put(hiddencolor.ca[1]).put(hiddencolor.ca[2]).put(hiddencolor.ca[3]);
	    cl.put(hiddencolor.ca[0]).put(hiddencolor.ca[1]).put(hiddencolor.ca[2]).put(hiddencolor.ca[3]);
	    cl.put(hiddencolor.ca[0]).put(hiddencolor.ca[1]).put(hiddencolor.ca[2]).put(hiddencolor.ca[3]);
	    cl.put(hiddencolor.ca[0]).put(hiddencolor.ca[1]).put(hiddencolor.ca[2]).put(hiddencolor.ca[3]);
	    cl.put(hiddencolor.ca[0]).put(hiddencolor.ca[1]).put(hiddencolor.ca[2]).put(hiddencolor.ca[3]);
	    cl.put(hiddencolor.ca[0]).put(hiddencolor.ca[1]).put(hiddencolor.ca[2]).put(hiddencolor.ca[3]);

	    //Top
	    sa.put((short)0).put((short)1).put((short)2);
	    sa.put((short)0).put((short)2).put((short)3);
	    //left 0-1 4-5
	    sa.put((short)0).put((short)4).put((short)1);
	    sa.put((short)1).put((short)5).put((short)4);
	    //right 2-3 6-7
	    sa.put((short)2).put((short)6).put((short)3);
	    sa.put((short)3).put((short)7).put((short)6);
	    //front 1-2 5-6
	    sa.put((short)1).put((short)5).put((short)2);
	    sa.put((short)2).put((short)6).put((short)5);
	    //back 0-3 4-7
	    sa.put((short)0).put((short)4).put((short)3);
	    sa.put((short)3).put((short)7).put((short)4);

	    hb = new HitboxMesh(new VertexBuf(new VertexBuf.VertexArray(pa),
		    new VertexBuf.NormalArray(na),
		    new VertexBuf.ColorArray(cl)),
		sa);
	    hbs.put(key, hb);
	    return hb;
	}
    }
}
