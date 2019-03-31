package haven.sloth.gfx;

import haven.*;
import haven.VertexBuf.NormalArray;
import haven.VertexBuf.VertexArray;
import haven.DefSettings;

import java.nio.ByteBuffer;
import java.nio.FloatBuffer;

public class GobPathSprite extends Sprite {
    private final GLState smat;
    private final VertexArray posa;
    private final NormalArray nrma;
    private final ByteBuffer sidx;
    public final Coord2d dest, rc;

    /**
     * @param dest Destination coordinate, for knowing if we changed
     * @param rc Current coordinate, for knowing if we changed
     * @param x The length of the path
     * @param endz the z position at the end of the path
     * @param col The color of this sprite
     */
    public GobPathSprite(final Coord2d dest, final Coord2d rc,
                         final float x, final float endz, final GLState col) {
	super(null, null);
	this.dest = dest;
	this.rc = rc;
	this.smat = col;
	final int faces = 3;
	final FloatBuffer pa = Utils.mkfbuf(4 * 3 * faces);
	final FloatBuffer na = Utils.mkfbuf(4 * 3 * faces);
	final ByteBuffer sa = Utils.mkbbuf(6 * faces);

	//Height of our path relative to gob z position, 11f is roughly hearthling eye height
	final float h = 11f;
	//The size of our path triangles is 0.5f, just enough to make it stand out from left, right, and top
	final float delta = 0.5f;
	//Since this Sprite is drawn relative to the gob's position AND rotation we only care about x really
	//This sprite is essentially drawn on the default plane and rotated/transformed with the gob
	//Therefore, y is a1lways 1f defining the width of our top face, delta defines the width of the side faces
	final float y = 1f;
	final float z = !DefSettings.FLATWORLD.get() ? endz + h: 0;
	final float dh = h - delta, dz = z - delta;

	pa.put(0).put(0).put(h);	na.put(0).put(0).put(0f);
	pa.put(0).put(y).put(h);	na.put(0).put(y).put(0f);
	pa.put(x).put(y).put(z);	na.put(x).put(y).put(0f);
	pa.put(x).put(0).put(z);	na.put(x).put(0).put(0f);
	sa.put((byte) 0).put((byte) 1).put((byte) 2);
	sa.put((byte) 0).put((byte) 2).put((byte) 3);

	//left face
	pa.put(0).put(0).put(h);	na.put(0).put(0).put(0f);
	pa.put(0).put(0).put(dh);	na.put(0).put(y).put(0f);
	pa.put(x).put(0).put(z);	na.put(x).put(y).put(0f);
	pa.put(x).put(0).put(dz);	na.put(x).put(0).put(0f);
	sa.put((byte) 4).put((byte) 5).put((byte) 6);
	sa.put((byte) 4).put((byte) 6).put((byte) 7);

	//right face
	pa.put(0).put(y).put(dh);	na.put(0).put(y).put(0f);
	pa.put(0).put(y).put(h);	na.put(0).put(y).put(0f);
	pa.put(x).put(y).put(z);	na.put(x).put(y).put(0f);
	pa.put(x).put(y).put(dz);	na.put(x).put(y).put(0f);
	sa.put((byte) 8).put((byte) 9).put((byte) 10);
	sa.put((byte) 8).put((byte) 10).put((byte) 11);

	this.posa = new VertexArray(pa);
	this.nrma = new NormalArray(na);
	this.sidx = sa;
    }

    public boolean setup(RenderList rl) {
	rl.prepo(Rendered.last);
	//rl.prepo(Rendered.eyesort);
	rl.prepo(Material.nofacecull);
	//Location.goback(rl.state(), "gobx");
	rl.state().put(States.color, null);
	return true;
    }

    public void draw(GOut g) {
	if (smat != null)
	    g.state(smat);
	g.apply();
	this.posa.bind(g, false);
	this.nrma.bind(g, false);
	this.sidx.rewind();
	g.gl.glDrawElements(g.curgl.gl.GL_TRIANGLES, this.sidx.capacity(),
		g.curgl.gl.GL_UNSIGNED_BYTE, this.sidx);
	this.posa.unbind(g);
	this.nrma.unbind(g);
    }
}
