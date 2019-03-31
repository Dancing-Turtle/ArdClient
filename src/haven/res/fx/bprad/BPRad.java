package haven.res.fx.bprad;

import haven.*;

import javax.media.opengl.GL;
import java.awt.*;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;

public class BPRad extends Sprite {
    static final GLState smat = new States.ColState(new Color(192, 0, 0, 128));
    static final GLState emat = new States.ColState(new Color(255, 224, 96));
	public static final GLState smatBeehive = new States.ColState(new Color(233, 234, 134, 80));
	public static final GLState smatTrough = new States.ColState(new Color(0, 255, 255, 80));
    final VertexBuf.VertexArray posa;
    final VertexBuf.NormalArray nrma;
    final ShortBuffer sidx;
    final ShortBuffer eidx;
    private Coord2d lc;

    public BPRad(Sprite.Owner owner, Resource res, float radius) {
	super(owner, res);
	final int per = Math.max(24, (int)(6.283185307179586D * radius / 11.0D));
	final float height = 10.0f;
	FloatBuffer verts = Utils.mkfbuf(per * 3 * 2);
	FloatBuffer normals = Utils.mkfbuf(per * 3 * 2);
	ShortBuffer indx = Utils.mksbuf(per * 6);
	ShortBuffer ring_indx = Utils.mksbuf(per);
	for (int i = 0; i < per; i++)
	{
	    float s = (float)Math.sin(6.283185307179586D * i / per);
	    float c = (float)Math.cos(6.283185307179586D * i / per);
	    verts.put(i * 3, c * radius).put(i * 3 + 1, s * radius).put(i * 3 + 2, height);
	    verts.put((per + i) * 3, c * radius).put((per + i) * 3 + 1, s * radius).put((per + i) * 3 + 2, 0);
	    normals.put(i * 3, c).put(i * 3 + 1, s).put(i * 3 + 2, 0.0F);
	    normals.put((per + i) * 3, c).put((per + i) * 3 + 1, s).put((per + i) * 3 + 2, 0.0F);
	    int v = i * 6;
	    indx.put(v, (short)i).put(v + 1, (short)(i + per)).put(v + 2, (short)((i + 1) % per));
	    indx.put(v + 3, (short)(i + per)).put(v + 4, (short)((i + 1) % per + per)).put(v + 5, (short)((i + 1) % per));
	    ring_indx.put(i, (short)i);
	}

	this.posa = new VertexBuf.VertexArray(verts);
	this.nrma = new VertexBuf.NormalArray(normals);
	this.sidx = indx;
	this.eidx = ring_indx;
    }

    public BPRad(Sprite.Owner owner, Resource res, Message msg) {
	this(owner, res, Utils.hfdec((short)msg.int16()) * 11.0F);
    }

    public boolean setup(RenderList rl) {
	rl.prepo(Rendered.eyesort);
	rl.prepo(Material.nofacecull);
	rl.state().put(States.color, null);
	return true;
    }

    public void draw(GOut g) {
	g.state(smat);
	g.apply();

	this.posa.bind(g, true);
	this.nrma.bind(g, true);
	this.sidx.rewind();
	g.gl.glDrawElements(4, this.sidx.capacity(), GL.GL_UNSIGNED_SHORT, this.sidx);

	g.state(emat);
	g.apply();
	this.eidx.rewind();
	g.gl.glLineWidth(3.0F);
	g.gl.glDrawElements(2, this.eidx.capacity(), GL.GL_UNSIGNED_SHORT, this.eidx);

	this.posa.unbind(g);
	this.nrma.unbind(g);
    }
}
