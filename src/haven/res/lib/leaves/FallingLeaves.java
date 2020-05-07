package haven.res.lib.leaves;

import haven.*;
import haven.res.lib.env.Environ;
import haven.res.lib.globfx.GlobEffect;
import haven.res.lib.globfx.GlobEffector;

import javax.media.opengl.GL;
import javax.media.opengl.GL2;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

public class FallingLeaves extends GlobEffect {
    static final int maxleaves = 10000;

    public final Random rnd = new Random();
    FloatBuffer posb, nrmb, texb;
    float[] spos, snrm;
    final Leaf leaves[] = new Leaf[maxleaves];
    final Map<Material, MSlot> matmap = new HashMap<Material, MSlot>();
    final Glob glob;
    int nl;
    float ckt = 0;
    boolean copied = false;

    private class MSlot implements Rendered {
	ShortBuffer indb;
	final Material m;
	int nl, indpos;

	MSlot(Material m) {
	    this.m = m;
	    indb = Utils.wsbuf(100 * 4);
	}

	public void draw(GOut g) {
	    if(posb == null)
		return;
	    g.apply();
	    BGL gl = g.gl;
	    if(!copied) {
		gl.bglCopyBufferf(posb, 0, spos, 0, FallingLeaves.this.nl * 4 * 3);
		gl.bglCopyBufferf(nrmb, 0, snrm, 0, FallingLeaves.this.nl * 4 * 3);
		copied = true;
	    }
	    posb.rewind();
	    nrmb.rewind();
	    texb.rewind();
	    indb.rewind();
	    gl.glEnableClientState(GL2.GL_VERTEX_ARRAY);
	    gl.glVertexPointer(3, GL.GL_FLOAT, 0, posb);
	    gl.glEnableClientState(GL2.GL_NORMAL_ARRAY);
	    gl.glNormalPointer(GL.GL_FLOAT, 0, nrmb);
	    gl.glEnableClientState(GL2.GL_TEXTURE_COORD_ARRAY);
	    gl.glTexCoordPointer(2, GL.GL_FLOAT, 0, texb);
	    gl.glDrawElements(GL2.GL_TRIANGLES, nl * 6, GL.GL_UNSIGNED_SHORT, indb);
	    gl.glDisableClientState(GL2.GL_VERTEX_ARRAY);
	    gl.glDisableClientState(GL2.GL_NORMAL_ARRAY);
	    gl.glDisableClientState(GL2.GL_TEXTURE_COORD_ARRAY);
	}

	public boolean setup(RenderList rl) {
	    rl.prepo(m);
	    return(true);
	}

	void rewind() {
	    indpos = 0;
	    nl = 0;
	}

	void add(int vi) {
	    if(vi >= 65536)
		throw(new RuntimeException("More leaf vertices than should be possible."));
	    if(indpos + 6 > indb.capacity()) {
		ShortBuffer nb = Utils.wsbuf(indb.capacity() * 2);
		nb.rewind();
		for(int i = 0; i < indpos; i++)
		    nb.put(i, indb.get(i));
		indb = nb;
	    }
	    indb.put(indpos++, (short)(vi + 0));
	    indb.put(indpos++, (short)(vi + 1));
	    indb.put(indpos++, (short)(vi + 3));
	    indb.put(indpos++, (short)(vi + 1));
	    indb.put(indpos++, (short)(vi + 2));
	    indb.put(indpos++, (short)(vi + 3));
	    nl++;
	}
    }

    public FallingLeaves(Glob glob) {
	this.glob = glob;
    }

    public abstract class Leaf {
	float x, y, z;
	float xv, yv, zv;
	float nx, ny, nz;
	float nxv, nyv, nzv;
	float ar = (0.5f + rnd.nextFloat()) / 50;
	MSlot m;

	public Leaf(float x, float y, float z) {
	    this.x = x; this.y = y; this.z = z;
	    nx = rnd.nextFloat();
	    ny = rnd.nextFloat();
	    nz = rnd.nextFloat();
	    if(nx < 0.5f) nx -= 1.0f;
	    if(ny < 0.5f) ny -= 1.0f;
	    if(nz < 0.5f) nz -= 1.0f;
	    float nf = 1.0f / (float)Math.sqrt((nx * nx) + (ny * ny) + (nz * nz));
	    nx *= nf;
	    ny *= nf;
	    nz *= nf;
	}

	public Leaf() {
	    this(0, 0, 0);
	}

	public Leaf(Coord3f c) {
	    this(c.x, c.y, c.z);
	}

	public abstract Material mat();
	public float size() {return(1);}
    }

    public static FallingLeaves get(Glob glob) {
	return(GlobEffector.get(glob, new FallingLeaves(glob)));
    }

    public void draw(GOut g) {
    }

    public boolean setup(RenderList ls) {
	copied = false;
	for(MSlot mat : matmap.values())
	    ls.add(mat, null);
	return(false);
    }

    void updvert() {
	for(MSlot m : matmap.values())
	    m.rewind();
	int vi = 0, vo = 0;
	for(int i = 0; i < nl; i++, vi += 4, vo += 12) {
	    Leaf l = leaves[i];
	    float sz = l.size();
	    try {
		spos[vo +  0] = l.x + sz * l.nz;
		spos[vo +  1] = l.y - sz * l.nz;
		spos[vo +  2] = l.z + sz * (l.ny - l.nx);
		spos[vo +  3] = l.x + sz * l.nz;
		spos[vo +  4] = l.y + sz * l.nz;
		spos[vo +  5] = l.z - sz * (l.nx - l.ny);
		spos[vo +  6] = l.x - sz * l.nz;
		spos[vo +  7] = l.y + sz * l.nz;
		spos[vo +  8] = l.z + sz * (l.nx - l.ny);
		spos[vo +  9] = l.x - sz * l.nz;
		spos[vo + 10] = l.y - sz * l.ny;
		spos[vo + 11] = l.z + sz * (l.nx + l.ny);

		snrm[vo +  0] = l.nx;
		snrm[vo +  1] = l.ny;
		snrm[vo +  2] = l.nz;
		snrm[vo +  3] = l.nx;
		snrm[vo +  4] = l.ny;
		snrm[vo +  5] = l.nz;
		snrm[vo +  6] = l.nx;
		snrm[vo +  7] = l.ny;
		snrm[vo +  8] = l.nz;
		snrm[vo +  9] = l.nx;
		snrm[vo + 10] = l.ny;
		snrm[vo + 11] = l.nz;
	    } catch(RuntimeException e) {
		throw(new RuntimeException(String.format("%d %d %d", i, vi, vo), e));
	    }

	    l.m.add(vi);
	}
    }

    void move(float dt) {
	Coord3f av = Environ.get(glob).wind();
	for(int i = 0; i < nl; i++) {
	    Leaf l = leaves[i];
	    float xvd = l.xv - av.x, yvd = l.yv - av.y, zvd = l.zv - av.z;
	    float vel = (float)Math.sqrt((xvd * xvd) + (yvd * yvd) + (zvd * zvd));

	    /* Rotate the normal around the normal velocity vector. */
	    float nvl = (float)Math.sqrt((l.nxv * l.nxv) + (l.nyv * l.nyv) + (l.nzv * l.nzv));
	    if(nvl > 0) {
		float s = (float)Math.sin(nvl * dt);
		float c = (float)Math.cos(nvl * dt);
		nvl = 1.0f / nvl;
		float nxvn = l.nxv * nvl, nyvn = l.nyv * nvl, nzvn = l.nzv * nvl;
		float nx = l.nx, ny = l.ny, nz = l.nz;
		l.nx = (nx * (nxvn * nxvn * (1 - c) + c)) + (ny * (nxvn * nyvn * (1 - c) - nzvn * s)) + (nz * (nxvn * nzvn * (1 - c) + nyvn * s));
		l.ny = (nx * (nyvn * nxvn * (1 - c) + nzvn * s)) + (ny * (nyvn * nyvn * (1 - c) + c)) + (nz * (nyvn * nzvn * (1 - c) - nxvn * s));
		l.nz = (nx * (nzvn * nxvn * (1 - c) - nyvn * s)) + (ny * (nzvn * nyvn * (1 - c) + nxvn * s)) + (nz * (nzvn * nzvn * (1 - c) + c));

		float df = (float)Math.pow(0.7, dt);
		l.nxv *= df;
		l.nyv *= df;
		l.nzv *= df;
	    }

	    /* Add the cross-product of the airspeed and the normal to the normal velocity. */
	    float vr = (vel * vel) / 5.0f, ar = 0.5f;
	    float rxvd = xvd + ((rnd.nextFloat() - 0.5f) * vr), ryvd = yvd + ((rnd.nextFloat() - 0.5f) * vr), rzvd = zvd + ((rnd.nextFloat() - 0.5f) * vr);
	    float nxv = l.nxv, nyv = l.nyv, nzv = l.nzv;
	    l.nxv += (l.ny * rzvd - l.nz * ryvd) * dt * ar;
	    l.nyv += (l.nz * rxvd - l.nx * rzvd) * dt * ar;
	    l.nzv += (l.nx * ryvd - l.ny * rxvd) * dt * ar;

	    float ae = Math.abs((l.nx * xvd) + (l.ny * yvd) + (l.nz * zvd));
	    float xa = (l.nx * ae - xvd), ya = (l.ny * ae - yvd), za = (l.nz * ae - zvd);
	    l.xv += xa * Math.abs(xa) * l.ar * dt;
	    l.yv += ya * Math.abs(ya) * l.ar * dt;
	    l.zv += za * Math.abs(za) * l.ar * dt;
	    l.x += l.xv * dt;
	    l.y += l.yv * dt;
	    l.z += l.zv * dt;
	    l.zv -= 9.81f * dt;
	}
    }

    void ckstop(Glob glob) {
	for(int i = 0; i < nl; i++) {
	    boolean drop = false;
	    try {
		drop = leaves[i].z < glob.map.getcz(leaves[i].x, -leaves[i].y) - 1;
	    } catch(Loading e) {
		drop = true;
	    }
	    if(drop) {
		leaves[i--] = leaves[--nl];
		leaves[nl] = null;
	    }
	}
    }

    public boolean tick(float dt) {
	if((ckt += dt) > 10) {
	    ckstop(glob);
	    ckt = 0;
	}
	if(nl == 0)
	    return(true);
	if(posb == null) {
	    posb = Utils.mkfbuf(maxleaves * 4 * 3);
	    spos = new float[maxleaves * 4 * 3];
	    nrmb = Utils.mkfbuf(maxleaves * 4 * 3);
	    snrm = new float[maxleaves * 4 * 3];
	    texb = Utils.mkfbuf(maxleaves * 4 * 2);
	    for(int i = 0; i < maxleaves * 4 * 2; i += 8) {
		texb.put(i + 0, 0.0f);
		texb.put(i + 1, 0.0f);
		texb.put(i + 2, 0.0f);
		texb.put(i + 3, 1.0f);
		texb.put(i + 4, 1.0f);
		texb.put(i + 5, 1.0f);
		texb.put(i + 6, 1.0f);
		texb.put(i + 7, 0.0f);
	    }
	}
	move(dt);
	updvert();
	return(false);
    }

    public Coord3f onevertex(Location.Chain loc, FastMesh m) {
	int vi = m.indb.get(rnd.nextInt(m.num));
	VertexBuf.VertexArray va = m.vert.buf(VertexBuf.VertexArray.class);
	Coord3f vc = new Coord3f(va.data.get(vi * 3),
				 va.data.get(vi * 3 + 1),
				 va.data.get(vi * 3 + 2));
	return(loc.fin(Matrix4f.id).mul4(vc));
    }

    public void addleaf(Leaf leaf) {
	synchronized(leaves) {
	    if(nl >= maxleaves)
		return;
	    leaves[nl] = leaf;
	    Material m = leaf.mat();
	    if((leaf.m = matmap.get(m)) == null)
		matmap.put(m, leaf.m = new MSlot(m));
	    nl++;
	}
    }
}