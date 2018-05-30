/*
 *  This file is part of the Haven & Hearth game client.
 *  Copyright (C) 2009 Fredrik Tolf <fredrik@dolda2000.com>, and
 *                     Björn Johannessen <johannessen.bjorn@gmail.com>
 *
 *  Redistribution and/or modification of this file is subject to the
 *  terms of the GNU Lesser General Public License, version 3, as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  Other parts of this source tree adhere to other copying
 *  rights. Please see the file `COPYING' in the root directory of the
 *  source tree for details.
 *
 *  A copy the GNU Lesser General Public License is distributed along
 *  with the source tree of which this file is a part in the file
 *  `doc/LPGL-3'. If it is missing for any reason, please see the Free
 *  Software Foundation's website at <http://www.fsf.org/>, or write
 *  to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
 */

package haven;

import static haven.glsl.Cons.mul;
import static haven.glsl.Cons.pick;
import static haven.glsl.Cons.textureCube;

import javax.media.opengl.GL;
import javax.media.opengl.GL2;

import haven.glsl.AutoVarying;
import haven.glsl.Expression;
import haven.glsl.Macro1;
import haven.glsl.ProgramContext;
import haven.glsl.ShaderMacro;
import haven.glsl.Type;
import haven.glsl.Uniform;
import haven.glsl.VertexContext;

public class DropSky extends Sprite implements Rendered {
    public final TexCube tex;
    
    private static Matrix4f cam = new Matrix4f();
    private static Matrix4f wxf = new Matrix4f();
    private static Matrix4f mv = new Matrix4f();
    private Location.Chain loc;
    private Camera camp;
    

    public DropSky(TexCube tex) {
    	super(null, null);
    	this.tex = tex;
    }

    private void vertex(GOut g, BGL gl, Matrix4f ixf, float[] oc, float x, float y) {
	float[] cc = {x, y, 0.99999f, 1};
	float[] vc = ixf.mul4(cc);
	float iw = 1 / vc[3];
	for(int i = 0; i < 4; i++) vc[i] *= iw;
	if(g.st.prog == null)
	    gl.glMultiTexCoord3f(GL.GL_TEXTURE0 + tsky.id, vc[0] - oc[0], vc[1] - oc[1], vc[2] - oc[2]);
	else
	    gl.glTexCoord3f(vc[0] - oc[0], vc[1] - oc[1], vc[2] - oc[2]);
	gl.glVertex4f(vc[0], vc[1], vc[2], vc[3]);
    }

    public void draw(GOut g) {
	g.apply();
	BGL gl = g.gl;
    Matrix4f mvxf = PView.mvxf(g);;
	Matrix4f pmvxf = g.st.cur(PView.proj).fin(Matrix4f.id).mul(mvxf);
	Matrix4f ixf = pmvxf.invert();
	float[] oc = mvxf.invert().mul4(new float[] {0, 0, 0, 1});
	float iw = 1 / oc[3];
	for(int i = 0; i < 4; i++) oc[i] *= iw;
	gl.glBegin(GL2.GL_QUADS);
	vertex(g, gl, ixf, oc, -1.05f, -1.05f);
	vertex(g, gl, ixf, oc,  1.05f, -1.05f);
	vertex(g, gl, ixf, oc,  1.05f,  1.05f);
	vertex(g, gl, ixf, oc, -1.05f,  1.05f);
	gl.glEnd();
    }

    private static final Uniform ssky = new Uniform(Type.SAMPLERCUBE);
    private static final ShaderMacro[] shaders = {
	new ShaderMacro() {
	    AutoVarying texcoord = new AutoVarying(Type.VEC3) {
		    protected Expression root(VertexContext vctx) {
			return(pick(vctx.gl_MultiTexCoord[0].ref(), "stp"));
		    }
		};
	    public void modify(ProgramContext prog) {
		prog.fctx.fragcol.mod(new Macro1<Expression>() {
			public Expression expand(Expression in) {
			    return(mul(in, textureCube(ssky.ref(), texcoord.ref())));
			}
		    }, 0);
	    }
	}
    };
    private GLState.TexUnit tsky;
    private final GLState st = new States.AdHoc(shaders) {
	    public boolean reqshaders() {return(false);}

	    public void reapply(GOut g) {
		g.gl.glUniform1i(g.st.prog.uniform(ssky), tsky.id);
	    }

	    private void papply(GOut g) {
		reapply(g);
	    }

	    private void fapply(GOut g) {
	    g.gl.glTexEnvi(GL2.GL_TEXTURE_ENV, GL2.GL_TEXTURE_ENV_MODE, GL2.GL_MODULATE);
		g.gl.glEnable(GL.GL_TEXTURE_CUBE_MAP);
	    }

	    public void apply(GOut g) {
		(tsky = g.st.texalloc()).act(g);
		g.gl.glBindTexture(GL.GL_TEXTURE_CUBE_MAP, tex.glid(g));
		if(!g.st.usedprog)
		    fapply(g);
		else
		    papply(g);
	    }

	    private void funapply(GOut g) {
		g.gl.glDisable(GL.GL_TEXTURE_CUBE_MAP);
	    }

	    public void unapply(GOut g) {
		tsky.act(g);
		if(!g.st.usedprog)
		    funapply(g);
		g.gl.glBindTexture(GL.GL_TEXTURE_CUBE_MAP, null);
		tsky.free(); tsky = null;
	    }
	};

    public boolean setup(RenderList rl) {
		rl.prepo(st);
        GLState.Buffer buf = rl.state();
        loc = buf.get(PView.loc);
        camp = buf.get(PView.cam);
        wxf = PView.locxf(buf);
		return(true);
    }
}