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

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferByte;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;

import javax.media.opengl.GL;
import javax.media.opengl.GL2;

public class GOut {
    public final static boolean glerror = false;
    public final BGL gl;
    public final GLConfig gc;
    public Coord ul, sz, tx;
    public final CurrentGL curgl;
    private final GOut root;
    public final GLState.Applier st;
    private final GLState.Buffer def2d, cur2d;

    protected GOut(GOut o) {
        this.gl = o.gl;
        this.gc = o.gc;
        this.ul = o.ul;
        this.sz = o.sz;
        this.tx = o.tx;
        this.curgl = o.curgl;
        this.root = o.root;
        this.st = o.st;
        this.def2d = o.def2d;
        this.cur2d = new GLState.Buffer(gc);
        defstate();
    }

    public GOut(BGL gl, CurrentGL curgl, GLConfig cfg, GLState.Applier st, GLState.Buffer def2d, Coord sz) {
        this.gl = gl;
        this.gc = cfg;
        this.ul = this.tx = Coord.z;
        this.sz = sz;
        this.curgl = curgl;
        this.st = st;
        this.root = this;
        this.def2d = def2d;
        this.cur2d = new GLState.Buffer(gc);
        defstate();
    }

    public static class GLException extends RuntimeException {
        public int code;
        public String str;
        private static javax.media.opengl.glu.GLU glu = new javax.media.opengl.glu.GLU();

        public GLException(int code) {
            super("GL Error: " + code + " (" + glu.gluErrorString(code) + ")");
            this.code = code;
            this.str = glu.gluErrorString(code);
        }

        public static String constname(Class<?> cl, int val) {
            String ret = null;
            for (java.lang.reflect.Field f : cl.getFields()) {
                if (((f.getModifiers() & java.lang.reflect.Modifier.STATIC) != 0) &&
                        ((f.getModifiers() & java.lang.reflect.Modifier.PUBLIC) != 0) &&
                        (f.getType() == Integer.TYPE)) {
                    int v;
                    try {
                        v = f.getInt(null);
                    } catch (IllegalAccessException e) {
                        continue;
                    }
                    if (v == val) {
                        if (ret == null)
                            ret = f.getName();
                        else
                            ret = ret + " or " + f.getName();
                    }
                }
            }
            if (ret == null)
                return (Integer.toString(val));
            return (ret);
        }

        public static String constname(int val) {
            return (constname(GL2.class, val));
        }
    }

    public static class GLInvalidEnumException extends GLException {
        public GLInvalidEnumException() {
            super(GL.GL_INVALID_ENUM);
        }
    }

    public static class GLInvalidValueException extends GLException {
        public GLInvalidValueException() {
            super(GL.GL_INVALID_VALUE);
        }
    }

    public static class GLInvalidOperationException extends GLException {
        public GLInvalidOperationException() {
            super(GL.GL_INVALID_OPERATION);
        }
    }

    public static class GLOutOfMemoryException extends GLException {
        public GLOutOfMemoryException() {
            super(GL.GL_OUT_OF_MEMORY);
        }
    }

    public static GLException glexcfor(int code) {
        switch (code) {
            case GL.GL_INVALID_ENUM:
                return (new GLInvalidEnumException());
            case GL.GL_INVALID_VALUE:
                return (new GLInvalidValueException());
            case GL.GL_INVALID_OPERATION:
                return (new GLInvalidOperationException());
            case GL.GL_OUT_OF_MEMORY:
                return (new GLOutOfMemoryException());
            default:
                return (new GLException(code));
        }
    }

    public static void checkerr(GL gl) {
        if (glerror) {
            int err = gl.glGetError();
            if (err != 0)
                throw (glexcfor(err));
        }
    }

    public static void checkerr(BGL gl) {
        if (glerror)
            gl.bglCheckErr();
    }

    private void checkerr() {
        checkerr(gl);
    }

    public GOut root() {
        return (root);
    }

    public GLState.Buffer basicstate() {
        return (def2d.copy());
    }

    private void render(Tex t, Coord ul, Coord br, Coord3f tul, Coord3f tbr)  {
        st.prep(t.draw());
        apply();


        gl.glBegin(GL2.GL_QUADS);

        gl.glTexCoord2f(tul.x, tul.y); gl.glVertex3i(ul.x, ul.y, 0);
        gl.glTexCoord2f(tbr.x, tul.y); gl.glVertex3i(br.x, ul.y, 0);
        gl.glTexCoord2f(tbr.x, tbr.y); gl.glVertex3i(br.x, br.y, 0);
        gl.glTexCoord2f(tul.x, tbr.y); gl.glVertex3i(ul.x, br.y, 0);

        gl.glEnd();
        checkerr(gl);
    }

    private void crender(TextureAtlas.Img tex, Coord c, Coord sz, Coord ul, Coord br) {
        final Tex texture = tex.tex();
        if(sz.x == 0 || sz.y == 0)
            return;

        if((c.x >= br.x) || (c.y >= br.y) ||
                (c.x + sz.x <= ul.x) || (c.y + sz.y <= ul.y))
            return;

        Coord
                vul = new Coord(c),		//Upper Left of draw coord
                vbr = new Coord(c.add(sz));	//Bottom right of draw coord
        Coord3f
                tul = new Coord3f(tex.tul),	//Upper Left coord within the texture
                tbr = new Coord3f(tex.tbr),	//Bottom right coord within the texture
                tfsz = tbr.sub(tul);		//The size of this unit within the texture


        if(c.x < ul.x) {
            vul.x = ul.x;
            tul.x = tex.tul.x + (tfsz.x * ((vul.x-c.x)/(float)sz.x));
        }
        if(c.y < ul.y) {
            vul.y = ul.y;
            tul.y = tex.tul.y + (tfsz.y * ((vul.y-c.y)/(float)sz.y));
        }
        if(c.x + sz.x > br.x) {
            vbr.x = br.x;
            tbr.x = tex.tul.x + (tfsz.x * ((vbr.x-c.x)/(float)sz.x));
        }
        if(c.y + sz.y > br.y) {
            vbr.y = br.y;
            tbr.y = tex.tul.y + (tfsz.y * ((vbr.y-c.y)/(float)sz.y));
        }


        render(texture, vul, vbr, tul, tbr);
    }

    public void image(TextureAtlas.Img tex, Coord c, Coord sz) {
        if(tex != null) {
            st.set(cur2d);
            crender(tex, c.add(tx), sz, ul, ul.add(this.sz));
            checkerr();
        }
    }


    public void image(TextureAtlas.Img tex, Coord c) {
        image(tex, c, tex.sz);
    }

    public void image(TextureAtlas.Img tex) {
        image(tex, Coord.z, tex.sz);
    }

    public void aimage(TextureAtlas.Img tex, Coord c, double ax, double ay) {
        image(tex, c.add((int)((double)tex.sz.x * -ax), (int)((double)tex.sz.y * -ay)));
    }

    public void image(BufferedImage img, Coord c) {
        if (img == null)
            return;
        Tex tex = new TexI(img);
        image(tex, c);
        tex.dispose();
    }

    public void image(Resource.Image img, Coord c) {
        if (img == null)
            return;
        image(img.tex(), c.add(img.o));
    }

    /* Draw texture at c, quite simply. */
    public void image(Tex tex, Coord c) {
        if (tex == null)
            return;
        st.set(cur2d);
        tex.crender(this, c.add(tx), ul, sz);
        checkerr();
    }

    public void image(Indir<Tex> tex, Coord c) {
        image(tex.get(), c);
    }

    public void aimage(Tex tex, Coord c, double ax, double ay) {
        Coord sz = tex.sz();
        image(tex, c.add((int) ((double) sz.x * -ax), (int) ((double) sz.y * -ay)));
    }

    /* Draw texture at c, scaling it to sz. */
    public void image(Tex tex, Coord c, Coord sz) {
        if (tex == null)
            return;
        st.set(cur2d);
        tex.crender(this, c.add(tx), ul, this.sz, sz);
        checkerr();
    }

    /* Draw texture at c, clipping everything outside ul to ul + sz. */
    public void image(Tex tex, Coord c, Coord ul, Coord sz) {
        if (tex == null)
            return;
        st.set(cur2d);
        ul = ul.add(this.tx);
        Coord br = ul.add(sz);
        if (ul.x < this.ul.x)
            ul.x = this.ul.x;
        if (ul.y < this.ul.y)
            ul.y = this.ul.y;
        if (br.x > this.ul.x + this.sz.x)
            br.x = this.ul.x + this.sz.x;
        if (br.y > this.ul.y + this.sz.y)
            br.y = this.ul.y + this.sz.y;
        tex.crender(this, c.add(this.tx), ul, br.sub(ul));
        checkerr();
    }

    public void rimagev(Tex tex, Coord c, int h) {
        Coord cc = new Coord(c);
        Coord sz = new Coord(tex.sz().x, h);
        for (; cc.y < c.y + h; cc.y += tex.sz().y)
            image(tex, cc, c, sz);
    }

    public void rimageh(Tex tex, Coord c, int w) {
        Coord cc = new Coord(c);
        Coord sz = new Coord(w, tex.sz().y);
        for (; cc.x < c.x + w; cc.x += tex.sz().x)
            image(tex, cc, c, sz);
    }

    public void rimage(Tex tex, Coord c, Coord sz) {
        Coord cc = new Coord();
        for (cc.y = c.y; cc.y < c.y + sz.y; cc.y += tex.sz().y) {
            for (cc.x = c.x; cc.x < c.x + sz.x; cc.x += tex.sz().x)
                image(tex, cc, c, sz);
        }
    }

    /* Draw texture at c, with the extra state s applied. */
    public void image(Tex tex, Coord c, GLState s) {
        st.set(cur2d);
        if (s != null)
            state(s);
        tex.crender(this, c.add(tx), ul, sz);
        checkerr();
    }

    public void vertex(Coord c) {
        gl.glVertex2i(c.x + tx.x, c.y + tx.y);
    }

    public void vertex(float x, float y) {
        gl.glVertex2f(x + tx.x, y + tx.y);
    }

    public void apply() {
        st.apply(this);
    }

    public void state(GLState st) {
        this.st.prep(st);
    }

    public void state2d() {
        st.set(cur2d);
    }

    public void line(Coord c1, Coord c2, double w) {
        st.set(cur2d);
        apply();
        gl.glLineWidth((float) w);
        gl.glBegin(GL.GL_LINES);
        vertex(c1);
        vertex(c2);
        gl.glEnd();
        checkerr();
    }

    //Alternative to line, uses GL_POINTS, surprisingly better than lines tbh
    public void dottedline(Coord c1, Coord c2, float w) {
        final float m = (float)(c2.y - c1.y) / (c2.x - c1.x);
        if(Float.isFinite(m) && m != 0) {
            final float b = c2.y - m * c2.x;
            float x = Math.max(c1.x < c2.x ? c1.x : c2.x, 0);
            float y;
            float end = Math.min(c1.x < c2.x ? c2.x : c1.x, sz.x);
            float step = Math.min(1f, (end-x)/Math.abs((m*x+b) - (m*end+b)));


            st.set(cur2d);
            apply();
            gl.glPointSize(w);
            gl.glBegin(GL.GL_POINTS);
            for (; x <= end; x += step) {
                y = m * x + b;
                if (y >= 0 && y <= sz.y) {
                    vertex(x, y);
                }
            }
            gl.glEnd();
            checkerr();
        } else if(m == 0) {
            //Horizontal
            if(c1.y >= 0 && c1.y <= sz.y) {
                float x = Math.max(Math.min(c1.x, c2.x), 0);
                if (x < sz.x) {
                    float mx = Math.min(Math.max(c1.x, c2.x), sz.x);

                    st.set(cur2d);
                    apply();
                    gl.glPointSize(w);
                    gl.glBegin(GL.GL_POINTS);
                    for (; x <= mx; x += 0.25) {
                        vertex(x, c1.y);
                    }
                    gl.glEnd();
                    checkerr();
                }
            }
        } else {
            //Vertical
            if(c1.x >= 0 && c1.x <= sz.x) {
                float y = Math.max(Math.min(c1.y, c2.y), 0);
                if (y < sz.x) {
                    float my = Math.min(Math.max(c1.y, c2.y), sz.y);

                    st.set(cur2d);
                    apply();
                    gl.glPointSize(w);
                    gl.glBegin(GL.GL_POINTS);
                    for (; y <= my; y += 0.25) {
                        vertex(c1.x, y);
                    }
                    gl.glEnd();
                    checkerr();
                }
            }
        }
    }

    public void text(String text, Coord c) {
        atext(text, c, 0, 0);
    }

    public void atext(String text, Coord c, double ax, double ay) {
        Text t = Text.render(text);
        Tex T = t.tex();
        Coord sz = t.sz();
        image(T, c.add((int) ((double) sz.x * -ax), (int) ((double) sz.y * -ay)));
        T.dispose();
        checkerr();
    }

    public void atext(String text, Coord c, double ax, double ay, Text.Foundry foundry) {
        Text t = Text.render(text, Color.WHITE, foundry);
        Tex T = t.tex();
        Coord sz = t.sz();
        image(T, c.add((int) ((double) sz.x * -ax), (int) ((double) sz.y * -ay)));
        T.dispose();
        checkerr();
    }

    public void atextstroked(String text, Coord c, double ax, double ay, Color color, Color stroke, Text.Foundry foundry) {
        Text t = Text.renderstroked(text, color, stroke, foundry);
        Tex T = t.tex();
        Coord sz = t.sz();
        image(T, c.add((int) ((double) sz.x * -ax), (int) ((double) sz.y * -ay)));
        T.dispose();
        checkerr();
    }

    public void poly(Coord... c) {
        if (TexGL.disableall)
            return;
        st.set(cur2d);
        apply();
        gl.glBegin(GL2.GL_POLYGON);
        for (Coord vc : c)
            vertex(vc);
        gl.glEnd();
        checkerr();
    }

    public void poly2(Object... c) {
        st.set(cur2d);
        st.put(States.color, States.vertexcolor);
        apply();
        gl.glBegin(GL2.GL_POLYGON);
        for (int i = 0; i < c.length; i += 2) {
            Coord vc = (Coord) c[i];
            Color col = (Color) c[i + 1];
            gl.glColor4f((col.getRed() / 255.0f), (col.getGreen() / 255.0f), (col.getBlue() / 255.0f), (col.getAlpha() / 255.0f));
            vertex(vc);
        }
        gl.glEnd();
        checkerr();
    }

    public void polyline(float w, Coord... c) {
        if (TexGL.disableall)
            return;
        st.set(cur2d);
        apply();
        gl.glLineWidth(w);
        gl.glBegin(GL2.GL_LINE_LOOP);
        for (Coord vc : c)
            gl.glVertex2i(vc.x + tx.x, vc.y + tx.y);
        gl.glEnd();
        checkerr();
    }

    public void frect(Coord ul, Coord sz) {
        if (TexGL.disableall)
            return;
        ul = tx.add(ul);
        Coord br = ul.add(sz);
        if (ul.x < this.ul.x) ul.x = this.ul.x;
        if (ul.y < this.ul.y) ul.y = this.ul.y;
        if (br.x > this.ul.x + this.sz.x) br.x = this.ul.x + this.sz.x;
        if (br.y > this.ul.y + this.sz.y) br.y = this.ul.y + this.sz.y;
        if ((ul.x >= br.x) || (ul.y >= br.y))
            return;
        st.set(cur2d);
        apply();
        gl.glBegin(GL2.GL_QUADS);
        gl.glVertex2i(ul.x, ul.y);
        gl.glVertex2i(br.x, ul.y);
        gl.glVertex2i(br.x, br.y);
        gl.glVertex2i(ul.x, br.y);
        gl.glEnd();
        checkerr();
    }

    public void frect(Coord c1, Coord c2, Coord c3, Coord c4) {
        st.set(cur2d);
        apply();
        gl.glBegin(GL2.GL_QUADS);
        vertex(c1);
        vertex(c2);
        vertex(c3);
        vertex(c4);
        gl.glEnd();
        checkerr();
    }

    public void ftexrect(Coord ul, Coord sz, GLState s, float tl, float tt, float tr, float tb) {
        ul = tx.add(ul);
        Coord br = ul.add(sz);
        Coord ult = new Coord(0, 0);
        Coord brt = new Coord(sz);
        if (ul.x < this.ul.x) {
            ult.x += this.ul.x - ul.x;
            ul.x = this.ul.x;
        }
        if (ul.y < this.ul.y) {
            ult.y += this.ul.y - ul.y;
            ul.y = this.ul.y;
        }
        if (br.x > this.ul.x + this.sz.x) {
            brt.x -= br.x - (this.ul.x + this.sz.x);
            br.x = this.ul.x + this.sz.x;
        }
        if (br.y > this.ul.y + this.sz.y) {
            brt.y -= br.y - (this.ul.y + this.sz.y);
            br.y = this.ul.y + this.sz.y;
        }
        if ((ul.x >= br.x) || (ul.y >= br.y))
            return;

        st.set(cur2d);
        state(s);
        apply();

        float l = tl + ((tr - tl) * ((float) ult.x) / ((float) sz.x));
        float t = tt + ((tb - tt) * ((float) ult.y) / ((float) sz.y));
        float r = tl + ((tr - tl) * ((float) brt.x) / ((float) sz.x));
        float b = tt + ((tb - tt) * ((float) brt.y) / ((float) sz.y));
        gl.glBegin(GL2.GL_QUADS);
        gl.glTexCoord2f(l, 1 - t); gl.glVertex2i(ul.x, ul.y);
        gl.glTexCoord2f(r, 1 - t); gl.glVertex2i(br.x, ul.y);
        gl.glTexCoord2f(r, 1 - b); gl.glVertex2i(br.x, br.y);
        gl.glTexCoord2f(l, 1 - b); gl.glVertex2i(ul.x, br.y);
        gl.glEnd();
        checkerr();
    }

    public void ftexrect(Coord ul, Coord sz, GLState s) {
        ftexrect(ul, sz, s, 0, 0, 1, 1);
    }

    // NOTE: this is terribly slow and should not be used in critical methods.
    public void fellipse(Coord c, Coord r, double a1, double a2) {
        st.set(cur2d);
        apply();
        gl.glBegin(GL.GL_TRIANGLE_FAN);
        vertex(c);
        double d = 0.1;
        int i = 0;
        double a = a1;
        while(true) {
            vertex(c.add((int)Math.round(Math.cos(a) * r.x), -(int)Math.round(Math.sin(a) * r.y)));
            if(a >= a2)
                break;
            a = Math.min(a + d, a2);
        }
        gl.glEnd();
        checkerr();
    }

    public void fellipse(Coord c, Coord r) {
        fellipse(c, r, 0, 360);
    }

    public void fcircle(int x, int y, double rad, final int points) {
        if (TexGL.disableall)
            return;
        st.set(cur2d);
        apply();

        int circumference = points - 1;

        float cx = x + tx.x;
        float cy = y + tx.y;

        int vi = 0;
        float vertices[] = new float[points * 2];

        vertices[vi++] = cx;
        vertices[vi++] = cy;

        for (int i = 0; i < circumference; i++) {
            float percent = (i / (float) (circumference - 1));
            float radians = (float) (percent * 2 * Math.PI);
            vertices[vi++] = (float) (cx + rad * Math.cos(radians));
            vertices[vi++] = (float) (cy + rad * Math.sin(radians));
        }

        FloatBuffer vbuf = Utils.mkfbuf(vertices.length);
        vbuf.put(vertices);
        vbuf.position(0);

        gl.glVertexPointer(2, GL.GL_FLOAT, 0, vbuf);
        gl.glEnableClientState(GL2.GL_VERTEX_ARRAY);
        gl.glDrawArrays(GL2.GL_TRIANGLE_FAN, 0, points);
        gl.glDisableClientState(GL2.GL_VERTEX_ARRAY);
    }

    public void rect(Coord ul, Coord sz) {
        st.set(cur2d);
        apply();
        gl.glLineWidth(1);
        gl.glBegin(GL.GL_LINE_LOOP);
        vertex(ul.x + 0.5f, ul.y + 0.5f);
        vertex(ul.x + sz.x - 0.5f, ul.y + 0.5f);
        vertex(ul.x + sz.x - 0.5f, ul.y + sz.y - 0.5f);
        vertex(ul.x + 0.5f, ul.y + sz.y - 0.5f);
        gl.glEnd();
        checkerr();
    }

    public void prect(Coord c, Coord ul, Coord br, double a) {
        st.set(cur2d);
        apply();
        gl.glEnable(GL2.GL_POLYGON_SMOOTH);
        gl.glBegin(GL.GL_TRIANGLE_FAN);
        vertex(c);
        vertex(c.add(0, ul.y));
        double p2 = Math.PI / 2;
        all:
        {
            float tc;

            tc = (float) (Math.tan(a) * -ul.y);
            if ((a > p2) || (tc > br.x)) {
                vertex(c.x + br.x, c.y + ul.y);
            } else {
                vertex(c.x + tc, c.y + ul.y);
                break all;
            }

            tc = (float) (Math.tan(a - (Math.PI / 2)) * br.x);
            if ((a > p2 * 2) || (tc > br.y)) {
                vertex(c.x + br.x, c.y + br.y);
            } else {
                vertex(c.x + br.x, c.y + tc);
                break all;
            }

            tc = (float) (-Math.tan(a - Math.PI) * br.y);
            if ((a > p2 * 3) || (tc < ul.x)) {
                vertex(c.x + ul.x, c.y + br.y);
            } else {
                vertex(c.x + tc, c.y + br.y);
                break all;
            }

            tc = (float) (-Math.tan(a - (3 * Math.PI / 2)) * -ul.x);
            if ((a > p2 * 4) || (tc < ul.y)) {
                vertex(c.x + ul.x, c.y + ul.y);
            } else {
                vertex(c.x + ul.x, c.y + tc);
                break all;
            }

            tc = (float) (Math.tan(a) * -ul.y);
            vertex(c.x + tc, c.y + ul.y);
        }
        gl.glEnd();
        gl.glDisable(GL2.GL_POLYGON_SMOOTH);
        checkerr();
    }

    public <T extends GLState> T curstate(GLState.Slot<T> slot) {
        return (cur2d.get(slot));
    }

    public void usestate(GLState st) {
        st.prep(cur2d);
    }

    public <T extends GLState> void usestate(GLState.Slot<? super T> slot) {
        cur2d.put(slot, null);
    }

    public void defstate() {
        def2d.copy(cur2d);
    }

    public void chcolor(Color c) {
        usestate(new States.ColState(c));
    }

    public void chcolor(int r, int g, int b, int a) {
        chcolor(Utils.clipcol(r, g, b, a));
    }

    public void chcolor() {
        usestate(States.color);
    }

    Color getcolor() {
        States.ColState color = curstate(States.color);
        return ((color == null) ? Color.WHITE : color.c);
    }

    public GOut reclip(Coord ul, Coord sz) {
        GOut g = new GOut(this);
        g.tx = this.tx.add(ul);
        g.ul = new Coord(g.tx);
        Coord gbr = g.ul.add(sz), tbr = this.ul.add(this.sz);
        if (g.ul.x < this.ul.x)
            g.ul.x = this.ul.x;
        if (g.ul.y < this.ul.y)
            g.ul.y = this.ul.y;
        if (gbr.x > tbr.x)
            gbr.x = tbr.x;
        if (gbr.y > tbr.y)
            gbr.y = tbr.y;
        g.sz = gbr.sub(g.ul);
        return (g);
    }

    public GOut reclipl(Coord ul, Coord sz) {
        GOut g = new GOut(this);
        g.tx = this.tx.add(ul);
        g.ul = new Coord(g.tx);
        g.sz = sz;
        return (g);
    }

    public void getpixel(final Coord c, final Callback<Color> cb) {
        gl.bglSubmit(new BGL.Request() {
            public void run(GL2 gl) {
                byte[] buf = new byte[4];
                gl.glReadPixels(c.x + tx.x, root.sz.y - c.y - tx.y, 1, 1, GL.GL_RGBA, GL2.GL_UNSIGNED_BYTE, ByteBuffer.wrap(buf));
                Color result = new Color(((int)buf[0]) & 0xff, ((int)buf[1]) & 0xff, ((int)buf[2]) & 0xff, ((int)buf[3]) & 0xff);
                checkerr(gl);
                cb.done(result);
            }
        });
    }

    public void getimage(final Coord ul, final Coord sz, final Callback<BufferedImage> cb) {
        gl.bglSubmit(new BGL.Request() {
            public void run(GL2 gl) {
                byte[] buf = new byte[sz.x * sz.y * 4];
                gl.glReadPixels(ul.x + tx.x, root.sz.y - ul.y - sz.y - tx.y, sz.x, sz.y, GL.GL_RGBA, GL2.GL_UNSIGNED_BYTE, ByteBuffer.wrap(buf));
                checkerr(gl);
                for (int y = 0; y < sz.y / 2; y++) {
                    int to = y * sz.x * 4, bo = (sz.y - y - 1) * sz.x * 4;
                    for (int o = 0; o < sz.x * 4; o++, to++, bo++) {
                        byte t = buf[to];
                        buf[to] = buf[bo];
                        buf[bo] = t;
                    }
                }
                WritableRaster raster = Raster.createInterleavedRaster(new DataBufferByte(buf, buf.length), sz.x, sz.y, 4 * sz.x, 4, new int[]{0, 1, 2, 3}, null);
                cb.done(new BufferedImage(TexI.glcm, raster, false, null));
            }
        });
    }

    public void getimage(final Callback<BufferedImage> cb) {
        getimage(Coord.z, sz, cb);
    }
}
