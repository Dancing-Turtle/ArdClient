import java.nio.FloatBuffer;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import haven.BGL;
import haven.Coord3f;
import haven.GLState;
import haven.GOut;
import haven.Gob;
import haven.Gob.Overlay.CDel;
import haven.Location;
import haven.Material;
import haven.Material.Res;
import haven.Message;
import haven.RenderList;
import haven.Resource;
import haven.Skeleton.BoneOffset;
import haven.Sprite;
import haven.States;
import haven.Utils;
import haven.glsl.Cons;
import haven.glsl.Expression;
import haven.glsl.FragmentContext;
import haven.glsl.Function;
import haven.glsl.Macro1;
import haven.glsl.MiscLib;
import haven.glsl.ProgramContext;
import haven.glsl.Return;
import haven.glsl.ShaderMacro;
import haven.glsl.Tex2D;
import haven.glsl.Type;
import haven.glsl.Uniform;
import haven.glsl.Variable;
import haven.glsl.VertexContext;
import haven.res.lib.env.Environ;

public class ISmoke extends Sprite implements CDel {
    FloatBuffer posb = null;
    FloatBuffer colb = null;
    final Material mat;
    final List<Boll> bollar = new LinkedList<>();
    final Random rnd = new Random();
    GLState loc;
    final float sz;
    float den;
    final float fadepow;
    final float initzv;
    float life;
    float srad;
    boolean spawn = true;
    float de = 0.0F;
    private static final Uniform bollsz = new Uniform(Type.FLOAT);
    private static final ShaderMacro prog = new SM1();
    private final GLState projsz;
    private final float r, g, b, a;

    public ISmoke(Owner owner, Resource res, Message sdt) {
        super(owner, res);
        this.projsz = new ISmoke$2(prog);
        this.mat = (res.layer(Res.class, Integer.valueOf(sdt.uint8()))).get();
        this.sz = (float) sdt.uint8() / 10.0F;
        String boffid = sdt.string();
        loc = boffid.equals("") ? null : owner.getres().layer(BoneOffset.class, boffid).forpose(null);
        int clr = sdt.uint16();
        r = (float) (((clr & 0xf000) >> 12) * 17) / 255.0F;
        g = (float) (((clr & 0x0f00) >> 8) * 17) / 255.0F;
        b = (float) (((clr & 0x00f0) >> 4) * 17) / 255.0F;
        a = (float) (((clr & 0x000f) >> 0) * 17) / 255.0F;
        this.den = (float) sdt.uint8();
        this.fadepow = (float) sdt.uint8() / 10.0F;
        this.life = (float) sdt.uint8() / 10.0F;
        this.initzv = (float) sdt.uint8() / this.life;
        this.srad = (float) sdt.uint8() / 10.0F;

        Resource ownres = owner.getres();
        if (ownres.name.endsWith("tarkiln")) {
            if (boffid.equals("s0")) {
                loc = GLState.compose(Location.xlate(new Coord3f(0, 0, 12)));
                srad = 90.0F / 10.F;
                den = 60.0F;
                life = 3;
            } else {
                spawn = false;
            }
        }
    }

    public boolean tick(int dt) {
        float var2 = (float) dt / 1000.0F;
        de += var2;

        while (spawn && (double) de > 0.1D) {
            de = (float) ((double) de - 0.1D);
            int var3 = (int) ((1.0F + rnd.nextFloat() * 0.5F) * den);

            for (int i = 0; i < var3; ++i) {
                float e = 0;
                float a = rnd.nextFloat() * (float) Math.PI * 2.0F;
                float r = (float) Math.sqrt((double) rnd.nextFloat()) * srad;

                float x = (float) Math.cos(a) * (float) Math.cos(e) * r + (float) (rnd.nextGaussian() * 0.3D);
                float y = (float) Math.sin(a) * (float) Math.cos(e) * r + (float) (rnd.nextGaussian() * 0.3D);
                float z = (float) Math.sin(e) * r;
                bollar.add(new Boll(x, y, z));
            }
        }

        Coord3f var6 = Environ.get(((Gob) owner).glob).wind().mul(0.4F).rot(Coord3f.zu, (float) ((Gob) owner).a);

        Iterator it = bollar.iterator();
        while (it.hasNext()) {
            Boll boll = (Boll) it.next();
            if (boll.tick(var2, var6))
                it.remove();
        }

        return !spawn && bollar.isEmpty();
    }

    public void draw(GOut g) {
        updpos(g);

        if (posb != null) {
            g.apply();
            BGL bgl = g.gl;
            bgl.glEnable('衡');
            bgl.glEnableClientState('聴');
            bgl.glVertexPointer(3, 5126, 0, posb);
            bgl.glEnableClientState('聶');
            bgl.glColorPointer(4, 5126, 0, colb);
            bgl.glNormal3f(0.0F, 0.0F, 1.0F);
            bgl.glDrawArrays(0, 0, bollar.size());
            bgl.glDisableClientState('聴');
            bgl.glDisableClientState('聶');
            bgl.glDisable('衡');
        }
    }

    private void updpos(GOut g) {
        if (bollar.size() < 1) {
            posb = colb = null;
        } else {
            if (posb == null || posb.capacity() < bollar.size() * 3) {
                int var2 = 3 * bollar.size() / 2;
                posb = Utils.mkfbuf(var2 * 3);
                colb = Utils.mkfbuf(var2 * 4);
            }

            FloatBuffer var6 = Utils.wfbuf(3 * bollar.size());
            FloatBuffer var7 = Utils.wfbuf(4 * bollar.size());

            Iterator it = bollar.iterator();
            while (it.hasNext()) {
                Boll var9 = (Boll) it.next();
                var6.put(var9.x).put(var9.y).put(var9.z);
                var7.put(r).put(this.g).put(b).put(a * (float) Utils.clip(1.0D - Math.pow((double) (var9.t / life), (double) fadepow), 0.0D, 1.0D));
            }

            g.gl.bglCopyBufferf(posb, 0, var6, 0, var6.capacity());
            g.gl.bglCopyBufferf(colb, 0, var7, 0, var7.capacity());
        }
    }

    public boolean setup(RenderList var1) {
        var1.prepo(mat);
        var1.prepo(States.presdepth);
        if (loc != null)
            var1.prepo(loc);

        var1.prepo(eyesort);
        var1.prepo(States.vertexcolor);
        var1.prepo(projsz);
        var1.state().put(States.fsaa.slot, null);
        return true;
    }

    public void delete() {
        spawn = false;
    }

    final static class SM1 implements ShaderMacro {
        public void modify(ProgramContext var1) {
            var1.vctx.ptsz.mod(new ISmoke$1$1(var1), 0);
            Tex2D.texcoord(var1.fctx).mod(new ISmoke$1$2(), 0);
        }

        final static class ISmoke$1$1 implements Macro1<Expression> {
            final Function pdiv;
            ProgramContext var2;

            ISmoke$1$1(ProgramContext var2) {
                this.var2 = var2;
                this.pdiv = new ISmoke$1$1$1(Type.FLOAT);
            }

            public Expression expand(Expression var1) {
                Expression[] var10000 = new Expression[2];
                Function var10003 = this.pdiv;
                Expression[] var10004 = new Expression[1];
                var10004[0] = VertexContext.projxf(Cons.add(new Expression[]{this.var2.vctx.eyev.depref(), Cons.vec4(new Expression[]{bollsz.ref() /*ISmoke.access$000*/, Cons.l(0.0D), Cons.l(0.0D), Cons.l(0.0D)})}));
                var10000[0] = Cons.sub(var10003.call(var10004), this.pdiv.call(new Expression[]{this.var2.vctx.posv.depref()}));
                var10000[1] = Cons.pick(MiscLib.screensize.ref(), "x");
                return Cons.mul(var10000);
            }

            final static class ISmoke$1$1$1 extends Function.Def {
                ISmoke$1$1$1(Type var2) {
                    super(var2);
                    Variable.Ref var3 = this.param(PDir.IN, Type.VEC4).ref();
                    this.code.add(new Return(Cons.div(Cons.pick(var3, "x"), Cons.pick(var3, "w"))));
                }
            }
        }

        final static class ISmoke$1$2 implements Macro1<Expression> {
            public Expression expand(Expression var1) {
                return FragmentContext.gl_PointCoord.ref();
            }
        }
    }

    class ISmoke$2 extends States.ProgPointSize {
        ISmoke$2(ShaderMacro var2) {
            super(var2);
        }

        public void reapply(GOut var1) {
            var1.gl.glUniform1f(var1.st.prog.uniform(bollsz), sz);
        }

        public void apply(GOut var1) {
            super.apply(var1);
            this.reapply(var1);
        }
    }

    class Boll {
        float x;
        float y;
        float z;
        float xv;
        float yv;
        float zv;
        float t = 0;

        Boll(float x, float y, float z) {
            this.x = x;
            this.y = y;
            this.z = z;
            this.xv = (float) rnd.nextGaussian() * 0.3F;
            this.yv = (float) rnd.nextGaussian() * 0.3F;
            this.zv = initzv;
        }

        public boolean tick(float dt, Coord3f dspl) {
            float var3 = this.xv - dspl.x;
            float var4 = this.yv - dspl.y;
            float var5 = this.zv - dspl.z;
            float var6 = -var3 * 0.2F + (float) rnd.nextGaussian() * 0.5F;
            float var7 = -var4 * 0.2F + (float) rnd.nextGaussian() * 0.5F;
            float var8 = (-var5 + initzv) * 0.2F + (float) rnd.nextGaussian() * 2.0F;
            this.xv += dt * var6;
            this.yv += dt * var7;
            this.zv += dt * var8;
            this.x += this.xv * dt;
            this.y += this.yv * dt;
            this.z += this.zv * dt;
            this.t += dt;
            return t > life;
        }
    }
}
