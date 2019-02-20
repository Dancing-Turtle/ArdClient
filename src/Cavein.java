import haven.*;

import java.awt.Color;
import java.util.Iterator;
import java.nio.Buffer;
import java.util.LinkedList;
import java.nio.FloatBuffer;
import java.util.Random;
import java.util.List;

//
// Decompiled by Procyon v0.5.30
//

public class Cavein extends Sprite implements Gob.Overlay.CDel
{
    static final GLState mat;
    List<Boll> bollar;
    Random rnd;
    boolean spawn;
    FloatBuffer posb;
    FloatBuffer nrmb;
    float de;
    float str;
    float life;
    Coord3f off;
    Coord sz;

    public Cavein(final Sprite.Owner owner, final Resource resource, final Message message) {
        super(owner, resource);
        this.bollar = new LinkedList<Boll>();
        this.rnd = new Random();
        this.spawn = true;
        this.posb = null;
        this.nrmb = null;
        this.de = 0.0f;
        this.str = message.uint8();
        this.sz = new Coord(message.uint8(), message.uint8());
        this.off = new Coord3f(-this.sz.x / 2.0f, -this.sz.y / 2.0f, (float)message.uint8());
        this.life = 60 * Config.caveinduration;//make dust last however many minutes selected in display settings
               // message.uint8(); default value
    }

    public void draw(final GOut gOut) {
        this.updpos(gOut);
        if (this.posb == null) {
            return;
        }
        gOut.apply();
        final BGL gl = gOut.gl;
        this.posb.rewind();
        this.nrmb.rewind();
        gl.glPointSize(1.1f);
        gl.glEnableClientState(32884);
        gl.glVertexPointer(3, 5126, 0, (Buffer)this.posb);
        gl.glEnableClientState(32885);
        gl.glNormalPointer(5126, 0, (Buffer)this.nrmb);
        gl.glDrawArrays(0, 0, this.bollar.size());
        gl.glDisableClientState(32884);
        gl.glDisableClientState(32885);
    }

    void updpos(final GOut gOut) {
        if (this.bollar.size() < 1) {
            this.posb = null;
            this.nrmb = null;
            return;
        }
        if (this.posb == null || this.posb.capacity() < this.bollar.size() * 3) {
            final int n = (this.posb == null) ? 512 : (this.posb.capacity() / 3);
            this.posb = Utils.mkfbuf(n * 2 * 3);
            this.nrmb = Utils.mkfbuf(n * 2 * 3);
        }
        final FloatBuffer wfbuf = Utils.wfbuf(3 * this.bollar.size());
        final FloatBuffer wfbuf2 = Utils.wfbuf(3 * this.bollar.size());
        for (final Boll boll : this.bollar) {
            wfbuf.put(boll.p.x).put(boll.p.y).put(boll.p.z);
            wfbuf2.put(boll.n.x).put(boll.n.y).put(boll.n.z);
        }
        gOut.gl.bglCopyBufferf(this.posb, 0, wfbuf, 0, wfbuf.capacity());
        gOut.gl.bglCopyBufferf(this.nrmb, 0, wfbuf2, 0, wfbuf.capacity());
    }

    public boolean tick(final int n) {
        final float n2 = n / 1000.0f;
        this.de += n2 * this.str;
        if (this.spawn && this.de > 1.0f) {
            --this.de;
            this.bollar.add(new Boll(this.off.add(this.rnd.nextFloat() * this.sz.x, this.rnd.nextFloat() * this.sz.y, 0.0f), 0.5f + this.rnd.nextFloat() * 1.5f));
        }
        final Iterator<Boll> iterator = this.bollar.iterator();
        while (iterator.hasNext()) {
            if (iterator.next().tick(n2)) {
                iterator.remove();
            }
        }
        if (this.life > 0.0f) {
            final float life = this.life - n2;
            this.life = life;
            if (life <= 0.0f) {
                this.spawn = false;
            }
        }
        return !this.spawn && this.bollar.isEmpty();
    }

    public boolean setup(final RenderList list) {
        list.prepo(Light.deflight);
        list.prepo(Cavein.mat);
        return true;
    }

    public void delete() {
        this.spawn = false;
    }

    static {
      if(!Config.colorfulcaveins)
       mat = (GLState)new Material.Colors(new Color(255, 255, 255), new Color(255, 255, 255), new Color(128, 128, 128), new Color(0, 0, 0), 1.0f);
      else {
          Random rnd = new Random();
          mat = new haven.Material.Colors(new Color(rnd.nextInt(256), rnd.nextInt(256), rnd.nextInt(256)),
                  new Color(rnd.nextInt(256), rnd.nextInt(256), rnd.nextInt(256)),
                  new Color(rnd.nextInt(256), rnd.nextInt(256), rnd.nextInt(256)),
                  new Color(0, 0, 0), 1.0F);
      }
    }

    class Boll
    {
        Coord3f p;
        Coord3f v;
        Coord3f n;
        float sz;
        float t;

        Boll(final Coord3f coord3f, final float sz) {
            this.p = new Coord3f(coord3f.x, coord3f.y, coord3f.z);
            this.v = new Coord3f(0.0f, 0.0f, 0.0f);
            this.n = new Coord3f(Cavein.this.rnd.nextFloat() - 0.5f, Cavein.this.rnd.nextFloat() - 0.5f, Cavein.this.rnd.nextFloat() - 0.5f).norm();
            this.sz = sz;
            this.t = -1.0f;
        }

        boolean tick(final float n) {
            final Coord3f v = this.v;
            v.z -= n;
            this.v.z = Math.min(0.0f, this.v.z + n * 5.0f * this.v.z * this.v.z / this.sz);
            final Coord3f v2 = this.v;
            v2.x += n * (float)Cavein.this.rnd.nextGaussian() * 0.1f;
            final Coord3f v3 = this.v;
            v3.y += n * (float)Cavein.this.rnd.nextGaussian() * 0.1f;
            final Coord3f p = this.p;
            p.x += this.v.x;
            final Coord3f p2 = this.p;
            p2.y += this.v.y;
            final Coord3f p3 = this.p;
            p3.z += this.v.z;
            if (this.p.z < 0.0f) {
                this.p.z = 0.0f;
                final Coord3f v4 = this.v;
                v4.z *= -0.7f;
                this.v.x = this.v.z * (Cavein.this.rnd.nextFloat() - 0.5f);
                this.v.y = this.v.z * (Cavein.this.rnd.nextFloat() - 0.5f);
                if (this.t < 0.0f) {
                    this.t = 0.0f;
                }
            }
            if (this.t >= 0.0f) {
                this.t += n;
            }
            return this.t > 1.5f;
        }
    }
}
