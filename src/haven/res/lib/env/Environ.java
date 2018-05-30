
package haven.res.lib.env;

import java.util.Random;

import haven.Coord3f;
import haven.Glob;
import haven.res.lib.globfx.GlobData;
import haven.res.lib.globfx.GlobEffector;

public class Environ extends GlobData {
    private final Random rnd = new Random();
    float wdir;
    float wvel;
    Coord3f gust;
    Coord3f wind;

    public Environ() {
        this.wdir = this.rnd.nextFloat() * 3.1415927F * 2.0F;
        this.wvel = this.rnd.nextFloat() * 3.0F;
        this.gust = new Coord3f(0.0F, 0.0F, 0.0F);
        this.wind = Coord3f.o;
    }

    private void wind(float var1) {
        Coord3f var2 = Coord3f.o.sadd(0.0F, this.wdir, this.wvel);
        this.wdir = (float)((double)this.wdir + (double)this.rnd.nextFloat() * 0.005D);
        if(this.wdir < 0.0F) {
            this.wdir += 6.2831855F;
        }

        if((double)this.wdir > 6.283185307179586D) {
            this.wdir -= 6.2831855F;
        }

        this.wvel = (float)((double)this.wvel + (double)this.rnd.nextFloat() * 0.005D);
        if(this.wvel < 0.0F) {
            this.wvel = 0.0F;
        }

        if(this.wvel > 20.0F) {
            this.wvel = 20.0F;
        }

        if(this.rnd.nextInt(2000) == 0) {
            this.gust.x = this.rnd.nextFloat() * 200.0F - 100.0F;
            this.gust.y = this.rnd.nextFloat() * 200.0F - 100.0F;
        }

        float var3 = (float)Math.pow(0.2D, (double)var1);
        this.gust.x *= var3;
        this.gust.y *= var3;
        this.gust.z *= var3;
        this.wind = var2.add(this.gust);
    }

    public Coord3f wind() {
        return this.wind;
    }

    public boolean tick(float var1) {
        this.wind(var1);
        return false;
    }

    public static Environ get(Glob var0) {
        return GlobEffector.getdata(var0, new Environ());
    }
}
