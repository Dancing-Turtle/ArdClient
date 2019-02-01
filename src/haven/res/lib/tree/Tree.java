package haven.res.lib.tree;

import haven.*;
import haven.MCache.Grid;
import java.util.Random;

public class Tree extends StaticSprite {
    private final Location scale;
    private final Location rot;
    public final float fscale;


    public Tree(Owner var1, Resource var2, float var3, Message var4) {
        super(var1, var2, var4);
        if (var1 instanceof Gob) {
            this.rot = rndrot(randoom((Gob)var1));
        } else {
            this.rot = null;
        }

        this.fscale = var3;

        if (Config.bonsai && var3 > 0.6)
            this.scale = mkscale(0.6f);
        else if (var3 == 1.0F)
            this.scale = null;
        else
            this.scale = mkscale(var3);
    }

    private static Message invert(Message var0) {
        int var1 = 0;

        int var2;
        for(var2 = 0; !var0.eom(); var2 += 8) {
            var1 |= var0.uint8() << var2;
        }

        var2 = -1 & ~var1;
        MessageBuf var3 = new MessageBuf();
        var3.addint32(var2);
        return new MessageBuf(var3.fin());
    }

    public Tree(Owner var1, Resource var2, Message var3) {
        this(var1, var2, var3.eom() ? 1.0F : (float)var3.uint8() / 100.0F, invert(var3));
    }

    public static Location mkscale(float var0, float var1, float var2) {
        return new Location(new Matrix4f(var0, 0.0F, 0.0F, 0.0F, 0.0F, var1, 0.0F, 0.0F, 0.0F, 0.0F, var2, 0.0F, 0.0F, 0.0F, 0.0F, 1.0F));
    }

    public static Location mkscale(float var0) {
        return mkscale(var0, var0, var0);
    }

    public static Random randoom(Gob var0) {
        Coord var1 = var0.rc.floor(MCache.tilesz);
        Grid var2 = var0.glob.map.getgridt(var1);
        var1 = var1.sub(var2.ul);
        Random var3 = new Random(var2.id);
        var3.setSeed(var3.nextLong() ^ (long)var1.x);
        var3.setSeed(var3.nextLong() ^ (long)var1.y);
        return var3;
    }

    public static Location rndrot(Random var0) {
        double var1 = var0.nextDouble() * 3.141592653589793D * 2.0D;
        double var3 = var0.nextGaussian() * 3.141592653589793D / 64.0D;
        Coord3f var5 = new Coord3f((float)Math.sin(var1), (float)Math.cos(var1), 0.0F);
        return Location.rot(var5, (float)var3);
    }

    public boolean setup(RenderList var1) {
        if (this.rot != null) {
            var1.prepc(this.rot);
        }

        if (this.scale != null) {
            var1.prepc(this.scale);
            var1.prepc(States.normalize);
        }

        return super.setup(var1);
    }
}
