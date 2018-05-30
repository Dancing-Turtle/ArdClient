package haven.res.lib.tree;

import haven.Config;
import haven.Location;
import haven.Matrix4f;
import haven.Message;
import haven.RenderList;
import haven.Resource;
import haven.States;
import haven.StaticSprite;

public class Tree extends StaticSprite {
    private final Location scale;
    public final float fscale;
    Message sdt;

    public Tree(Owner owner, Resource res, float scale) {
        super(owner, res, Message.nil);
        this.fscale = scale;

        if (Config.bonsai && scale > 0.6)
            this.scale = mkscale(0.6f);
        else if (scale == 1.0F)
            this.scale = null;
        else
            this.scale = mkscale(scale);
    }

    public Tree(Owner owner, Resource res, Message std) {
        this(owner, res, std.eom() ? 1.0F : (float) std.uint8() / 100.0F);
        this.sdt = std;
    }

    public static Location mkscale(float var0, float var1, float var2) {
        return new Location(new Matrix4f(var0, 0.0F, 0.0F, 0.0F, 0.0F, var1, 0.0F, 0.0F, 0.0F, 0.0F, var2, 0.0F, 0.0F, 0.0F, 0.0F, 1.0F));
    }

    public static Location mkscale(float scale) {
        return mkscale(scale, scale, scale);
    }

    public boolean setup(RenderList var1) {
        if (this.scale != null) {
            var1.prepc(this.scale);
            var1.prepc(States.normalize);
        }

        return super.setup(var1);
    }
}
