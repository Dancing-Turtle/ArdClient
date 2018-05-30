package haven.res.lib.tspec;

import java.util.List;
import java.util.Random;

import haven.GSprite;
import haven.GSprite.Owner;
import haven.Glob;
import haven.ItemInfo;
import haven.ItemInfo.Name;
import haven.ItemInfo.SpriteOwner;
import haven.OwnerContext;
import haven.ResData;
import haven.Resource;
import haven.Session;
import haven.Tex;
import haven.TexI;
import haven.UI;
import haven.res.ui.tt.defn.DefName;

public class Spec implements Owner, SpriteOwner {
    private static final Object[] definfo = new Object[]{new Object[]{new DefName()}};
    public final Object[] info;
    public final ResData res;
    public final OwnerContext ctx;
    public static final ClassResolver<UI> uictx = new ClassResolver<UI>()
            .add(Glob.class, var0 -> var0.sess.glob)
            .add(Session.class, var0 -> var0.sess);
    private Random rnd = null;
    private GSprite spr = null;
    private List<ItemInfo> cinfo = null;

    public Spec(ResData var1, OwnerContext var2, Object[] var3) {
        this.res = var1;
        this.ctx = var2;
        this.info = var3 == null ? definfo : var3;
    }

    public static OwnerContext uictx(UI var0) {
        return new Spec$1(var0);
    }

    public <T> T context(Class<T> var1) {
        return this.ctx.context(var1);
    }

    /**
     * @deprecated
     */
    @Deprecated
    public Glob glob() {
        return (Glob) this.context(Glob.class);
    }

    public Resource getres() {
        return (Resource) this.res.res.get();
    }

    public Random mkrandoom() {
        if (this.rnd == null) {
            this.rnd = new Random();
        }

        return this.rnd;
    }

    public GSprite sprite() {
        return this.spr;
    }

    public Resource resource() {
        return (Resource) this.res.res.get();
    }

    public GSprite spr() {
        if (this.spr == null) {
            this.spr = GSprite.create(this, (Resource) this.res.res.get(), this.res.sdt.clone());
        }

        return this.spr;
    }

    public List<ItemInfo> info() {
        if (this.cinfo == null) {
            this.cinfo = ItemInfo.buildinfo(this, this.info);
        }

        return this.cinfo;
    }

    public Tex longtip() {
        return new TexI(ItemInfo.longtip(this.info()));
    }

    public String name() {
        GSprite var1 = this.spr();
        Name var2 = (Name) ItemInfo.find(Name.class, this.info());
        return var2 == null ? null : var2.str.text;
    }

    final static class Spec$1 implements OwnerContext {
        private UI ui;

        Spec$1(UI var1) {
            this.ui = var1;
        }

        public <C> C context(Class<C> var1) {
            return Spec.uictx.context(var1, this.ui);
        }
    }
}
