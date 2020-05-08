import haven.*;
import haven.ItemInfo.Tip;
import haven.res.ui.tt.Wear;
import java.awt.image.BufferedImage;

public class Gast extends Tip implements GItem.NumberInfo {
    public final double glut;
    public final double fev;

    public Gast(Owner var1, double var2, double var4) {
        super(var1);
        this.glut = var2;
        this.fev = var4;
    }

    public static ItemInfo mkinfo(Owner var0, Object... var1) {
        return new Gast(var0, ((Number)var1[1]).doubleValue(), ((Number)var1[2]).doubleValue());
    }

    public BufferedImage tipimg() {
        StringBuilder var1 = new StringBuilder();
        if (this.glut != 1.0D) {
            var1.append(String.format(Resource.getLocString(Resource.BUNDLE_LABEL, "Hunger reduction: %s%%\n"), Utils.odformat2(100.0D * this.glut, 1)));
        }

        if (this.fev != 1.0D) {
            var1.append(String.format(Resource.getLocString(Resource.BUNDLE_LABEL, "Food event bonus: %s%%\n"), Utils.odformat2(100.0D * this.fev, 1)));
        }

        return RichText.render(var1.toString(), 0, new Object[0]).img;
    }

    public int itemnum() {
        Wear var1 = (Wear)find(Wear.class, this.owner.info());
        return var1 == null ? 0 : var1.m - var1.d;
    }
}
