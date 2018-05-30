import java.awt.image.BufferedImage;

import haven.ItemInfo.Tip;
import haven.Resource;
import haven.RichText;
import haven.Utils;

public class Gast extends Tip {
    public final double glut;
    public final double fev;

    public Gast(Owner var1, double var2, double var4) {
        super(var1);
        this.glut = var2;
        this.fev = var4;
    }

    public BufferedImage tipimg() {
        StringBuilder var1 = new StringBuilder();
        if(this.glut != 1.0D) {
            var1.append(String.format(Resource.getLocString(Resource.BUNDLE_LABEL, "Hunger reduction: %s%%\n"), new Object[]{Utils.odformat2(100.0D * this.glut, 1)}));
        }

        if(this.fev != 1.0D) {
            var1.append(String.format(Resource.getLocString(Resource.BUNDLE_LABEL, "Food event bonus: %s%%\n"), new Object[]{Utils.odformat2(100.0D * this.fev, 1)}));
        }

        return RichText.render(var1.toString(), 0, new Object[0]).img;
    }
}