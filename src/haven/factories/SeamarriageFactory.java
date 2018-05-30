package haven.factories;

import java.awt.image.BufferedImage;

import haven.ItemInfo;
import haven.Resource;
import haven.RichText;
import haven.Text;

public class SeamarriageFactory implements ItemInfo.InfoFactory {
    public SeamarriageFactory() {
    }

    public ItemInfo build(ItemInfo.Owner var1, Object... var2) {
        double var3 = ((Number) var2[1]).doubleValue();
        double var5 = ((Number) var2[2]).doubleValue();
        return new Tip(var1, var3, var5);
    }

    public class Tip extends ItemInfo.Tip {
        Text tip;

        public Tip(Owner var2, double var3, double var5) {
            super(var2);
            String str = Resource.getLocString(Resource.BUNDLE_LABEL, "Quality of caught fish: +%d%%\nQuality on built knarrs: +%d%%");
            this.tip = RichText.render(String.format(str, Math.round(var3 * 100.0D), Math.round(var5 * 100.0D)), 0);
        }

        public BufferedImage tipimg() {
            return this.tip.img;
        }
    }
}