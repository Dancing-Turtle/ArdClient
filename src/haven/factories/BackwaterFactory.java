package haven.factories;

import java.awt.image.BufferedImage;

import haven.ItemInfo;
import haven.Resource;
import haven.RichText;
import haven.Text;

public class BackwaterFactory implements ItemInfo.InfoFactory {
    public BackwaterFactory() {
    }

    public ItemInfo build(ItemInfo.Owner var1, Object... var2) {
        double var3 = ((Number) var2[1]).doubleValue();
        return new Tip(var1, var3);
    }

    public class Tip extends ItemInfo.Tip {
        Text tip;

        public Tip(Owner var2, double var3) {
            super(var2);
            String str = Resource.getLocString(Resource.BUNDLE_LABEL, "Growth speed of onion and lettuce: +%d%%\nQuality of squirrels, moles and badgers: +%d%%");
            this.tip = RichText.render(String.format(str, Math.round(var3 * 100.0D), Math.round(var3 * 100.0D)), 0);
        }

        public BufferedImage tipimg() {
            return this.tip.img;
        }
    }
}