import java.awt.image.BufferedImage;
import java.util.List;

import haven.CharWnd;
import haven.Coord;
import haven.ItemInfo;
import haven.ItemInfo.Tip;
import haven.PUtils;
import haven.Resource;
import haven.Resource.Image;
import haven.RichText;
import haven.Text;
import haven.Text.Line;

public class Slotted extends Tip {
    public static final Line ch = Text.render(Resource.getLocString(Resource.BUNDLE_LABEL, "As gilding:"));
    public final double pmin;
    public final double pmax;
    public final Resource[] attrs;
    public final List<ItemInfo> sub;
    public static final String chc = "192,192,255";

    public Slotted(Owner var1, double var2, double var4, Resource[] var6, List<ItemInfo> var7) {
        super(var1);
        this.pmin = var2;
        this.pmax = var4;
        this.attrs = var6;
        this.sub = var7;
    }

    public void layout(Layout var1) {
        var1.cmp.add(ch.img, new Coord(0, var1.cmp.sz.y));
        BufferedImage var2;
        if (this.attrs.length > 0) {
            String chanceStr = Resource.getLocString(Resource.BUNDLE_LABEL, "Chance: $col[%s]{%d%%} to $col[%s]{%d%%}");
            var2 = RichText.render(String.format(chanceStr, "192,192,255", Long.valueOf(Math.round(100.0D * this.pmin)), "192,192,255", Long.valueOf(Math.round(100.0D * this.pmax))), 0, new Object[0]).img;
            int var3 = var2.getHeight();
            byte var4 = 10;
            int var5 = var1.cmp.sz.y;
            var1.cmp.add(var2, new Coord(var4, var5));
            int var8 = var4 + var2.getWidth() + 10;

            for (int var6 = 0; var6 < this.attrs.length; ++var6) {
                BufferedImage var7 = PUtils.convolvedown(((Image) this.attrs[var6].layer(Resource.imgc)).img, new Coord(var3, var3), CharWnd.iconfilter);
                var1.cmp.add(var7, new Coord(var8, var5));
                var8 += var7.getWidth() + 2;
            }
        } else {
            String chanceStr = Resource.getLocString(Resource.BUNDLE_LABEL, "Chance: $col[%s]{%d%%}");
            var2 = RichText.render(String.format(chanceStr, "192,192,255", Integer.valueOf((int) Math.round(100.0D * this.pmin))), 0, new Object[0]).img;
            var1.cmp.add(var2, new Coord(10, var1.cmp.sz.y));
        }

        var2 = longtip(this.sub);
        if (var2 != null) {
            var1.cmp.add(var2, new Coord(10, var1.cmp.sz.y));
        }

    }

    public int order() {
        return 200;
    }
}