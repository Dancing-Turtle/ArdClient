
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import haven.CharWnd;
import haven.Coord;
import haven.GItem.NumberInfo;
import haven.GSprite;
import haven.GSprite.ImageSprite;
import haven.Glob;
import haven.ItemInfo;
import haven.ItemInfo.Tip;
import haven.PUtils;
import haven.ResData;
import haven.Resource;
import haven.Resource.Image;
import haven.RichText;
import haven.Text;
import haven.Utils;
import haven.res.lib.tspec.Spec;
import haven.res.ui.tt.defn.DefName;


public class ISlots extends Tip implements NumberInfo {
    public static final Text ch = Text.render(Resource.getLocString(Resource.BUNDLE_LABEL, "Gilding:"));
    public final Collection<SItem> s = new ArrayList<SItem>();
    public final int left;
    public final double pmin;
    public final double pmax;
    public final Resource[] attrs;
    public static final String chc = "192,192,255";
    public static final Object[] defn = new Object[]{new DefName()};

    public ISlots(Owner var1, int var2, double var3, double var5, Resource[] var7) {
        super(var1);
        this.left = var2;
        this.pmin = var3;
        this.pmax = var5;
        this.attrs = var7;
    }

    public void layout(Layout var1) {
        var1.cmp.add(ch.img, new Coord(0, var1.cmp.sz.y));
        BufferedImage var2;
        if (this.attrs.length > 0) {
            String chanceStr = Resource.getLocString(Resource.BUNDLE_LABEL, "Chance: $col[%s]{%d%%} to $col[%s]{%d%%}");
            var2 = RichText.render(String.format(chanceStr,
                    chc,
                    Long.valueOf(Math.round(100.0D * this.pmin)),
                    chc,
                    Long.valueOf(Math.round(100.0D * this.pmax))),
                    0,
                    new Object[0]).img;
            int var3 = var2.getHeight();
            byte var4 = 10;
            int var5 = var1.cmp.sz.y;
            var1.cmp.add(var2, new Coord(var4, var5));
            int var10 = var4 + var2.getWidth() + 10;

            for (int var6 = 0; var6 < this.attrs.length; ++var6) {
                BufferedImage var7 = PUtils.convolvedown((this.attrs[var6].layer(Resource.imgc)).img, new Coord(var3, var3), CharWnd.iconfilter);
                var1.cmp.add(var7, new Coord(var10, var5));
                var10 += var7.getWidth() + 2;
            }
        } else {
            String chanceStr = Resource.getLocString(Resource.BUNDLE_LABEL, "Chance: $col[%s]{%d%%}");
            var2 = RichText.render(String.format(chanceStr,
                    chc, Integer.valueOf((int) Math.round(100.0D * this.pmin))),
                    0,
                    new Object[0]).img;
            var1.cmp.add(var2, new Coord(10, var1.cmp.sz.y));
        }

        Iterator var8 = this.s.iterator();

        while (var8.hasNext()) {
            SItem var9 = (SItem) var8.next();
            var9.layout(var1);
        }

        if (this.left > 0) {
            String gildStr = Resource.getLocString(Resource.BUNDLE_LABEL, "Gildable ×%d");
            String gild2Str = Resource.getLocString(Resource.BUNDLE_LABEL, "Gildable");
            var1.cmp.add(Text.slotFnd.render(this.left > 1 ? String.format(gildStr, Integer.valueOf(this.left)) : gild2Str).img, new Coord(10, var1.cmp.sz.y));
        }

    }

    public int order() {
        return 200;
    }

    public int itemnum() {
        return this.s.size();
    }

    public Color numcolor() {
        return left > 0 ? new Color(0, 169, 224) : Color.WHITE;
    }

    public static class SItem {
        private final ISlots islots;
        public final Resource res;
        public final GSprite spr;
        public final List<ItemInfo> info;
        public final String name;

        public SItem(ISlots var1, ResData var2, Object[] var3) {
            this.islots = var1;
            this.res = (Resource)var2.res.get();
            Spec var4 = new Spec(var2, var1.owner, Utils.extend(new Object[]{ISlots.defn}, var3));
            this.spr = var4.spr();
            this.name = var4.name();
            Spec var5 = new Spec(var2, var1.owner, var3);
            this.info = var5.info();
        }

        private BufferedImage img() {
            return this.spr instanceof ImageSprite ? ((ImageSprite)this.spr).image() : ((Image)this.res.layer(Resource.imgc)).img;
        }


        public Glob glob() {
            return this.islots.owner.glob();
        }

        public List<ItemInfo> info() {
            return this.info;
        }

        public Resource resource() {
            return this.res;
        }

        public void layout(Layout var1) {
            BufferedImage var2 = PUtils.convolvedown(this.img(), new Coord(16, 16), CharWnd.iconfilter);
            BufferedImage var3 = Text.render(this.name).img;
            BufferedImage var4 = ItemInfo.longtip(this.info);
            byte var5 = 10;
            int var6 = var1.cmp.sz.y;
            var1.cmp.add(var2, new Coord(var5, var6));
            var1.cmp.add(var3, new Coord(var5 + 16 + 3, var6 + (16 - var3.getHeight()) / 2));
            var1.cmp.add(var4, new Coord(var5 + 16, var6 + 16));
        }
    }
}
