import haven.CharWnd;
import haven.Coord;
import haven.GItem;
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
import haven.purus.pbot.PBotAPI;
import haven.res.lib.tspec.Spec;
import haven.res.ui.tt.defn.DefName;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;


public class ISlots extends Tip implements NumberInfo {
    public static final Text ch = Text.render(Resource.getLocString(Resource.BUNDLE_LABEL, "Gilding:"));
    public final Collection<SItem> s = new ArrayList<SItem>();
    public final int left;
    public final double pmin;
    public final double pmax;
    public final Resource[] attrs;
    public static final String chc = "192,192,255";
    public static final Object[] defn = new Object[]{new DefName()};

    public ISlots(Owner owner, int left, double pmin, double pmax, Resource[] attrs) {
        super(owner);
        this.left = left;
        this.pmin = pmin;
        this.pmax = pmax;
        this.attrs = attrs;
    }

    public void layout(Layout layout) {
        layout.cmp.add(ch.img, new Coord(0, layout.cmp.sz.y));
        BufferedImage chanceImg;
        if (attrs.length > 0) {
            String chanceStr = Resource.getLocString(Resource.BUNDLE_LABEL, "Chance: $col[%s]{%d%%} to $col[%s]{%d%%}");
            chanceImg = RichText.render(String.format(chanceStr,
                    chc,
                    Long.valueOf(Math.round(100.0D * pmin)),
                    chc,
                    Long.valueOf(Math.round(100.0D * pmax))),
                    0,
                    new Object[0]).img;
            int szy = chanceImg.getHeight();
            byte cx = 10;
            int cy = layout.cmp.sz.y;
            layout.cmp.add(chanceImg, new Coord(cx, cy));
            int iconX = cx + chanceImg.getWidth() + 10;

            for (int i = 0; i < attrs.length; ++i) {
                BufferedImage iconImg = PUtils.convolvedown((attrs[i].layer(Resource.imgc)).img, new Coord(szy, szy), CharWnd.iconfilter);
                layout.cmp.add(iconImg, new Coord(iconX, cy));
                iconX += iconImg.getWidth() + 2;
            }
        } else {
            String chanceStr = Resource.getLocString(Resource.BUNDLE_LABEL, "Chance: $col[%s]{%d%%}");
            chanceImg = RichText.render(String.format(chanceStr,
                    chc, Integer.valueOf((int) Math.round(100.0D * pmin))),
                    0,
                    new Object[0]).img;
            layout.cmp.add(chanceImg, new Coord(10, layout.cmp.sz.y));
        }

        Iterator sItemIterator = s.iterator();

        while (sItemIterator.hasNext()) {
            SItem sItem = (SItem) sItemIterator.next();
            sItem.layout(layout);
        }

        if (left > 0) {
            String gildStr = Resource.getLocString(Resource.BUNDLE_LABEL, "Gildable ×%d");
            String gild2Str = Resource.getLocString(Resource.BUNDLE_LABEL, "Gildable");
            layout.cmp.add(Text.slotFnd.render(left > 1 ? String.format(gildStr, Integer.valueOf(left)) : gild2Str).img, new Coord(10, layout.cmp.sz.y));
        }

        BufferedImage totalString = RichText.render(Resource.getLocString(Resource.BUNDLE_LABEL, "Total:")).img;
        layout.cmp.add(totalString, new Coord(0, layout.cmp.sz.y));

        Map<Resource, Integer> totalAttrs = new HashMap<>();
        GItem[] gItems = new GItem[]{(GItem) owner};
        totalAttrs = Arrays.stream(gItems)
                .map(GItem::info)
                .map(ItemInfo::getBonuses)
                .map(Map::entrySet)
                .flatMap(Collection::stream)
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, Integer::sum));
        for (Resource r : totalAttrs.keySet()) {
            System.out.println(r + " " + totalAttrs.get(r));
        }
        List<ItemInfo> info = null;
        if (totalAttrs != null) {
            ItemInfo compiled = make(totalAttrs.entrySet().stream().sorted(this::BY_PRIORITY).collect(Collectors.toList()));
            info = compiled != null ? Collections.singletonList(compiled) : null;
        }

        BufferedImage tip = null;

        if (info != null && !info.isEmpty()) {
            tip = ItemInfo.longtip(info);
        } else {
            tip = null;
        }

        layout.cmp.add(tip, new Coord(10, layout.cmp.sz.y));

    }

    private ItemInfo make(Collection<Map.Entry<Resource, Integer>> mods) {
        if (mods.isEmpty()) {
            return null;
        }
        Resource res = Resource.remote().load("ui/tt/attrmod").get();
        ItemInfo.InfoFactory f = res.layer(Resource.CodeEntry.class).get(ItemInfo.InfoFactory.class);
        Object[] args = new Object[mods.size() * 2 + 1];
        int i = 1;
        for (Map.Entry<Resource, Integer> entry : mods) {
            args[i] = PBotAPI.ui().sess.getresid(entry.getKey());
            args[i + 1] = entry.getValue();
            i += 2;
        }
        return f.build(owner, args);
    }

    private int BY_PRIORITY(Map.Entry<Resource, Integer> o1, Map.Entry<Resource, Integer> o2) {
        Resource r1 = o1.getKey();
        Resource r2 = o2.getKey();

        if (PBotAPI.ui().gui.chrwdg != null) {
            return PBotAPI.ui().gui.chrwdg.BY_PRIORITY(r1, r2);
        }
        return r1.name.compareTo(r2.name);
    }

    public int order() {
        return 200;
    }

    public int itemnum() {
        return s.size();
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

        public SItem(ISlots iSlots, ResData resData, Object[] args) {
            this.islots = iSlots;
            this.res = resData.res.get();
            Spec spriteSpec = new Spec(resData, iSlots.owner, Utils.extend(new Object[]{ISlots.defn}, args));
            this.spr = spriteSpec.spr();
            this.name = spriteSpec.name();
            Spec infoSpec = new Spec(resData, iSlots.owner, args);
            this.info = infoSpec.info();
        }

        private BufferedImage img() {
            return spr instanceof ImageSprite ? ((ImageSprite) spr).image() : (res.layer(Resource.imgc)).img;
        }


        public Glob glob() {
            return islots.owner.glob();
        }

        public List<ItemInfo> info() {
            return info;
        }

        public Resource resource() {
            return res;
        }

        public void layout(Layout layout) {
            BufferedImage iconImg = PUtils.convolvedown(img(), new Coord(16, 16), CharWnd.iconfilter);
            BufferedImage nameImg = Text.render(name).img;
            BufferedImage tipImg = ItemInfo.longtip(info);
            byte cx = 10;
            int cy = layout.cmp.sz.y;
            layout.cmp.add(iconImg, new Coord(cx, cy));
            layout.cmp.add(nameImg, new Coord(cx + 16 + 3, cy + (16 - nameImg.getHeight()) / 2));
            if (tipImg != null) {
                layout.cmp.add(tipImg, new Coord(cx + 16, cy + 16));
            }
        }
    }
}
