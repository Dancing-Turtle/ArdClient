package haven.res.ui.tt;

import java.awt.image.BufferedImage;

import haven.ItemInfo.Tip;
import haven.Resource;
import haven.Text;

public class Armor extends Tip {
    public final int hard;
    public final int soft;

    public Armor(Owner var1, int hard, int soft) {
        super(var1);
        this.hard = hard;
        this.soft = soft;
    }

    public BufferedImage tipimg() {
        return Text.render(String.format(Resource.getLocString(Resource.BUNDLE_LABEL, "Armor class: %,d/%,d"), Integer.valueOf(this.hard), Integer.valueOf(this.soft))).img;
    }
}
