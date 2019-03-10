package haven.res.ui.tt.q.qbuff;

import static haven.Text.num10Fnd;
import static haven.Text.num12boldFnd;



import java.awt.Color;
import java.awt.image.BufferedImage;

import haven.*;


public class QBuff extends ItemInfo.Tip {
    public final BufferedImage icon;
    public final String name;
    public final String origName;
    public final double q;
    public static final Layout.ID<Table> lid = new Tid();
    public static final Layout.ID<Summary> sid = new Sid();
    public Tex qtex, qwtex;

    public QBuff(Owner owner, BufferedImage icon, String name, double q) {
        super(owner);
        this.icon = icon;
        this.origName = name;
        this.name = Resource.getLocString(Resource.BUNDLE_LABEL, name);
        this.q = q;
        if (q != 0) {
            if(!Config.largeqfont) {
                qtex = Text.renderstroked(Utils.fmt1DecPlace(q), Color.WHITE, Color.BLACK, num10Fnd).tex();
                qwtex = Text.renderstroked(Math.round(q) + "", Color.WHITE, Color.BLACK, num10Fnd).tex();
            }else{
                qtex = Text.renderstroked(Utils.fmt1DecPlace(q), Color.WHITE, Color.BLACK, num12boldFnd).tex();
                qwtex = Text.renderstroked(Math.round(q) + "", Color.WHITE, Color.BLACK, num12boldFnd).tex();
            }
        }
    }

    public void prepare(Layout layout) {
        layout.intern(lid).ql.add(this);
    }

    public Tip shortvar() {
        return new ShortTip(this, this.owner);
    }
}
