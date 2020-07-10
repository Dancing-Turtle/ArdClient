package haven.res.ui.tt.q.qbuff;

import haven.*;
import haven.purus.pbot.PBotUtils;

import java.awt.*;
import java.awt.image.BufferedImage;

import static haven.Text.num10Fnd;
import static haven.Text.num12boldFnd;


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
        Color color, outline = Color.BLACK;
        if(Config.qualitycolor){
            if(q < 26){
                color = Config.uncommon;
            } else if(q > 25 && q < 101){
                color = Config.rare;
            } else if(q > 100 && q < 250){
                color = Config.epic;
            } else if(q > 249 && q < 400){
                color = Config.legendary;
            } else {
                if(Config.insaneitem){
                    PBotUtils.sysMsg("What a nice item!");
                }
                color = Color.orange;
                outline = Color.RED;
            }
        } else {
            color = Color.white;
        }
        if (q != 0) {
            if(!Config.largeqfont) {
                qtex = Text.renderstroked(Utils.fmt1DecPlace(q), color, outline, num10Fnd).tex();
                qwtex = Text.renderstroked(Math.round(q) + "", color, outline, num10Fnd).tex();
            }else{
                qtex = Text.renderstroked(Utils.fmt1DecPlace(q), color, outline, num12boldFnd).tex();
                qwtex = Text.renderstroked(Math.round(q) + "", color, outline, num12boldFnd).tex();
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
