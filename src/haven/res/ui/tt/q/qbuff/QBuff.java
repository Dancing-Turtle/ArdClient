package haven.res.ui.tt.q.qbuff;

import haven.*;

import java.awt.*;
import java.awt.image.BufferedImage;

import static haven.Text.num10Fnd;
import static haven.Text.num12boldFnd;


public class QBuff extends ItemInfo.Tip {
    public final BufferedImage icon;
    public final String name;
    public final String origName;
    public double q;
    public static final Layout.ID<Table> lid = new Tid();
    public static final Layout.ID<Summary> sid = new Sid();
    public Tex qtex, qwtex;
    public Color color, outline = Color.BLACK;
    public QBuff(Owner owner, BufferedImage icon, String name, double q) {
        super(owner);
        this.icon = icon;
        this.origName = name;
        this.name = Resource.getLocString(Resource.BUNDLE_LABEL, name);
        this.q = q;
        if(Config.qualitycolor){
            if(q < 11){
                color = Color.white;
            } else if(q < Config.uncommonq){
                color = Config.uncommon;
            } else if(q < Config.rareq){
                color = Config.rare;
            } else if(q < Config.epicq){
                color = Config.epic;
            } else if(q < Config.legendaryq){
                color = Config.legendary;
            } else {
                if(Config.insaneitem){
                    //PBotUtils.sysMsg("What a nice item!");
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
