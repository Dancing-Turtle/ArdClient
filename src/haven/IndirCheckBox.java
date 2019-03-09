package haven;


import java.util.function.Consumer;
import haven.IndirSetting;
import haven.Theme;
import haven.TextureAtlas;
/**
 * Only small checkboxes supported
 * 0 = off
 * 1 = on
 */
public class IndirCheckBox extends Widget {
    private static final int spacer = 5;
    private final IndirSetting<Boolean> setting;
    private final Consumer<Boolean> onChange;
    private final Text lbl;

    public IndirCheckBox(final String lbl, final IndirSetting<Boolean> setting, final Consumer<Boolean> onChange) {
	super(Coord.z);
	this.lbl = Text.render(lbl);
	this.onChange = onChange;
	this.setting = setting;

	final Coord boxsz = Theme.timg("chkbox/small", 0).sz;
	resize(new Coord(boxsz.x + spacer + this.lbl.sz().x, Math.max(boxsz.y, this.lbl.sz().y)));
    }

    public IndirCheckBox(final String lbl, final IndirSetting<Boolean> setting) {
        this(lbl, setting, null);
    }

    public boolean state() { return setting.get(); }
    public void set(final boolean val) {
        if(setting.get() != val)
            setting.set(val);
    }

    @Override
    public void draw(GOut g) {
        //Draw checkbox
        final int id = setting.get() ? 1 : 0;
        final TextureAtlas.Img chk = Theme.timg("chkbox/small", id);
        g.image(chk, new Coord(0, sz.y / 2 - chk.sz.y / 2));
        g.image(chk, new Coord(0,sz.y/2 - chk.sz.y /2));


        //Draw label
	g.image(lbl.tex(), new Coord(chk.sz.x + spacer, sz.y /2 - lbl.sz().y / 2 ));
	super.draw(g);
    }

    public boolean mousedown(Coord c, int button) {
        if(button == 1) {
            setting.set(!setting.get());
            if(onChange != null)
            	onChange.accept(setting.get());
            return true;
	}
        return false;
    }
}
