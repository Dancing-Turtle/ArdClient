package haven;

import haven.TextureAtlas;

import java.util.function.Consumer;

public class IndirRadioGroup<T> extends Widget {
    private static final int spacer = 5;
    private class RadioButton extends Widget {
	private final T condition;
	private final Text lbl;

	private RadioButton(final String name, final T condition) {
	    this.condition = condition;
	    this.lbl = Text.render(name);

	    final Coord boxsz = Theme.timg("chkbox/small", 0).sz;
	    resize(new Coord(boxsz.x + spacer + this.lbl.sz().x, Math.max(boxsz.y, this.lbl.sz().y)));
	}

	public void draw(GOut g) {
	    //Draw checkbox
	    final int id = setting.get().equals(condition) ? 1 : 0;
	    final TextureAtlas.Img chk = Theme.timg("chkbox/small", id);
	    g.image(chk, new Coord(0, sz.y / 2 - chk.sz.y / 2));
	    //Draw label
	    g.image(lbl.tex(), new Coord(chk.sz.x + spacer, sz.y /2 - lbl.sz().y / 2 ));
	    super.draw(g);
	}

	public boolean mousedown(Coord c, int button) {
	    if(button == 1 && c.isect(Coord.z, Theme.timg("chkbox/small", 0).sz)) {
	        if(!setting.get().equals(condition)) { //Don't allow deselecting
	            setting.set(condition);
	            if(onChange != null)
	                onChange.accept(setting.get());
		}
		return true;
	    }
	    return false;
	}
    }

    public static final IBox box = new IBox(Theme.fullres("frame"));
    private final static int SPACER = 5;
    private Coord nc = new Coord(box.ctloff());
    private final Label title;
    private final IndirSetting<T> setting;
    private final Consumer<T> onChange;

    public IndirRadioGroup(final String lbl, final IndirSetting<T> setting, final Consumer<T> callback) {
        this.setting = setting;
        this.onChange = callback;
	nc.y += add(title = new Label(lbl), nc.copy()).sz.y;
    }

    public IndirRadioGroup(final String lbl, final IndirSetting<T> setting) {
	this(lbl, setting, null);
    }

    @Override
    public void draw(GOut g) {
	box.draw(g, Coord.z, sz);
	super.draw(g);
    }

    public void add(final String lbl, final T condition) {
	final RadioButton btn = add(new RadioButton(lbl, condition), nc.copy());
	nc.y += btn.sz.y + SPACER;
	pack();
	resize(sz.add(box.cbr.sz()));
	title.c.x = sz.x/2 - title.sz.x/2;
    }
}
