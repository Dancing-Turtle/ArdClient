package haven.sloth.gui;

import haven.*;
import haven.Theme;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

/**
 * Simple radio group for checkboxes that displays them in a vertical list
 *
 * Maybe change it someday to do grids, etc
 */
public class RadioGroup extends Widget {
    private class RadioButton extends CheckBox {
	private final Consumer<Boolean> callback;

	private RadioButton(final String name, final Consumer<Boolean> callback) {
	    super(name, false);
	    this.callback = callback;
	    this.a = false;
	}

	public void set(boolean a) {
	    //Can never deselect a radiobutton
	    if (a) {
		this.a = true;
		changed(true);
	    }
	}

	public void changed(boolean val) {
	    super.changed(val);
	    callback.accept(val);
	    select(this);
	}
    }

    public static final IBox box = new IBox(Theme.fullres("frame"));
    private final static int SPACER = 5;
    private List<RadioButton> btns = new ArrayList<>();
    private Coord nc = new Coord(box.ctloff());
    private final Label title;

    public RadioGroup(final String lbl) {
	nc.y += add(title = new Label(lbl), nc.copy()).sz.y;
    }

    @Override
    public void draw(GOut g) {
        box.draw(g, Coord.z, sz);
	super.draw(g);
    }

    private void select(final RadioButton btn) {
	btns.forEach((obtn) -> obtn.a = obtn == btn);
    }

    public void add(final String lbl, final boolean selected, final Consumer<Boolean> callback) {
	final RadioButton btn = add(new RadioButton(lbl, callback), nc.copy());
	btns.add(btn);
	nc.y += btn.sz.y + SPACER;
	if (selected)
	    select(btn);
	pack();
	resize(sz.add(box.cbr.sz()));
	title.c.x = sz.x/2 - title.sz.x/2;
    }

    public void add(final String lbl, final Consumer<Boolean> callback) {
	add(lbl, false, callback);
    }
}

