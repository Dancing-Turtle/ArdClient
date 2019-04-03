package haven.sloth.gui;
import haven.Coord;
import haven.IButton;
import haven.TextEntry;
import haven.Widget;
import haven.Theme;

import java.util.function.Consumer;

public class NumberEntry extends Widget {
    private final Consumer<Integer> onChange;
    private final Consumer<Integer> onActivate;

    final TextEntry entry;
    private int min;
    private int max;

    /**
     * w in this case is only the width of the TextEntry
     */
    public NumberEntry(final int w, final int defval, final int min, final int max,
		       final Consumer<Integer> onChange, final Consumer<Integer> onActivate) {
	super(Coord.z);
	this.onChange = onChange;
	this.onActivate = onActivate;
	this.min = min;
	this.max = max;
	final IButton add = new IButton(Theme.fullres("buttons/circular/small/add"), this::add);
	final IButton sub = new IButton(Theme.fullres("buttons/circular/small/sub"), this::sub);
	entry = new TextEntry(w - (add.sz.x + sub.sz.x + 6), defval+"", this::onTextChange, this::onTextActivate);
	entry.numeric = true;

	add(add);
	add(entry, add.c.add(add.sz.x + 3, 0));
	add(sub, entry.c.add(entry.sz.x + 3, 0));
	pack();
	add.c = add.c.add(0, sz.y / 2 - add.sz.y / 2);
	sub.c = sub.c.add(0, sz.y / 2 - sub.sz.y / 2);
    }

    public NumberEntry(final int w, final int defval, final int min, final int max) {
        this(w, defval, min, max, (val) -> {}, (val) -> {});
    }

    public int value() {
        return entry.numvalue();
    }

    public void setMin(final int min) { this.min = min; }
    public void setMax(final int max) { this.max = max; }

    private void change(final int dir) {
	final int val;
	if(ui.modshift)
	    val = dir * 10;
	else if(ui.modctrl)
	    val = dir * 5;
	else
	    val = dir;
	entry.settext(entry.numvalue()+val+"");
    }

    @Override
    public boolean mousewheel(Coord c, int amount) {
	if(c.isect(entry.c, entry.sz)) {
	    change(-amount);
	    return true;
	} else {
	    return false;
	}
    }

    private void add() {
        change(1);
    }

    private void sub() {
        change(-1);
    }

    private void onTextChange(final String val) {
        final int num = entry.numvalue();
        if(num < min)
            entry.settext(min+"");
        else if(num > max)
            entry.settext(max+"");
        onChange.accept(entry.numvalue());
    }

    private void onTextActivate(final String val) {
	onActivate.accept(entry.numvalue());
    }
}
