package haven;

import java.util.function.Consumer;

public class IndirHSlider extends Widget {
    private static final String RESNAME = "scroll/horizontal";
    private final int min, max;
    private final IndirSetting<Integer> val;
    private final Consumer<Integer> onChange;
    private UI.Grab drag = null;

    public IndirHSlider(int w, int min, int max, IndirSetting<Integer> val, final Consumer<Integer> onChange) {
	super(new Coord(w, Theme.timg(RESNAME, 3).sz.y));
	this.val = val;
	this.min = min;
	this.max = max;
	this.onChange = onChange;
    }

    public IndirHSlider(int w, int min, int max, IndirSetting<Integer> val) {
	this(w, min, max, val, null);
    }

    public void draw(GOut g) {
	g.chcolor(DefSettings.SLIDERCOL.get());
	//y offset incase sflarp.sz.y > schain.sz.y
	int cy = (Theme.timg(RESNAME, 3).sz.y / 2) - (Theme.timg(RESNAME, 0).sz.y / 2);
	//Top
	g.image(Theme.timg(RESNAME, 0), new Coord(0, cy));
	//middle
	for(int x = Theme.timg(RESNAME, 0).sz.x; x < sz.x - Theme.timg(RESNAME, 2).sz.x; x += Theme.timg(RESNAME, 1).sz.x)
	    g.image(Theme.timg(RESNAME, 1), new Coord(x, cy));
	//bottom
	g.image(Theme.timg(RESNAME, 2), new Coord(sz.x-Theme.timg(RESNAME, 2).sz.x, cy));
	//slider
	int fx = ((sz.x - Theme.timg(RESNAME, 3).sz.x) * (val.get() - min)) / (max - min);
	g.image(Theme.timg(RESNAME, 3), new Coord(fx, 0));
	g.chcolor();
    }

    public boolean mousedown(Coord c, int button) {
	if(button != 1)
	    return(false);
	drag = ui.grabmouse(this);
	mousemove(c);
	return(true);
    }

    public void mousemove(Coord c) {
	if(drag != null) {
	    double a = (double)(c.x - (Theme.timg(RESNAME, 3).sz.x / 2)) / (double)(sz.x - Theme.timg(RESNAME, 3).sz.x);
	    if(a < 0)
		a = 0;
	    if(a > 1)
		a = 1;
	    val.set((int)Math.round(a * (max - min)) + min);
	    if(onChange != null)
	    	onChange.accept(val.get());
	}
    }

    public boolean mouseup(Coord c, int button) {
	if(button != 1)
	    return(false);
	if(drag == null)
	    return(false);
	drag.remove();
	drag = null;
	return(true);
    }

    public void resize(int w) {
	super.resize(new Coord(w, Theme.timg(RESNAME, 3).sz.y));
    }
}
