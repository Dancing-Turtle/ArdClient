package haven.sloth.gui;

import haven.*;
import haven.Theme;

import java.awt.*;

public class Grouping extends Widget {
    public static final IBox box = new IBox(Theme.fullres("frame"));
    private Text cap;
    private Coord ctl;

    public Grouping(final String cap) {
        setCap(cap);
    }

    public void setCap(final String cap) {
        this.cap = Text.renderstroked(cap, Color.WHITE, Color.BLACK);
    }

    @Override
    public void resize(Coord sz) {
        final Coord capsz = cap.sz();
        ctl = box.ctloff().add(0, capsz.y);
        sz.x = Math.max(sz.x, capsz.x);
	this.sz = sz.add(box.cisz().add(0, cap.sz().y));
    }

    @Override
    public void draw(GOut g) {
        box.draw(g, Coord.z, sz);
        g.aimage(cap.tex(), new Coord(sz.x/2, box.ctloff().y), 0.5, 0);
	super.draw(g);
    }

    public Coord xlate(Coord c, boolean in) {
	if(in)
	    return(c.add(ctl));
	else
	    return(c.sub(ctl));
    }
}
