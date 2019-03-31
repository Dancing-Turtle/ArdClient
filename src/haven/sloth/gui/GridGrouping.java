package haven.sloth.gui;

import haven.Coord;
import haven.Widget;

public class GridGrouping extends Grouping {
    private final int spacer;
    private final int maxy;

    public GridGrouping(final String cap, final int spacer, final int maxy) {
	super(cap);
	this.spacer = spacer;
	this.maxy = maxy;
    }

    @Override
    public void pack() {
	int y = 0;
	int x = 0;
	int stepx = 0;

	for(Widget wdg = child; wdg != null; wdg = wdg.next) {
	    if(y + wdg.sz.y + spacer > maxy) {
	        y = 0;
	        x += stepx + spacer;
	        stepx = 0;
	    }
	    stepx = Math.max(stepx, wdg.sz.x);
	    wdg.c = new Coord(x, y);
	    y += wdg.sz.y + spacer;
	}

	super.pack();
    }
}
