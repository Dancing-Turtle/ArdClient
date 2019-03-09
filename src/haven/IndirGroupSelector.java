package haven;

import java.awt.*;
import java.util.function.Consumer;

public class IndirGroupSelector extends Widget {
    private final Color[] groups;
    private final IndirSetting<Integer> group;
    private Consumer<Integer> changed;

    public IndirGroupSelector(IndirSetting<Integer> group, final Color[] groups, Consumer<Integer> changed) {
	super(new Coord((groups.length * 20) + 20, 20));
	this.group = group;
	this.groups = groups;
	this.changed = changed;
    }

    public IndirGroupSelector(IndirSetting<Integer> group, final Color[] groups) {
	this(group, groups,null);
    }

    public void draw(GOut g) {
	for(int i = 0; i < groups.length; i++) {
	    if(i == group.get()) {
		g.chcolor();
		g.frect(new Coord(i * 20, 0), new Coord(19, 19));
	    }
	    g.chcolor(groups[i]);
	    g.frect(new Coord(2 + (i * 20), 2), new Coord(15, 15));
	}
	g.chcolor();
    }

    public boolean mousedown(Coord c, int button) {
	if((c.y >= 2) && (c.y < 17)) {
	    int g = (c.x - 2) / 20;
	    if((g >= 0) && (g < groups.length) && (c.x >= 2 + (g * 20)) && (c.x < 17 + (g * 20))) {
	        group.set(g);
	        if(changed != null)
	            changed.accept(group.get());
		return(true);
	    }
	}
	return(super.mousedown(c, button));
    }
}
