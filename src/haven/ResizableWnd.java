package haven;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

public class ResizableWnd extends Window {
    private static final Tex sizer = Resource.loadtex("gfx/hud/wnd/sizer");    //Database key
    private static final Map<String, Coord> knownSizes = new HashMap<>();
    static {
	//These settings are stored in dynamic.sqlite under `widget_size`
	Storage.dynamic.ensure(sql -> {
	    try(final Statement stmt = sql.createStatement()) {
		stmt.executeUpdate("CREATE TABLE IF NOT EXISTS widget_size ( name TEXT PRIMARY KEY, x INTEGER, y INTEGER )");
	    }
	});
	Storage.dynamic.ensure(sql -> {
	    try(final Statement stmt = sql.createStatement()) {
		try(final ResultSet res = stmt.executeQuery("SELECT name, x, y FROM widget_size")) {
		    while(res.next()) {
			final String name = res.getString(1);
			final int x = res.getInt(2);
			final int y = res.getInt(3);
			knownSizes.put(name, new Coord(x, y));
		    }
		}
	    }
	});
    }
    private final String key;
    private Coord dragc;
    private UI.Grab dm;

    public ResizableWnd(Coord sz, String cap, boolean lg, Coord tlo, Coord rbo) {
	super(sz, cap, cap, lg, tlo, rbo);
	this.key = cap;
    }

    public ResizableWnd(Coord sz, String cap, boolean lg) {
	super(sz, cap, cap, lg);
	this.key = cap;
    }

    public ResizableWnd(Coord sz, String cap) {
	super(sz, cap, cap);
	this.key = cap;
    }

    @Override
    protected void added() {
	if(key != null && knownSizes.containsKey(key)) {
	    resize(knownSizes.get(key));
	}
        super.added();
    }

    @Override
    protected void drawframe(GOut g) {

	g.image(sizer, ctl.add(csz).sub(sizer.sz()));
        super.drawframe(g);
    }

    @Override
    public boolean mousedown(Coord c, int button) {
	if (!locked() && button == 1 && c.isect(ctl.add(csz).sub(sizer.sz()), sizer.sz())) {
	    dm = ui.grabmouse(this);
	    dragc = asz.sub(c);
	    parent.setfocus(this);
	    raise();
	    return true;
	} else {
	    return super.mousedown(c, button);
	}
    }

    @Override
    public boolean mouseup(Coord c, int button) {
        if(dm != null) {
            dm.remove();
            dm = null;
	    knownSizes.put(key, asz);
	    Storage.dynamic.write(sql -> {
		final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO widget_size VALUES (?, ?, ?)");
		stmt.setString(1, key);
		stmt.setInt(2, asz.x);
		stmt.setInt(3, asz.y);
		stmt.executeUpdate();
	    });
            return true;
	} else {
	    return super.mouseup(c, button);
	}
    }

    @Override
    public void mousemove(Coord c) {
        if(dm != null) {
	    Coord nsz = c.add(dragc);
	    nsz.x = Math.max(nsz.x, 100);
	    nsz.y = Math.max(nsz.y, 100);
	    resize(nsz);
	}
	super.mousemove(c);
    }
}
