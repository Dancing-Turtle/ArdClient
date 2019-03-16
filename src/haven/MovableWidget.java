package haven;

import haven.Storage;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

/**
 * Based class to handle everything to do with moving widgets around.
 */
public abstract class MovableWidget extends Widget {
    public static final Map<String, Coord2d> knownPositions = new HashMap<>();
    private static final Map<String, Boolean> knownLocks = new HashMap<>();
    static {
        //These settings are stored in dynamic.sqlite under `widget_position`
	Storage.dynamic.ensure(sql -> {
	    try(final Statement stmt = sql.createStatement()) {
            stmt.executeUpdate("CREATE TABLE IF NOT EXISTS widget_position ( name TEXT PRIMARY KEY, x REAL, y REAL, locked BOOLEAN)");
            //For older client versions widget_position didn't have the `locked` column and it needs added in
            try(final ResultSet res = stmt.executeQuery("PRAGMA table_info(widget_position)")) {
                boolean has3 = false;
                while(res.next()) {
                    if(res.getInt(1) == 3) {
                        has3 = true;
                        break;
                    }
                }

                if(!has3) {
                    //Older client, make the column
                    stmt.executeUpdate("ALTER TABLE widget_position ADD COLUMN locked BOOLEAN");
                }
            }
        }
    });
            Storage.dynamic.ensure(sql -> {
	    try(final Statement stmt = sql.createStatement()) {
            try(final ResultSet res = stmt.executeQuery("SELECT name, x, y, locked FROM widget_position")) {
	            while(res.next()) {
	                final String name = res.getString(1);
	                final double x = res.getDouble(2);
	                final double y = res.getDouble(3);
                    final boolean locked = res.getBoolean(4);
	                knownPositions.put(name, new Coord2d(x, y));
                    knownLocks.put(name, locked);
		    }
		}
	    }
	});
    }
    public static final double VISIBLE_PER = 0.8;

    //Database key
    private final String key;
    //Whether we want to lock the current position or not
        private boolean lock;

    private UI.Grab dm = null;
    private Coord doff;

    public MovableWidget(final Coord sz, final String name) {
        super(sz);
        this.key = name;
    }

    public MovableWidget(final String name) {
        super();
        this.key = name;
    }

    public MovableWidget() {
        super();
        this.key = null;
    }

    public MovableWidget(final UI ui, final Coord c, final Coord sz, final String name) {
        super(ui, c, sz);
        this.key = name;
    }

    public void toggleLock() {
     lock = !lock;
     savePosition();
     }


    public boolean locked() { return lock; }

    private void loadPosition() {
        if(key != null && knownPositions.containsKey(key)) {
            setPosRel(knownPositions.get(key));
	}
    }

    public boolean moving() {
        return dm != null;
    }


    private void savePosition() {
        if(key != null) {
            final Coord2d rel = relpos();
            knownPositions.put(key, rel);
            Storage.dynamic.write(sql -> {
                final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO widget_position VALUES (?, ?, ?, ?)");
                stmt.setString(1, key);
                stmt.setDouble(2, rel.x);
                stmt.setDouble(3, rel.y);
                stmt.setBoolean(4, lock);
                stmt.executeUpdate();
	    });
	}
    }

    @Override
    protected void added() {
	loadPosition();
            lock = knownLocks.getOrDefault(key, false);
        super.added();
    }

    protected abstract boolean moveHit(final Coord c, final int btn);

    @Override
    public boolean mousedown(final Coord mc, final int button) {
        if(super.mousedown(mc, button)) {
            //Give preference to the Widget using this
            return true;
	} else if(moveHit(mc, button)) {
            if(!lock) {
		dm = ui.grabmouse(this);
		doff = mc;
		parent.setfocus(this);
		raise();
	    }
            return true;
	} else {
            return false;
	}
    }

    @Override
    public boolean mouseup(final Coord mc, final int button) {
        if(dm != null) {
            //Preference to this if we're in the middle of moving the widget
            dm.remove();
            dm = null;
            //Ensure user didn't throw the window right off the visible screen...
	    if( (c.x + sz.x * VISIBLE_PER) > parent.sz.x) {
		c.x = parent.sz.x - sz.x;
	    } else if((c.x + (sz.x * VISIBLE_PER)) < 0) {
		c.x = 0;
	    }
	    if((c.y + sz.y * VISIBLE_PER) > parent.sz.y) {
		c.y = parent.sz.y - sz.y;
	    } else if((c.y + (sz.y * VISIBLE_PER)) < 0) {
		c.y = 0;
	    }
            savePosition();
	    return true;
	} else {
            return super.mouseup(mc, button);
	}
    }

    @Override
    public void mousemove(final Coord mc) {
        if(dm != null) {
	    //Preference to this if we're in the middle of moving the widget
	    c = c.add(mc.add(doff.inv()));
	} else {
	    super.mousemove(mc);
	}
    }
}
