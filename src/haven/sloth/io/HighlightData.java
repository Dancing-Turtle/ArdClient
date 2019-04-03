package haven.sloth.io;

import haven.sloth.util.ObservableCollection;
import haven.sloth.util.ObservableListener;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashSet;
import haven.Storage;

public class HighlightData {
    private static ObservableCollection<String> highlighted = new ObservableCollection<>(new HashSet<>());
    public static void init() {
	Storage.dynamic.ensure(sql -> {
	    try(final Statement stmt = sql.createStatement()) {
		stmt.executeUpdate("CREATE TABLE IF NOT EXISTS gob_highlight ( name TEXT PRIMARY KEY )");
	    }
	});
	Storage.dynamic.ensure(sql -> {
	    try(final Statement stmt = sql.createStatement()) {
		try(final ResultSet res = stmt.executeQuery("SELECT name FROM gob_highlight")) {
		    while (res.next()) {
			highlighted.add(res.getString(1));
		    }
		}
	    }
	});
    }

    public synchronized static void listen(final ObservableListener<String> listener) {
	highlighted.addListener(listener);
    }

    public synchronized static void unlisten(final ObservableListener<String> listener) {
	highlighted.removeListener(listener);
    }

    public synchronized static void add(final String name) {
	highlighted.add(name);
	Storage.dynamic.write(sql -> {
	    final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR IGNORE INTO gob_highlight VALUES (?)");
	    stmt.setString(1, name);
	    stmt.executeUpdate();
	});
    }

    public synchronized static void remove(final String name) {
	highlighted.remove(name);
	Storage.dynamic.write(sql -> {
	    final PreparedStatement stmt = Storage.dynamic.prepare("DELETE FROM gob_highlight WHERE name = ?");
	    stmt.setString(1, name);
	    stmt.executeUpdate();
	});
    }

    public synchronized static boolean isHighlighted(final String name) {
	return highlighted.contains(name);
    }
}
