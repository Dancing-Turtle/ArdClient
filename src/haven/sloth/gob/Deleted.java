package haven.sloth.gob;

import haven.Storage;
import haven.sloth.util.ObservableCollection;
import haven.sloth.util.ObservableListener;
import haven.DefSettings;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashSet;

public class Deleted {
    private static ObservableCollection<String> deleted = new ObservableCollection<>(new HashSet<>());
    public static void init() {
        Storage.dynamic.ensure(sql -> {
            try(final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS gob_deleted ( name TEXT PRIMARY KEY )");
            }
        });
        Storage.dynamic.ensure(sql -> {
            try(final Statement stmt = sql.createStatement()) {
                try(final ResultSet res = stmt.executeQuery("SELECT name FROM gob_deleted")) {
                    while (res.next()) {
                        deleted.add(res.getString(1));
                    }
                }
            }
        });
    }

    public synchronized static void listen(final ObservableListener<String> listener) {
        deleted.addListener(listener);
    }

    public synchronized static void unlisten(final ObservableListener<String> listener) {
        deleted.removeListener(listener);
    }

    public synchronized static void add(final String name) {
        deleted.add(name);
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR IGNORE INTO gob_deleted VALUES (?)");
            stmt.setString(1, name);
            stmt.executeUpdate();
        });
    }

    public synchronized static void remove(final String name) {
        deleted.remove(name);
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("DELETE FROM gob_deleted WHERE name = ?");
            stmt.setString(1, name);
            stmt.executeUpdate();
        });
    }

    public synchronized static boolean isDeleted(final String name) {
        return deleted.contains(name);
    }
}
