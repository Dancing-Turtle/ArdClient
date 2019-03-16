package haven;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

public class HiddenWndData {

        private static final Map<String, Boolean> knownHidables = new HashMap<>();
        static {
            Storage.dynamic.ensure(sql -> {
                try(final Statement stmt = sql.createStatement()) {
                    stmt.executeUpdate("CREATE TABLE IF NOT EXISTS window_hidden ( name TEXT PRIMARY KEY, canhide BOOLEAN )");
                }
            });
            Storage.dynamic.ensure(sql -> {
                try(final Statement stmt = sql.createStatement()) {
                    try(final ResultSet res = stmt.executeQuery("SELECT name, canhide FROM window_hidden")) {
                        while(res.next()) {
                            final String name = res.getString(1);
                            final boolean hide = res.getBoolean(2);
                            knownHidables.put(name, hide);
                        }
                    }
                }
            });
        }

        public static boolean shouldHide(final String name) {
            return knownHidables.getOrDefault(name, false);
        }

        public static void saveHide(final String name, final boolean hide) {
            Storage.dynamic.write(sql -> {
                final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO window_hidden VALUES (?, ?)");
                stmt.setString(1, name);
                stmt.setBoolean(2, hide);
                stmt.executeUpdate();
            });
        }
    }

