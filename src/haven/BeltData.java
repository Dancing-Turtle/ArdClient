package haven;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * Hotkeys are stored in dnyamic.sqlite only for custom items not stored on server-side
 * Table: beltslot
 * Columns:
 * 	character_names
 * 	slot_id
 * 	pagina_key
 *
 */
public class BeltData {
    static {
        Storage.dynamic.ensure(sql -> {
            try(final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS beltslot ( name TEXT, slot INTEGER, item TEXT," +
                        "CONSTRAINT beltslot_pk_name_slot PRIMARY KEY (name, slot) )");
	    }
	});
    }


    private final Map<Integer, String> slotmap = new HashMap<>();
    private final String name;

    public BeltData(final String name) {
        this.name = name;
        Storage.dynamic.ensure(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("SELECT slot, item FROM beltslot WHERE name = ?");
            stmt.setString(1, name);
            try(final ResultSet res = stmt.executeQuery()) {
                while (res.next()) {
                    slotmap.put(res.getInt(1), res.getString(2));
                }
	    }
	});
    }

    public Optional<String> get(final int slot) {
        if(slotmap.containsKey(slot))
            return Optional.of(slotmap.get(slot));
        else
            return Optional.empty();
    }

    public void add(final int slot, final String key) {
        slotmap.put(slot, key);
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO beltslot VALUES (?, ?, ?)");
            stmt.setString(1, name);
            stmt.setInt(2, slot);
            stmt.setString(3, key);
            stmt.executeUpdate();
	});
    }

    public void remove(final int slot) {
        if(slotmap.containsKey(slot)) {
            slotmap.remove(slot);
            Storage.dynamic.write(sql -> {
                final PreparedStatement stmt = Storage.dynamic.prepare("DELETE FROM beltslot WHERE name = ? AND slot = ?");
                stmt.setString(1, name);
                stmt.setInt(2, slot);
                stmt.executeUpdate();
            });
        }
    }
}
