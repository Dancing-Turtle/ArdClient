package haven.sloth.io;

import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import haven.Storage;

public class ItemData {
    public enum ContainerType {
        LIQUID, WEIGHT, SEED
    }
    private static class Container {
        final double liquid_max;
        final double weight_max;
        final double seed_max;

        private Container(final double l, final double d, final double s) {
            this.liquid_max = l;
            this.weight_max = d;
            this.seed_max = s;
	}
    }

    private static final Set<String> equipables = new HashSet<>();
    private static final Map<String, Container> containers = new HashMap<>();
    public static void init(final Storage internal) {
	internal.ensure((sql) -> {
	    try(final Statement stmt = sql.createStatement()) {
		try(final ResultSet res = stmt.executeQuery("SELECT name_key FROM item WHERE item_id in (SELECT item_id FROM item_equipable)")) {
		    while (res.next()) {
		        equipables.add(res.getString(1));
		    }
		}
		try(final ResultSet res = stmt.executeQuery("SELECT item.name_key, item_contents.liquid_max, item_contents.weight_max, item_contents.seed_max FROM item JOIN item_contents USING (item_id)")) {
		    while (res.next()) {
		        containers.put(res.getString(1),
				new Container(res.getDouble(2), res.getDouble(3), res.getDouble(4)));
		    }
		}
	    }
	});
    }

    public static boolean isEquipable(final String name) {
        return equipables.contains(name.toUpperCase());
    }

    public static double maxContent(final String name, final ContainerType type) {
        if(containers.containsKey(name.toUpperCase())) {
	    final Container cont = containers.get(name.toUpperCase());
	    switch (type) {
		case LIQUID: return cont.liquid_max;
		case WEIGHT: return cont.weight_max;
		case SEED: return cont.seed_max;
		default: return 0;
	    }
        } else {
            return 0;
	}
    }
}
