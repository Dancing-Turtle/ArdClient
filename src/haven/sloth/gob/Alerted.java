package haven.sloth.gob;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.DefSettings;
import haven.Storage;
import haven.sloth.gui.SoundManager;
import haven.sloth.util.ObservableMap;
import haven.sloth.util.ObservableMapListener;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.*;
import java.util.stream.Collectors;

//TODO: Idealy all the sounds we allow should be stored locally and separate from jorb's names to avoid issues in the future
public class Alerted {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public static final List<Resource.Named> sounds = new ArrayList<>();
    private static final ObservableMap<String, Resource.Named> sfxmap = new ObservableMap<>(new TreeMap<>());
	public static ObservableMap<String, Double> volmap = new ObservableMap<>(new TreeMap<>());
	private static HashSet<Long> sgobs = new HashSet<Long>();
	private static HashMap<Long, Long> alertedmap = new HashMap<>(); //map for the purposes of tracking gobs/alerts to ensure alerts only first with a minimum of a 1 second delay
    public static void init(final Storage internal) {
		Storage.dynamic.ensure((sql) -> {
			try (final Statement stmt = sql.createStatement()) {
				stmt.executeUpdate("CREATE TABLE IF NOT EXISTS gob_sound ( name TEXT PRIMARY KEY, sfx TEXT, volume DOUBLE )");
			}
		});
		Storage.dynamic.ensure((sql) -> {
			try (final Statement stmt = sql.createStatement()) {
				try (final ResultSet res = stmt.executeQuery("SELECT name, sfx, volume FROM gob_sound")) {
					while (res.next()) {
						sfxmap.put(res.getString(1), Resource.remote().load(res.getString(2)));
						volmap.put(res.getString(1), res.getDouble(3));
					}
				}
			}
		});
		internal.ensure((sql) -> {
			try (final Statement stmt = sql.createStatement()) {
				try (final ResultSet res = stmt.executeQuery("SELECT name, volume FROM alerts WHERE type_id = (SELECT type_id FROM type WHERE name_key = 'SOUND')")) {
					while (res.next()) {
						sounds.add(Resource.remote().load(res.getString(1)));
						if(!volmap.containsKey(res.getString(1)))
							volmap.put(res.getString(1), res.getDouble(2));
					}
				}
			}
			sounds.sort(Comparator.comparing(Resource.Named::name));
		});
		for (final Resource.Named sound : sounds) {
			try {
				Resource.remote().loadwait(sound.name);
			} catch (Exception e) {
				//Ignore it
				logger.atSevere().withCause(e).log("Failed to load %s", sound);
			}
		}
	}


    public static synchronized void remove(final String name) {
        sfxmap.remove(name);
        Storage.dynamic.write(sql -> {
           final PreparedStatement stmt = Storage.dynamic.prepare("DELETE FROM gob_sound WHERE name = ?");
           stmt.setString(1, name);
           stmt.executeUpdate();
	});
    }

    public static synchronized void add(final String name, final Resource.Named sound, final Double volume) {
        if(!(sfxmap.containsKey(name) && sfxmap.get(name).equals(sound) && volmap.get(name).equals(volume))) {
            //Only update if we have to.
	    sfxmap.put(name, sound);
	    volmap.put(name,volume);
	    Storage.dynamic.write(sql -> {
		final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO gob_sound VALUES (?, ?, ?)");
		stmt.setString(1, name);
		stmt.setString(2, sound.name);
		stmt.setDouble(3, volume);
		stmt.executeUpdate();
	    });
	}
    }

    public synchronized static void listen(final ObservableMapListener<String, Resource.Named> listener) {
	sfxmap.addListener(listener);
    }

    public synchronized static void unlisten(final ObservableMapListener<String, Resource.Named> listener) {
	sfxmap.removeListener(listener);
    }

    public static void checkAlert(final String name, final Gob g) {
    	try {
			if (sfxmap.containsKey(name)) {
				if (!name.equals("gfx/borka/body") && !g.isDead() && !sgobs.contains(g.id)) {
					if (!alertedmap.containsKey(g.id) || (System.currentTimeMillis() - alertedmap.get(g.id) > 5000)) {
						Audio.play(sfxmap.get(name), volmap.get(name));
						if (Config.alarmonce) {
							sgobs.add(g.id);
						}
						alertedmap.put(g.id, System.currentTimeMillis());
					}
				}
			}
		}catch(Exception e){e.printStackTrace();}//crashing during an alarm would be bad
	}


    public static boolean shouldAlert(final String name) {
        return sfxmap.containsKey(name);
    }
}
