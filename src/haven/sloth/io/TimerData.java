package haven.sloth.io;

import com.google.common.flogger.FluentLogger;
import haven.sloth.util.ObservableCollection;
import haven.sloth.util.ObservableListener;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class TimerData extends Thread {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final double timefac = 3.0;
    private static final ObservableCollection<Timer> timers = new ObservableCollection<>(new ArrayList<>());
    static {
	Storage.dynamic.ensure(sql -> {
	    try(final Statement stmt = sql.createStatement()) {
		stmt.executeUpdate("CREATE TABLE IF NOT EXISTS timer ( timer_id INTEGER PRIMARY KEY, name TEXT, duration INTEGER )");
		stmt.executeUpdate("CREATE TABLE IF NOT EXISTS timer_instance ( timer_instance_id INTEGER PRIMARY KEY, timer_id INTEGER, start INTEGER )");
	    }
	});

	Storage.dynamic.ensure(sql -> {
	    final List<Integer> delete = new ArrayList<>();
	    try(final Statement stmt = sql.createStatement()) {
	        try(final ResultSet res = stmt.executeQuery("SELECT timer_id, name, duration FROM timer")) {
	            while(res.next()) {
	                timers.add(new Timer(res.getInt(1), res.getString(2), res.getLong(3)));
		    }
		}
		try(final ResultSet res = stmt.executeQuery("SELECT timer_instance_id, timer_id, start FROM timer_instance")) {
		    while (res.next()) {
			final TimerInstance inst = new TimerInstance(res.getInt(1), res.getLong(3));
			final Optional<Timer> timer = getTimer(res.getInt(2));
			if (timer.isPresent()) {
			    timer.get().addInstance(inst);
			} else {
			    logger.atWarning().log("Found timer instance without timer [Instance %d] [start %d]", inst.id, inst.start);
			    delete.add(inst.id);
			}
		    }
		}
	    }

	    final PreparedStatement stmt = Storage.dynamic.prepare("DELETE FROM timer_instance WHERE timer_instance_id = ?");
	    for(final int id : delete) {
		stmt.setInt(1, id);
		stmt.executeUpdate();
	    }
	});
    }

    public static class Timer {
        public final int id;
	public final String name;
	public final long duration;
	private ObservableCollection<TimerInstance> instances = new ObservableCollection<>(new ArrayList<>());

	private Timer(int id, String n, long d) {
	    this.id = id;
	    name = n;
	    duration = d;
	}

	public void listen(final ObservableListener<TimerInstance> listener) {
	    instances.addListener(listener);
	}

	public void unlisten(final ObservableListener<TimerInstance> listener) {
	    instances.removeListener(listener);
	}

	public void makeInstance(final long globtime) {
	    final long start = globtime;
	    Storage.dynamic.write(sql -> {
	        final PreparedStatement stmt = Storage.dynamic.prepare("INSERT INTO timer_instance (timer_id, start) VALUES (?, ?)");
	        stmt.setInt(1, this.id);
	        stmt.setLong(2, start);
	        stmt.executeUpdate();
	        try(final ResultSet keys = stmt.getGeneratedKeys()) {
	            if(keys.next()) {
			this.instances.add(new TimerInstance(keys.getInt(1), start));
		    }
		}
	    });
	}

	void addInstance(final TimerInstance inst) {
	    instances.add(inst);
	}

	public void finish(final TimerInstance inst) {
	    instances.remove(inst);
	    Storage.dynamic.write(sql -> {
		final PreparedStatement stmt = Storage.dynamic.prepare("DELETE FROM timer_instance WHERE timer_instance_id = ?");
		stmt.setInt(1, inst.id);
		stmt.executeUpdate();
	    });
	}
    }

    public static class TimerInstance {
        private final int id;
        public long start;

	private TimerInstance(final int id, final long start) {
            this.id = id;
            this.start = start;
	}
    }


    public static void listenTimers(final ObservableListener<Timer> listener) {
        timers.addListener(listener);
    }

    public static void removeTimerListener(final ObservableListener<Timer> listener) {
        timers.removeListener(listener);
    }

    private static Optional<Timer> getTimer(final int id) {
        for(final Timer timer : timers) {
            if(timer.id == id) {
                return Optional.of(timer);
	    }
	}
        return Optional.empty();
    }

    public static void addTimer(final String name, final long duration) {
	Storage.dynamic.write(sql -> {
	    final PreparedStatement stmt = Storage.dynamic.prepare("INSERT INTO timer (name, duration) VALUES (?, ?)");
	    stmt.setString(1, name);
	    stmt.setLong(2, duration);
	    stmt.executeUpdate();
	    try (final ResultSet keys = stmt.getGeneratedKeys()) {
		if (keys.next()) {
		    timers.add(new Timer(keys.getInt(1), name, duration));
		}
	    }
	});
    }

    public static void remTimer(final Timer t) {
	timers.remove(t);
	Storage.dynamic.write(sql -> {
	    final PreparedStatement stmt = Storage.dynamic.prepare("DELETE FROM timer WHERE timer_id = ?");
	    final PreparedStatement stmt2 = Storage.dynamic.prepare("DELETE FROM timer_instance WHERE timer_id = ?");
	    stmt.setInt(1, t.id);
	    stmt.executeUpdate();
	    stmt2.setInt(1, t.id);
	    stmt.executeUpdate();
	});
    }
}
