package haven;

import com.google.common.flogger.FluentLogger;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

/**
 * Just a helper class to access our sqlite storage for dynamic settings that are
 * user defined. Unlike static.sqlite this should never be touched in updates
 *
 * Only for writing back, for reading since it's only at startup can be done on their
 * own connections
 */
public class Storage {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Executor writerHandler = Executors.newSingleThreadExecutor();
    public static final Storage dynamic;
    static {
    	final Optional<Storage> cls = create("jdbc:sqlite:dynamic.sqlite");
    	if(cls.isPresent()) {
    	    dynamic = cls.get();
	    Runtime.getRuntime().addShutdownHook(new Thread() {
		public void run() {
		    dynamic.close();
		}
	    });
	} else {
    	    dynamic = null;
	}
    }

    /**
     * This will exit if it fails since its assumed these are important to load in at the start of the client
     */
    public static Optional<Storage> create(final String jdbc) {
	try {
	    return Optional.of(new Storage(jdbc));
	} catch (SQLException se) {
	    logger.atSevere().withCause(se).log("failed to create dynamic storage %s", jdbc);
	    System.exit(0);
	    return Optional.empty();
	}
    }

    private final Connection conn;
    private final Map<String, PreparedStatement> stmts = new HashMap<>();

    private Storage(final String jdbc) throws SQLException {
        this.conn = mkcon(jdbc);
    }

    public PreparedStatement prepare(final String sql) throws SQLException {
	if (stmts.containsKey(sql))
	    return stmts.get(sql);
	else {
	    final PreparedStatement stmt;
	    stmts.put(sql, (stmt = conn.prepareStatement(sql)));
	    return stmt;
	}
    }

    public void close() {
        try {
	    conn.close();
	} catch (SQLException se) {
            logger.atSevere().withCause(se).log("Failed to close %s", conn);
	}
    }

    private Connection mkcon(final String jdbc) throws SQLException {
	final Connection con =  DriverManager.getConnection(jdbc);
	con.setAutoCommit(false);
	return con;
    }

    @FunctionalInterface
    public interface SQLCallback {
        void run(final Connection sql) throws SQLException;
    }

    public synchronized void ensure(final SQLCallback callback) {
	try {
	    callback.run(conn);
	    conn.commit();
	} catch (SQLException se) {
	    try {
		conn.rollback();
	    } catch (SQLException se2) {
		//Eat it.
	    }
	    se.printStackTrace();
	    logger.atSevere().withCause(se).log("Failed to commit transaction");
	    System.exit(0);
	}
    }

    /**
     * These are done async
     */
    public synchronized void write(final SQLCallback callback) {
        final StackTraceElement[] stack = Thread.currentThread().getStackTrace();
	writerHandler.execute(() -> {
	    try {
		callback.run(conn);
		conn.commit();
	    } catch (SQLException se) {
		try {
		    conn.rollback();
		} catch (SQLException se2) {
		    //Eat it.
		}
		for(final StackTraceElement ele : stack) {
		    logger.atSevere().log(ele.toString());
		}
		logger.atSevere().withCause(se).log("Failed to commit transaction");
	    }
	});
    }
}
