package haven.sloth.gob;

import com.google.common.flogger.FluentLogger;

import java.sql.*;
import java.util.HashMap;
import java.util.Map;

public enum Type {
    PLANT,
    MULTISTAGE_PLANT,
    HUMAN,
    ANIMAL,
    DANGANIMAL,
    TAMEDANIMAL,
    SMALLANIMAL,
    WATERVEHICLE,
    VEHICLE,
    TILE,
    SOUND,
    TREE,
    BUSH,
    BOULDER,
    ROAD,
    ROADENDPOINT,
    DUNGEONDOOR,
    WOODEN_SUPPORT,
    STONE_SUPPORT,
    METAL_SUPPORT,
    TROUGH,
    BEEHIVE,
    DFRAME,
    GARDENPOT,
    TANTUB,
    CUPBOARD,
    CHEESERACK,
    HUTCH,
    COOP,
    STRANGLEVINE,
    WALLSEG,
    SHED,
    UNKNOWN;


    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static Map<String, Type> types = new HashMap<>();
    static {
	try(final Connection sql = DriverManager.getConnection("jdbc:sqlite:static.sqlite")) {
	    try(final Statement stmt = sql.createStatement()) {
		try(final ResultSet res = stmt.executeQuery(
			"SELECT object.name, type.name_key " +
				"FROM object JOIN type USING (type_id)")) {
		    while (res.next()) {
			types.put(res.getString(1), Type.valueOf(res.getString(2)));
		    }
		}
	    }
	} catch (final SQLException e) {
	    logger.atSevere().withCause(e).log("Failed to load movable data from static sqlite");
	    System.exit(0);
	}
    }

    public static Type getType(final String name)
    {
        return types.getOrDefault(name, UNKNOWN);
    }
}
