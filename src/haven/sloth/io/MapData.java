package haven.sloth.io;

import com.google.common.flogger.FluentLogger;
import haven.Coord;
import haven.LocalMiniMap;

import javax.imageio.ImageIO;
import java.awt.*;
import java.io.ByteArrayOutputStream;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

/**
 * Segment
 * 	Long 	 :: ID
 * 	Grid[] 	 :: grids
 * 	Marker[] :: markers
 *
 * Grid
 * 	Long	:: Segment ID
 * 	Long	:: Grid ID
 * 	Coord	:: Offset
 * 	Byte[]	:: Rendered Image
 *
 * Marker
 *	Long	:: Marker ID
 *   	Boolean :: Natural
 * 	Long	:: Grid ID
 * 	String	:: Name
 * 	Color	:: Marker Color
 * 	Coord	:: Offset
 * For natural objects the client provides a unique OID (true gob id) to identify
 * For placed objects, they will just generate a new id on creation (not implemented yet)
 *
 *Every frame there's an update to MCache or a new grid:
 * For each grid:
 * 	- Check if we already know about this grid
 * 		- Yes: update the rendered image, is `mseq` defined?
 * 			- Yes: Does `mseq` match this Grid Segment ID?
 * 				- Yes: Nothing addition
 * 				- No : Merge `mseq` and this Grid Segment ID together
 * 			- No : define `mseq` as our grid segment id
 * 		- No : Is `mseq` undefined?
 * 			- Yes: Make a new segment and define `mseq` to be its ID
 * 			- No : Add this grid into `mseq` Segment
 * This is a way to replicate Loftar's Map but in a manageable way that isn't a thousand little
 * files that require the mess that he had to make to even reassemble maps
 *
 * Merging is also just a matter of checking Grid for same grid_ids that have different segment ids
 *
 * For now this is just passive and will save data in the background
 *----------
 * Good idea, not useful though in the long run. Loftar's maps work just as good and have potential for modifications
 */
public class MapData {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public static void init() {
        Storage.dynamic.write(sql -> {
            try(final Statement stmt = sql.createStatement()) {
		//This is no longer used, clear up space and delete these tables
		boolean cleanup = false;
		try (final ResultSet res = stmt.executeQuery("SELECT name FROM sqlite_master WHERE type='table'")) {
		    while(res.next()) {
		        if(res.getString(1).equals("map_segment")) {
		            cleanup = true;
		            break;
			}
		    }
		}
		if (cleanup) {
		    stmt.executeUpdate("DROP TABLE map_segment");
		    stmt.executeUpdate("DROP TABLE map_grid");
		    stmt.executeUpdate("DROP TABLE map_marker");
		}
	    }
                /*
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS map_segment ( map_segment_id INTEGER PRIMARY KEY, x INTEGER, y INTEGER )");
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS map_grid (" +
			"map_segment_id INTEGER," +
			"map_grid_id INTEGER," +
			"x INTEGER," +
			"y INTEGER," +
			"img BLOB," +
			"CONSTRAINT map_grid_pk_segment_grid PRIMARY KEY (map_segment_id, map_grid_id))");
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS map_marker (" +
			"map_marker_id INTEGER," +
			"natural BOOLEAN," +
			"map_grid_id INTEGER," +
			"name TEXT," +
			"color INTEGER," +
			"x INTEGER," +
			"y INTEGER," +
			"CONSTRAINT map_marker_pk_id_natural PRIMARY KEY (map_marker_id, natural))");*/
	});
    }

    // current map segment id
    private long segment_id;
    private Coord center = null;

    /**
     * @deprecated
     */
    public void newSegment(final Coord center) {
        Storage.dynamic.write(sql -> {
	    try {
		final PreparedStatement stmt = Storage.dynamic.prepare("INSERT INTO map_segment (x, y) VALUES (?, ?)");
		stmt.setInt(1, center.x);
		stmt.setInt(2, center.y);
		stmt.executeUpdate();
		try (final ResultSet key = stmt.getGeneratedKeys()) {
		    if (key.next()) {
			segment_id = key.getLong(1);
			this.center = center;
		    } else {
			this.center = null;
		    }
		}
	    } catch (SQLException se) {
		logger.atSevere().withCause(se).log("Failed to create new segment");
		this.center = null;
		throw se;
	    }
	});
    }

    /**
     * @deprecated
     */
    public void save(final LocalMiniMap.MapTile tile) {
    	//Only matters if we had an active segment
        if(center != null) {
            final Coord offset = center.sub(tile.gc);
            Storage.dynamic.write(sql -> {
                final ByteArrayOutputStream data = new ByteArrayOutputStream();
                try {
		    ImageIO.write(tile.img.back, "png", data);
		} catch (Exception e) {
                    logger.atSevere().withCause(e).log("Failed to serialize backing image");
                    return;
		}

                final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO map_grid VALUES (?, ?, ?, ?, ?)");
                stmt.setLong(1, segment_id);
                stmt.setLong(2, tile.id);
                stmt.setInt(3, offset.x);
                stmt.setInt(4, offset.y);
                stmt.setBytes(5, data.toByteArray());
		stmt.executeUpdate();
	    });
	}
    }

    /**
     * @deprecated
     */
    public void saveNaturalMarker(final long oid, final String nm, final long grid_id, final Coord offset) {
	if(center != null) {
	    Storage.dynamic.write(sql -> {
		final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO map_marker VALUES (?, ?, ?, ?, ?, ?, ?)");
		stmt.setLong(1, oid);
		stmt.setBoolean(2, true);
		stmt.setLong(3, grid_id);
		stmt.setString(4, nm);
		stmt.setInt(5, Color.WHITE.getRGB());
		stmt.setInt(6, offset.x);
		stmt.setInt(7, offset.y);
		stmt.executeUpdate();
	    });
	}
    }
}
