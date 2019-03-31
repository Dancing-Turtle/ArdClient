package haven.sloth.io;

import haven.MapFile;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

//Too small for me to care to put into static.sqlite
public class MarkerData {
    public static class Marker {
        public final String defname;
        public final String res;
        public final Type type;

        public Marker(final String defname, final String res, final Type type) {
            this.defname = defname;
            this.res = res;
            this.type = type;
	}
    }

    public static class LinkedMarker extends Marker {
        public final byte ltype;
        public LinkedMarker(final String defname, final String res, final Type type, final byte ltype) {
            super(defname, res, type);
            this.ltype = ltype;
	}
    }

    enum Type {
        LINKED, SLOTH
    }
    private static final Map<String, Marker> markable = new HashMap<>();
    static {
	markable.put("gfx/terobjs/villageidol", new Marker("Village", "custom/mm/icons/vidol", Type.SLOTH));
        markable.put("gfx/terobjs/minehole", new LinkedMarker("Minehole", "custom/mm/icons/minehole", Type.LINKED, MapFile.MINEHOLE));
	markable.put("gfx/terobjs/ladder", new LinkedMarker("Ladder", "custom/mm/icons/ladder", Type.LINKED, MapFile.LADDER));
	markable.put("gfx/tiles/ridges/cavein", new LinkedMarker("Cave (Surface)", "custom/mm/icons/cave", Type.LINKED, MapFile.CAVE));
        markable.put("gfx/tiles/ridges/caveout", new LinkedMarker("Cave (L1)", "custom/mm/icons/cave", Type.LINKED, MapFile.CAVEIN));
    }

    public static Optional<Marker> marker(final String name) {
        return Optional.ofNullable(markable.get(name));
    }
}
