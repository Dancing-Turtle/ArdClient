package haven.sloth.gob;

import haven.*;
import haven.res.fx.bprad.BPRad;
import haven.resutil.BPRadSprite;

import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;
import haven.Storage;

public class Range extends GAttrib implements Rendered {
    private static Map<Integer, BPRadSprite> rads = new HashMap<>();
	private static Map<Integer, BPRadSprite> radshive = new HashMap<>();
	private static Map<Integer, BPRadSprite> radstrough = new HashMap<>();
	private static Map<String, BPRadSprite> rangemap = new HashMap<>();
	private static Map<String, BPRadSprite> rangemaphive = new HashMap<>();
	private static Map<String, BPRadSprite> rangemaptrough = new HashMap<>();

    public static void init(final Storage internal) {
    	System.out.println("Range class initalizing");
	internal.ensure(sql -> {
	    try (final Statement stmt = sql.createStatement()) {
		try (final ResultSet res = stmt.executeQuery(
			"SELECT object.name, range.radius " +
				"FROM object JOIN range USING (object_id)")) {
		    while (res.next()) {
			final String name = res.getString(1);
			final int tiles = res.getInt(2);
			if (rads.containsKey(tiles)) {
			    rangemap.put(name, rads.get(tiles));
			} else if(radshive.containsKey(tiles)){
				rangemaphive.put(name,radshive.get(tiles));
			} else if(radstrough.containsKey(tiles)){
				rangemaptrough.put(name,radstrough.get(tiles));
			}else{
				if(name.contains("hive")){
					radshive.put(tiles,new BPRadSprite(151.0F, -10F, new States.ColState(DefSettings.BEEHIVECOLOR.get())));
					rangemaphive.put(name,radshive.get(tiles));
				}else if(name.contains("trough")){
					radstrough.put(tiles, new BPRadSprite(200.0F, -10F, new States.ColState(DefSettings.TROUGHCOLOR.get())));
					System.out.println(DefSettings.TROUGHCOLOR.get());
					rangemaptrough.put(name,radstrough.get(tiles));
				} else {
					if(Config.doubleradius)
						rads.put(tiles, new BPRadSprite(200.0F, -10F, new States.ColState(DefSettings.ANIMALDANGERCOLOR.get())));
					else
						rads.put(tiles, new BPRadSprite(100.0F, -10F,new States.ColState(DefSettings.ANIMALDANGERCOLOR.get())));
					rangemap.put(name, rads.get(tiles));
				}
				}
			}
		}
	    }
	});
    }
    public static boolean hasRange(final String resname) {
    	return (rangemap.containsKey(resname) | rangemaphive.containsKey(resname)| rangemaptrough.containsKey(resname));
    }

    private final BPRadSprite bp;
	private final BPRadSprite bphive;
	private final BPRadSprite bptrough;

    public Range(final Gob g, final String name) {
        super(g);
        bp = rangemap.get(name);
        bphive = rangemaphive.get(name);
		bptrough = rangemaptrough.get(name);
    }

    public void setup(RenderList rl) {
    if((gob.type == Type.ANIMAL && Config.showanimalrad && !gob.isDead())) {
	    rl.add(bp, null);
	}else if(gob.type == Type.BEEHIVE && Config.showBeehiverad){
    	rl.add(bphive,null);
	}else if(gob.type == Type.TROUGH && Config.showTroughrad){
    	rl.add(bptrough,null);
	}
    }
    //used to clear the lists when changing radius colors
    public static void clearAll(){
		rads.clear();
		radshive.clear();
		radstrough.clear();
		rangemap.clear();
		rangemaphive.clear();
		rangemaptrough.clear();
	}
}
