package haven.purus.pbot;

import haven.Coord2d;
import haven.GameUI;
import haven.Gob;
import haven.automation.GobSelectCallback;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import static haven.OCache.posres;

public class PBotGobAPI {

	private static GameUI gui = PBotAPI.gui;
	private static boolean gobSelectWait = false;
	private static Gob selectedGob;

	public static HashMap<String, String> gobWindowMap = new HashMap<String, String>() {{
		put("gfx/terobjs/crate", "Crate");
		put("gfx/terobjs/dframe", "Frame");
		put("gfx/terobjs/kiln", "Kiln");
		put("gfx/terobjs/fineryforge", "Finery Forge");
		put("gfx/terobjs/steelcrucible", "Steelbox");
		put("gfx/terobjs/smelter", "Ore Smelter");
		put("gfx/terobjs/pow", "Fireplace");
		put("gfx/terobjs/oven", "Oven");
		put("gfx/terobjs/cauldron", "Cauldron");
		put("gfx/terobjs/woodbox", "Woodbox");
		put("gfx/terobjs/create", "Crate");
		put("gfx/terobjs/furn/table-stone", "Table");
		put("gfx/terobjs/furn/cottagetable", "Table");
		put("gfx/terobjs/wbasket", "Basket");
		put("gfx/terobjs/chickencoop", "Chicken Coop");
		put("gfx/terobjs/htable", "Herbalist Table");
		put("gfx/terobjs/studydesk", "Study Desk");
		put("gfx/terobjs/cupboard", "");
		put("gfx/terobjs/ttub", "Tub");
		put("gfx/terobjs/chest", "Chest");
	}};

	/**
	 * List of all gobs visible to the client
	 * @return List of all gobs
	 */
	public static List<PBotGob> getAllGobs() {
		List<PBotGob> list = new ArrayList<PBotGob>();
		synchronized(gui.ui.sess.glob.oc) {
			for(Gob gob : gui.ui.sess.glob.oc) {
				list.add(new PBotGob(gob));
			}
		}
		return list;
	}

	/**
	 * Finds the closest object that matches one of the given names
	 * @param radius Radius to look for objects in tiles
	 * @param names Name(s) of gobs to look for
	 * @return Gob of the object, or null if not found
	 */
	public static PBotGob findGobByNames(double radius, String... names) {
		Coord2d plc = player().getRcCoords();
		double min = radius;
		Gob nearest = null;
		synchronized (gui.ui.sess.glob.oc) {
			for (Gob gob : gui.ui.sess.glob.oc) {
				double dist = gob.rc.dist(plc);
				if (dist < min) {
					boolean matches = false;
					for (String name : names) {
						if (gob.getres() != null && gob.getres().name.contains(name)) {
							matches = true;
							break;
						}
					}
					if (matches) {
						min = dist;
						nearest = gob;
					}
				}
			}
		}
		if(nearest == null)
			return null;
		else
			return new PBotGob(nearest);
	}

	/**
	 * Returns gob with exactly the given coords or null if not found
	 * @param c Coords of gob
	 * @return Gob with coordinates or null
	 */
	public static PBotGob getGobWithCoords(Coord2d c) {
		synchronized (gui.ui.sess.glob.oc) {
			for (Gob gob : gui.ui.sess.glob.oc) {
				if(gob.rc.x == c.x && gob.rc.y == c.y)
					return new PBotGob(gob);
			}
		}
		return null;
	}

	/**
	 * Returns the player gob
	 * @return Player gob
	 */
	public static PBotGob player() {
		return new PBotGob(gui.map.player());
	}


	/**
	 * Find object by ID
	 * @param id ID of the object to look for
	 * @return Object, or null if not found
	 */
	public static PBotGob findGobById(long id) {
		Gob gob = gui.ui.sess.glob.oc.getgob(id);
		if(gob == null)
			return null;
		else
			return new PBotGob(gui.ui.sess.glob.oc.getgob(id));
	}


	/**
	 * Next alt+click to gob returns the gob, the function will wait until this happens
	 */
	public static PBotGob selectGob() {
		gobSelectWait = true;
		synchronized (GobSelectCallback.class) {
			PBotAPI.gui.map.registerGobSelect(new GobCb());
		}
		while(gobSelectWait) {
			PBotUtils.sleep(25);
		}
		gui.map.unregisterGobSelect();
		return new PBotGob(selectedGob);
	}

	private static class GobCb implements GobSelectCallback {

		public GobCb() {
		}

		@Override
		public void gobselect(Gob gob) {
			selectedGob = gob;
			gobSelectWait = false;
		}
	}

	/**
	 * Itemact with item in hand, for example, to make a stockpile
	 */
	public static void makePile() {
		PBotAPI.gui.map.wdgmsg("itemact", PBotUtils.getCenterScreenCoord(), player().getRcCoords().floor(posres), 0);
	}

	/**
	 * Use to place something, for example, a stockpile
	 * 11 offset = 1 tile
	 * @param x Offset from player to place stockpile to
	 * @param y Offset from player to place stockpile to
	 */
	public static void placeThing(int x, int y) {
		gui.map.wdgmsg("place", player().getRcCoords().add(x, y).floor(posres), 0, 1, 0);
	}
}
