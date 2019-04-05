package haven.purus.pbot;

import haven.*;
import haven.automation.GobSelectCallback;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import static haven.OCache.posres;

public class PBotGobAPI {

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
		synchronized(PBotAPI.gui.ui.sess.glob.oc) {
			for(Gob gob : PBotAPI.gui.ui.sess.glob.oc) {
				list.add(new PBotGob(gob));
			}
		}
		return list;
	}

	/**
	 * Waits for any gob to appear at the precisely given coordinates, excluding player
	 * @param x
	 * @param y
	 */
	public static void waitForGob(double x, double y) {
		Coord2d expected = new Coord2d(x, y);
		while(true) {
			for(PBotGob gob:getAllGobs()) {
				if(gob.getRcCoords().equals(expected) && player().getGobId() != gob.getGobId())
					return;
			}
			PBotUtils.sleep(25);
		}
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
		synchronized (PBotAPI.gui.ui.sess.glob.oc) {
			for (Gob gob : PBotAPI.gui.ui.sess.glob.oc) {
				double dist = gob.rc.dist(plc);
				if (dist < min) {
					boolean matches = false;
					try {
					for (String name : names) {
						if (gob.getres() != null && gob.getres().name.contains(name)) {
							matches = true;
							break;
						}
					}
					} catch(Loading l) {
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
		synchronized (PBotAPI.gui.ui.sess.glob.oc) {
			for (Gob gob : PBotAPI.gui.ui.sess.glob.oc) {
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
		return new PBotGob(PBotAPI.gui.map.player());
	}


	/**
	 * Find object by ID
	 * @param id ID of the object to look for
	 * @return Object, or null if not found
	 */
	public static PBotGob findGobById(long id) {
		Gob gob = PBotAPI.gui.ui.sess.glob.oc.getgob(id);
		if(gob == null)
			return null;
		else
			return new PBotGob(PBotAPI.gui.ui.sess.glob.oc.getgob(id));
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
		PBotAPI.gui.map.unregisterGobSelect();
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
	 * @param x x place stockpile to
	 * @param y y place stockpile to
	 */
	public static void placeThing(double x, double y) {
		PBotAPI.gui.map.wdgmsg("place", new Coord2d(x, y).floor(posres), 0, 1, 0);
	}
}
