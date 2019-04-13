package haven.purus.pbot;

import haven.*;
import haven.purus.gobText;

import java.util.ArrayList;

import static haven.OCache.posres;

public class PBotGob {

	public Gob gob;

	PBotGob(Gob gob) {
		this.gob = gob;
	}

	/**
	 * Check if stockpile is full
	 * @return True if stockpile is full, else false
	 */
	public boolean stockpileIsFull() {
		if(gob.sdt()==31)
			return true;
		else
			return false;
	}


	/**
	 * Itemact with gob, to fill trough with item in hand for example
	 * @param mod 1 = shift, 2 = ctrl, 4 = alt
	 */
	public void itemClick(int mod) {
		PBotAPI.gui.map.wdgmsg("itemact", Coord.z, gob.rc.floor(posres), mod, 0, (int) gob.id, gob.rc.floor(posres), 0, -1);
	}

	/**
	 * Add cool hovering text above gob
	 * @param text text to add
	 * @param height height that the hext hovers at
	 */
	public void addGobText(String text, int height) {
		gob.addol(new gobText(text, height));
	}

	/**
	 * Click the gob
	 * @param button 1 = Left click, 3 = Right click
	 * @param mod 0 = no modifier, 1 = shift, 2 = ctrl, 4 = alt
	 */
	public void doClick(int button, int mod) {
		PBotAPI.gui.map.wdgmsg("click", Coord.z, gob.rc.floor(posres), button, 0, mod, (int) gob.id, gob.rc.floor(posres), 0,
				-1);
	}

	/**
	 * Get id of the gob
	 * @return Id of the gob
	 */
	public long getGobId() {
		return gob.id;
	}

	/**
	 * Get stage of the crop
	 * @return Stage of the crop
	 */
	public int getCropStage() {
		return gob.getStage();
	}

	/**
	 * Returns the name of the gobs resource file, or null if not found
	 * @return Name of the gob
	 */
	public String getResname() {
		try {
		if(gob.getres() != null)
			return gob.getres().name;
		} catch(Loading l) {
		}
			return null;
	}

	/**
	 * Get name of window for gob from gobWindowMap
	 * @param gob Gob to get inventory of
	 * @return Inventory window name
	 */
	public static String windowNameForGob(Gob gob) {
		if(gob.getres() == null)
			return "Window name for gob found!!";
		else
			return (String)PBotGobAPI.gobWindowMap.get(gob.getres().name);
	}

	/**
	 * Returns rc-coords of the gob
	 * @return Coords of the gob
	 */
	public Coord2d getRcCoords() {
		return gob.rc;
	}

	/**
	 * Highlights the gob by Alt+Clicking it
	 */
	public void highlightGob() {
		doClick(0, 4);
	}

	/**
	 * Right click a gob with pathfinder, wait until pathfinder is finished
	 * @param btn 1 = left click, 3 = right click
	 * @param mod 1 = shift, 2 = ctrl, 4 = alt
	 */
	public void pfClick(int btn, int mod) {
		PBotAPI.gui.map.purusPfRightClick(this.gob, -1, btn, mod, null);
		try {
			PBotAPI.gui.map.pastaPathfinder.join();
		} catch(InterruptedException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Check if the object is moving
	 * @return Returns true if gob is moving, false otherwise
	 */
	public boolean isMoving() {
		if (gob.getv() == 0)
			return false;
		else
			return true;
	}

	/**
	 * Returns resnames of poses of this gob if it has any poses
	 * @return Resnames of poses
	 */
	public ArrayList<String> getPoses() {
		ArrayList<String> ret = new ArrayList<>();
		Drawable d = gob.getattr(Drawable.class);

		if(d instanceof Composite) {
			Composite comp = (Composite)d;
			for(ResData rd:comp.prevposes) {
				try {
					ret.add(rd.res.get().name);
				} catch(Loading l) {

				}
			}
		}
		return ret;
	}

	/**
	 * Repeat shift + itemact until all similar items from the inventory have been clicked insinde trough etc.
	 * May result in valuable items accidentally being put in troughs etc...
	 */
	public void itemClickAll() {
		Object[] args = {Coord.z, gob.rc.floor(posres), 1, 0, (int) gob.id, gob.rc.floor(posres), 0, -1};
		PBotAPI.gui.map.lastItemactClickArgs = args;
		PBotAPI.gui.map.wdgmsg("itemact", Coord.z, gob.rc.floor(posres), 1, 0, (int) gob.id, gob.rc.floor(posres), 0, -1);
	}
}
