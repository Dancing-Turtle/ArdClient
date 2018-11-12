package haven.purus.pbot;

import static haven.OCache.posres;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import haven.Coord;
import haven.Coord2d;
import haven.FlowerMenu;
import haven.GItem;
import haven.GameUI;
import haven.Gob;
import haven.Inventory;
import haven.ItemInfo;
import haven.Loading;
import haven.MCache;
import haven.Makewindow;
import haven.ResDrawable;
import haven.WItem;
import haven.Widget;
import haven.Window;
import haven.purus.BotUtils;

public class PBotAPI {
	
	public static GameUI gui;
	
	private static Coord selectedAreaA, selectedAreaB;
	
	/**
	 * Send a system message to the user
	 * @param str Message to send
	 * @param col Color of the text
	 */
	public static void sysMsg(String str, Color col) {
		gui.msg(str, col);
	}
	
	/**
	 * Send a system message to the user
	 * @param str Message to send
	 * @param r Amount of red colour in the text
	 * @param g Amount of green colour in the text
	 * @param b Amount of blue colour in the text
	 */
	public static void sysMsg(String str, int r, int g, int b) {
		gui.msg(str, new Color(r, g, b));
	}
	/* Get tile type
         * @param x X-Coordinate
         * @param y Y-Coordinate
         * @return Tile type (integer)
         */
	public static int getTile(int x, int y) {
		return gui.ui.sess.glob.map.gettile(new Coord(x, y));
	}

       /* Teleport to you hearthfire
         */
	public static void travelHearth() {
		gui.act("travel", "hearth");
	}
	/**
	 * Click an object
	 * @param gob Gob to click
	 * @param button 1 = Left click, 3 = Right click
	 * @param mod 1 = shift, 2 = ctrl, 4 = alt
	 */
	public static void doClick(Gob gob, int button, int mod) {
		gui.map.wdgmsg("click", Coord.z, gob.rc.floor(posres), button, 0, mod, (int) gob.id, gob.rc.floor(posres), 0,
				-1);
	}
	
	/**
	 * Get an amount of something such as seeds in a stack
	 * @param item Item to check
	 * @return Amount of something in the item
	 */
	public static int getAmount(WItem item) {
		int ret = -1;
		for(ItemInfo o:item.item.info()) {
			if(o instanceof GItem.Amount)
				ret = ((GItem.Amount) o).itemnum();
		}
		return ret;
	}
	
	/**
	 * Finds the closest object that matches one of the given names
	 * @param radius Radius to look for objects in tiles
	 * @param names Name(s) of gobs to look for
	 * @return Gob of the object, or null if not found
	 */
	public static Gob findObjectByNames(double radius, String... names) {
		Coord2d plc = player().rc;
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
		return nearest;
	}

	/**
	 * Find object by ID
	 * @param id ID of the object to look for
	 * @return Object, or null if not found
	 */
	public static Gob findObjectById(long id) {
		return gui.ui.sess.glob.oc.getgob(id);
	}
	
	/**
	 * Get object id
	 * @param gob Gob to get id of
	 * @return Id of the gob
	 */
	public static long getGobId(Gob gob) {
		return gob.id;
	}
	
	/**
	 * Returns the name of the gobs resource, or null if not found
	 * @param gob Gob to get name of
	 * @return Name of the gob
	 */
	public static String getGobName(Gob gob) {
		if(gob.getres() != null)
			return gob.getres().name;
		else
			return null;
	}
	
	/**
	 * Get the player stamina
	 * @return Returns 0-100
	 */
	public static int getStamina() {
		return gui.getmeter("stam", 0).a;
	}
	
	/**
	 * Get the player energy
	 * @return Returns 0-100
	 */
	public static int getEnergy() {
		return gui.getmeter("nrj", 0).a;
	}
	
	/**
	 * Get the player hp
	 * @return Returns 0-100
	 */
	public static int getShp() {
		return gui.getmeter("hp", 0).a;
	}

	/**
	 * Check if the object is moving
	 * @param gob Gob to check moving of
	 * @return Returns true if gob is moving, false otherwise
	 */
	public static boolean isMoving(Gob gob) {
		if (gob.getv() == 0)
			return false;
		else
			return true;
	}

	/**
	 * Right click a gob with pathfinder
	 * @param gob Gob to right click
	 * @param mod 1 = shift, 2 = ctrl, 4 = alt
	 */
	public static void pfRightClick(Gob gob, int mod) {
		gui.map.pfRightClick(gob, -1, 3, mod, null);
	}
	
	/**
	 * Left click to somewhere with pathfinder
	 * @param x X-Coordinat
	 * @param y Y-Coordinate
	 */
	public static void pfLeftClick(int x, int y) {
		gui.map.pfLeftClick(new Coord(x, y), null);
	}
	
	/**
	 * Chooses a petal with given label from a flower menu that is currently open
	 * @param name Name of petal to open
	 */
	public static void choosePetal(String name) {
		FlowerMenu menu = gui.ui.root.findchild(FlowerMenu.class);
		if (menu != null) {
			for (FlowerMenu.Petal opt : menu.opts) {
				if (opt.name.equals(name)) {
					menu.choose(opt);
					menu.destroy();
				}
			}
		}	
	}

	/**
	 * Itemact with gob, to fill trough with item in hand for example
	 * @param gob Gob to interact with
	 * @param mod 1 = shift, 2 = ctrl, 4 = alt
	 */
	public static void itemClick(Gob gob, int mod) {
		gui.map.wdgmsg("itemact", Coord.z, gob.rc.floor(posres), mod, 0, (int) gob.id, gob.rc.floor(posres), 0, -1);
	}

	/**
	 * Returns the player gob
	 * @return Player gob
	 */
	public static Gob player() {
		return gui.map.player();
	}

	/**
	 * Returns contents of an item
	 * @param item WItem to return contents of
	 * @return Contents of the item
	 */
	public static ItemInfo.Contents getContents(WItem item) {
		try {
			for (ItemInfo info : item.item.info())
				if (info instanceof ItemInfo.Contents)
					return (ItemInfo.Contents) info;
		} catch (Loading ignored) {
		}
		return null;
	}

	/**
	 * Returns the players inventory
	 * @return Inventory of the player
	 */
	public static Inventory playerInventory() {
		return gui.maininv;
	}
	
	/**
	 * Tries to find inventory attached to the given window, such as cupboard
	 * @param window Window to look inventory from
	 * @return Inventory of the window or null if not found
	 */
	public static Inventory getInventory(Window window) {
		for(Widget wdg = window.lchild; wdg!=null; wdg = wdg.prev) {
			if(wdg instanceof Inventory) {
				return (Inventory) wdg;
			}
		}
		return null;
	}

	/**
	 * Take an item to hand
	 * @param item Item to take to hand
	 */
	public static void takeItem(WItem item) {
		item.item.wdgmsg("take", Coord.z);
		while (getItemAtHand() == null) {
			sleep(50);
		}
	}

	/**
	 * Returns the item currently in the hand
	 * @return Item at hand
	 */
	public static GItem getItemAtHand() {
		for (GameUI.DraggedItem item : gui.hand)
			return item.item;
		return null;
	}

	/**
	 * Sleep for t milliseconds
	 * @param t Time to wait in milliseconds
	 */
	public static void sleep(int t) {
		try {
			Thread.sleep(t);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Get stage of the crop
	 * @param gob Crop to get stage of
	 * @return Stage of the crop
	 */
	public static int getCropStage(Gob gob) {
		return gob.getStage();
	}
	
	/**
	 * Click some place on map
	 * @param x x-coordinate
	 * @param y y-coordinate
	 * @param btn 1 = left click, 3 = right click
	 * @param mod 1 = shift, 2 = ctrl, 4 = alt
	 */
	public static void mapClick(int x, int y, int btn, int mod) {
		gui.map.wdgmsg("click", getCenterScreenCoord(), new Coord2d(x, y).floor(posres), btn, mod);
	}

	/**
	 * Use item in hand to ground below player, for example, to plant carrot
	 */
	public static void mapInteractClick() {
		gui.map.wdgmsg("itemact", getCenterScreenCoord(), player().rc.floor(posres), 3, gui.ui.modflags());
	}
	
	/**
	 * Destroys the given gob
	 * @param gob Gob to destroy
	 */
	public static void liftGob(Gob gob) {
		gui.menu.wdgmsg("act", new Object[] { "carry" });
		doClick(gob, 1, 0);
	}

	public static void destroyGob(Gob gob) {
		gui.menu.wdgmsg("act", new Object[] { "destroy" });
		doClick(gob, 1, 0);
	}
	
	/**
	 * Drops an item from the hand
	 * @param mod 1 = shift, 2 = ctrl, 4 = alt
	 */
	public static void dropItem(int mod) {
		gui.map.wdgmsg("drop", Coord.z, gui.map.player().rc.floor(posres), mod);
	}
	
	/**
	 * Drops the given item from the inventory
	 * @param item Item to drop
	 */
	public static void dropItemFromInventory(WItem item) {
		item.item.wdgmsg("drop", Coord.z);
	}

	/**
	 * Coordinates of the center of the screen
	 * @return Coordinates of the center of the screen
	 */
	public static Coord getCenterScreenCoord() {
		Coord sc, sz;
		sz = gui.map.sz;
		sc = new Coord((int) Math.round(Math.random() * 200 + sz.x / 2 - 100),
				(int) Math.round(Math.random() * 200 + sz.y / 2 - 100));
		return sc;
	}

	/**
	 * Wait for a window with a specific name to appear
	 * @param windowName Name of the window
	 */
	public static void waitForWindow(String windowName) {
		while (gui.getwnd(windowName) == null) {
			sleep(50);
		}
	}
	
	/**
	 * Get a window with name
	 * @param name Name of the window
	 * @return The window or null if not found
	 */
	public static Window getWindow(String name) {
		return gui.getwnd(name);
	}
	
	/**
	 * Close the window
	 * @param window The window to close
	 */
	public static void closeWindow(Window window) {
		window.reqdestroy();
	}

	/**
	 * Returns list of items with specific name(s) from the given inventory
	 * @param invwdg Inventory to look items from
	 * @param items List of names of the items
	 * @return List of items found
	 */
	public static List<WItem> getInventoryItemsByNames(Inventory invwdg, List<String> items) {
		List<WItem> witems = new ArrayList<WItem>();
		for (WItem wi : getInventoryContents(invwdg)) {
			String resname = wi.item.resource().name;
			for (String s : items) {
				if (resname.contains(s))
					witems.add(wi);
			}
		}
		return witems;
	}
	
	/**
	 * Returns item with the specific name, or null if not found
	 * @param invwdg Inventory to look items from
	 * @param items Name(s) of the items to look for
	 * @return Item, or null if not found
	 */
	public static WItem getInventoryItemByNames(Inventory invwdg, List<String> items) {
		for (WItem wi : getInventoryContents(invwdg)) {
			String resname = wi.item.resource().name;
			for (String s : items) {
				if (resname.contains(s))
					return wi;
			}
		}
		return null;
	}
	
	/**
	 * Return all items that the given inventory contains
	 * @param invwdg Inventory of which items to return
	 * @return List of items in the inventory
	 */
	public static List<WItem> getInventoryContents(Inventory invwdg) {
		List<WItem> witems = new ArrayList<WItem>();
		for (Widget witm = invwdg.lchild; witm != null; witm = witm.prev) {
			if (witm instanceof WItem) {
				witems.add((WItem) witm);
			}
		}
		return witems;
	}

	/**
	 * Log out to character selection
	 */
	public static void logoutChar() {
		gui.act("lo", "cs");
	}
	
	/**
	 * Check if stockpile is full
	 * @param gob Stockpile gob to check
	 * @return True if stockpile is full, else false
	 */
	public static boolean stockpileIsFull(Gob gob) {
		if(gob.getattr(ResDrawable.class).sdt.peekrbuf(0)==31)
			return true;
		else
			return false;	
	}
	
	/**
	 * Starts crafting item with the given name
	 * @param name Name of the item ie. "clogs"
	 * @param makeAll 0 To craft once, 1 to craft all
	 */
	public static void craftItem(String name, int makeAll) {
		gui.wdgmsg("act", "craft", name);
		waitForWindow("Crafting");
		loop:
		while(true) {
	        for(Widget w : gui.ui.widgets.values()) {
	            if (w instanceof Makewindow) {
            		gui.wdgmsg(w, "make", makeAll);
            		break loop;
	            }
	        }
	       sleep(100);
		}
	}
	
	/**
	 * Waits for flower menu to appear
	 */
	public static void waitForFlowerMenu() {
		while (gui.ui.root.findchild(FlowerMenu.class) == null) {
			BotUtils.sleep(50);
		}
	}

	/**
	 * Amount of free slots in the inventory
	 * @param inventory Inventory to check
	 * @return Amount of free inventory slots
	 */
	public static int freeSlotsInv(Inventory inventory) {
		int takenSlots = 0;
		for (Widget i = inventory.child; i != null; i = i.next) {
			if (i instanceof WItem) {
				WItem buf = (WItem) i;
				takenSlots += buf.size().x * buf.size().y;
			}
		}
		int allSlots = inventory.isz.x * inventory.isz.y;
		return allSlots - takenSlots;
	}
	
	/**
	 * Activate area selection by dragging.
	 * To get coordinates of the area selected, use getSelectedAreaA and getSelectedAreaB
	 * User can select an area by dragging
	 */
	public static void selectArea() {
		//sysMsg("Please select an area by dragging!", Color.ORANGE);
		gui.map.PBotAPISelect = true;
		while(gui.map.PBotAPISelect)
			sleep(100);
	}
	
	/**
	 * Return A point of the selected area
	 * @return Returns coord of A point or null if not selected
	 */
	public static Coord getSelectedAreaA() {
		return selectedAreaA;
	}
	
	/**
	 * Return B point of the selected area
	 * @return Returns coord of B point or null if not selected
	 */
	public static Coord getSelectedAreaB() {
		return selectedAreaB;
	}
	/**
	 * Checks if player can drink from the given inventory
	 * Only checks water and tea
	 * @param item Item to check
	 * @return True if player can drink, else false
	 */
	public static boolean canDrinkFrom(WItem item) {
		Pattern liquidPattern = Pattern.compile(String.format("[0-9.]+ l of (%s)",
			//	String.join("|", new String[] { "Water", "Piping Hot Tea", "Tea" }), Pattern.CASE_INSENSITIVE));
		String.join("|", new String[] { "Water", "Piping Hot Tea", "Tea" , "Milk" , "Cowsmilk", "Sheepsmilk", "Goatsmilk"}), Pattern.CASE_INSENSITIVE));
		ItemInfo.Contents contents = getContents(item);
		if (contents != null && contents.sub != null) {
			for (ItemInfo info : contents.sub) {
				if (info instanceof ItemInfo.Name) {
					ItemInfo.Name name = (ItemInfo.Name) info;
					if (name.str != null && liquidPattern.matcher(name.str.text).matches())
						return true;
				}
			}
		}
		return false;
	}
	
	/**
	 * Right clicks item in the inventory
	 * @param item Item to activate
	 */
	public static void activateItem(WItem item) {
		item.item.wdgmsg("iact", Coord.z, 3);
	}
	
	/**
	 * Waits for the hourglass timer when crafting or drinking for example
	 * Also waits until the hourglass has been seen to change at least once
	 */
	public static void waitForHourglass() {
		double prog = gui.prog;
		while (prog == gui.prog) {
			prog = gui.prog;
			sleep(5);
		}
		while (gui.prog >= 0) {
			sleep(50);
		}
	}
	
	/**
	 * List of all gobs visible to the client
	 * @return List of all gobs
	 */
	public static List<Gob> getGobs() {
		List<Gob> list = new ArrayList<Gob>();
		for(Gob gob : gui.ui.sess.glob.oc) {
			list.add(gob);
		}
		return list;
	}
	
	/**
	 * Create a PBotWindow object, See PBotWindow for usage
	 * @param title Title of the window
	 * @param height Height of the window
	 * @param width Width of the window
	 * @param id scriptID variable of the script
	 * @return PBotWindow object
	 */
	public static PBotWindow PBotWindow(String title, int height, int width, String id) {
		PBotWindow window = new PBotWindow(new Coord(width, height), title, id);
		gui.add(window, 300, 300);
		return window;
	}
	
	/**
	 * Send act message to server
	 * Act can be used for example to choose a cursor
	 * Some acts:
	 * dig, mine, carry, destroy, fish, inspect, repair, crime, swim, tracking, aggro, shoot
	 * @param act Act to choose
	 */
	public static void doAct(String act) {
		gui.menu.wdgmsg("act", act);
	}
	
	/**
	 * Returns coords of the gob
	 * @param gob Gob to return coordinates of
	 * @return Coords of the gob
	 */
	public static Coord2d getCoords(Gob gob) {
		return gob.rc;
	}
	
	/**
	 * Transfer an item to the active inventory
	 * @param item Item to transfer
	 */
	public static void transferItem(WItem item) {
		item.item.wdgmsg("transfer", Coord.z);
	}
	
	public static void areaSelect(Coord a, Coord b) {
		selectedAreaA = a.mul(MCache.tilesz2);
		selectedAreaB = b.mul(MCache.tilesz2).add(11, 11);
		//sysMsg("Area selected!", Color.ORANGE);
	}
}
