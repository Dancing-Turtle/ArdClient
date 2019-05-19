package haven.purus.pbot;

import haven.*;
import haven.Button;
import haven.Window;
import haven.automation.Discord;
import haven.purus.DrinkWater;
import haven.purus.ItemClickCallback;
import haven.purus.pbot.gui.PBotWindow;
import net.dv8tion.jda.core.entities.TextChannel;

import java.awt.*;
import java.lang.ref.WeakReference;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Pattern;

import static haven.OCache.posres;

public class PBotUtils {

	private static Coord selectedAreaA, selectedAreaB;
	private static boolean itemSelectWait;
	private static PBotItem selectedItem;


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
	 * Right click a gob with pathfinder, wait until pathfinder is finished
	 * @param gob Gob to right click
	 * @param mod 1 = shift, 2 = ctrl, 4 = alt
	 * @return False if path was not found, true if it was found
	 */
	public static boolean pfRightClick(PBotGob gob, int mod) {
		PBotAPI.gui.map.purusPfRightClick(gob.gob, -1, 3, mod, null);
		try {
			PBotAPI.gui.map.pastaPathfinder.join();
		} catch(InterruptedException e) {
			e.printStackTrace();
		}
		synchronized(PBotAPI.gui.map) {
			return PBotAPI.gui.map.foundPath;
		}
	}
	//new boshaw pf right clicks.
	public static void PathfinderRightClick(Gob gob, int mod){
		PBotAPI.gui.map.pathtoRightClick(gob, mod);
	}


	/**
	 * Chooses a petal with given label from a flower menu that is currently open
	 * @param name Name of petal to open
	 * @return False if petal or flower menu with name could not be found
	 */
	public static boolean choosePetal(String name) {
		FlowerMenu menu = PBotAPI.gui.ui.root.findchild(FlowerMenu.class);
		if(menu != null) {
			for(FlowerMenu.Petal opt : menu.opts) {
				if(opt.name.equals(name)) {
					menu.choose(opt);
					menu.destroy();
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Closes flowermenu, if it is open
	 */
	public static void waitFlowermenuClose() {
		while(PBotAPI.gui.ui.root.findchild(FlowerMenu.class) != null)
			sleep(25);
	}

	/**
	 * Click some place on map
	 * @param x x-coordinate
	 * @param y y-coordinate
	 * @param btn 1 = left click, 3 = right click
	 * @param mod 1 = shift, 2 = ctrl, 4 = alt
	 */
	public static void mapClick(int x, int y, int btn, int mod) {
		PBotAPI.gui.map.wdgmsg("click", getCenterScreenCoord(), new Coord2d(x, y).floor(posres), btn, mod);
	}

	/**
	 * Use item in hand to ground below player, for example, to plant carrot
	 */
/*	public static void mapInteractClick() {
		gui.map.wdgmsg("itemact", PBotUtils.getCenterScreenCoord(), PBotGobAPI.player().getRcCoords().floor(posres), 3, gui.ui.modflags());
	}*/


	/**
	 * Coordinates of the center of the screen
	 * @return Coordinates of the center of the screen
	 */
	public static Coord getCenterScreenCoord() {
		Coord sc, sz;
		sz = PBotAPI.gui.map.sz;
		sc = new Coord((int) Math.round(Math.random() * 200 + sz.x / 2 - 100),
				(int) Math.round(Math.random() * 200 + sz.y / 2 - 100));
		return sc;
	}


	/**
	 * Left click to somewhere with pathfinder, wait until pathfinder is finished
	 * @param x X-Coordinate
	 * @param y Y-Coordinate
	 * @return False if path was not found, true if it was found
	 */
	public static boolean pfLeftClick(double x, double y) {
		PBotAPI.gui.map.purusPfLeftClick(new Coord2d(x, y), null);
		try {
			PBotAPI.gui.map.pastaPathfinder.join();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		synchronized(PBotAPI.gui.map) {
			return PBotAPI.gui.map.foundPath;
		}
	}

	/**
	 * Starts crafting item with the given name
	 * @param name Name of the item ie. "clogs"
	 * @param makeAll 0 To craft once, 1 to craft all
	 */
	public static void craftItem(String name, int makeAll) {
		openCraftingWnd(name);
		loop:
		while(true) {
			for(Widget w : PBotAPI.gui.ui.widgets.values()) {
				if (w instanceof Makewindow) {
					PBotAPI.gui.wdgmsg(w, "make", makeAll);
					break loop;
				}
			}
			sleep(25);
		}
	}

	/**
	 * Waits for flower menu to appear
	 */
	public static void waitForFlowerMenu() {
		while(PBotAPI.gui.ui.root.findchild(FlowerMenu.class) == null) {
			sleep(15);
		}
	}
	/**
	 * Waits for the flower menu to disappear
	 */
	public static void closeFlowermenu() {
		FlowerMenu menu = PBotAPI.gui.ui.root.findchild(FlowerMenu.class);
		if(menu != null) {
			menu.choose(null);
			menu.destroy();
		}
		while(PBotAPI.gui.ui.root.findchild(FlowerMenu.class) != null) {
			sleep(15);
		}
	}
	/**
	 * Waits for the hourglass timer when crafting or drinking for example
	 * Also waits until the hourglass has been seen to change at least once
	 */
	public static void waitForHourglass() {
		double prog = PBotAPI.gui.prog;
		while (prog == PBotAPI.gui.prog) {
			prog = PBotAPI.gui.prog;
			sleep(5);
		}
		while (PBotAPI.gui.prog >= 0) {
			sleep(50);
		}
	}

	/**
	 * Waits for the hourglass timer when crafting or drinking for example
	 * Also waits until the hourglass has been seen to change at least once
	 * If hourglass does not appear within timeout, returns false, else true
	 * @param timeout Timeout in milliseconds
	 */
	public static boolean waitForHourglass(int timeout) {
		double prog = PBotAPI.gui.prog;
		int retries = 0;
		while(prog == PBotAPI.gui.prog) {
			if(retries > timeout/5)
				return false;
			retries++;
			prog = PBotAPI.gui.prog;
			sleep(5);
		}
		while (PBotAPI.gui.prog >= 0) {
			sleep(25);
		}
		return true;
	}
	//will send a message to the selected channel with message text if discord is connected ingame.
	public static void sendDiscordMsg(String channel, String text){
		if(Discord.jdalogin!=null){
			GameUI gui = PBotAPI.gui;
			for(TextChannel loop:haven.automation.Discord.channels)
				if (loop.getName().equals(channel)) {
					loop.sendMessage(gui.getparent(GameUI.class).buddies.getCharName() + ": " + text).queue();
					break;
				}
		}
	}

	/**
	 * Returns value of hourglass, -1 = no hourglass, else the value between 0.0 and 1.0
	 * @return value of hourglass
	 */
	public static double getHourglass() {
		return PBotAPI.gui.prog;
	}

	// TODO: Return false if drinking was not successful (no water found for example)
	/**
	 * Attempts to drink water by using the same water drinking script as in extensions
	 * @param wait Wait for the drinking to finish
	 */
	public static boolean drink(boolean wait) {
		if(!PBotAPI.gui.drinkingWater) {
			Thread t = new Thread(new DrinkWater(PBotAPI.gui));
			t.start();
			if(wait) {
				try {
					t.join();
					if(!PBotAPI.gui.lastDrinkingSucessful) {
						sysMsg("PBotUtils Warning: Couldn't drink, didn't find anything to drink!", Color.ORANGE);
						return false;
					}
				} catch(InterruptedException e) {
					e.printStackTrace();
		}
				waitForHourglass();
			}
		}
		return true;
	}

	/**
	 * Opens the crafting window for given item
	 * @param name Name of craft for wdgmsg
	 */
	public static void openCraftingWnd(String name) {
		// Close current window and wait for it to close
		Window wnd = PBotWindowAPI.getWindow("Crafting");
		if(wnd != null)
			PBotWindowAPI.closeWindow(wnd);
		PBotWindowAPI.waitForWindowClose("Crafting");
		PBotAPI.gui.wdgmsg("act", "craft", name);
		PBotWindowAPI.waitForWindow("Crafting");
	}

	/**
	 * Send a system message to the user
	 * @param str Message to send
	 */
	public static void sysMsg(String str) {
		PBotAPI.gui.msg(str, Color.WHITE);
	}

	/**
	 * Send a system message to the user
	 * @param str Message to send
	 * @param col Color of the text
	 */
	public static void sysMsg(String str, Color col) {
		PBotAPI.gui.msg(str, col);
	}

	/**
	 * Send a system message to the user
	 * @param str Message to send
	 * @param r Amount of red colour in the text
	 * @param g Amount of green colour in the text
	 * @param b Amount of blue colour in the text
	 */
	public static void sysMsg(String str, int r, int g, int b) {
		PBotAPI.gui.msg(str, new Color(r, g, b));
	}

	/**
	 * Returns the players inventory
	 * @return Inventory of the player
	 */
	public static PBotInventory playerInventory() {
		return new PBotInventory(PBotAPI.gui.maininv);
	}

	/**
	 * Returns all open inventories
	 * @return List of inventories
	 */
	public static ArrayList<PBotInventory> getAllInventories() {
		ArrayList<PBotInventory> ret = new ArrayList<>();
		for(Widget window = PBotAPI.gui.lchild; window != null; window = window.prev) {
			if(window instanceof Window) {
				for(Widget wdg = window.lchild; wdg != null; wdg = wdg.prev) {
					if(wdg instanceof Inventory) {
						ret.add(new PBotInventory((Inventory) wdg));
					}
				}
			}
		}
		return ret;
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
		PBotAPI.gui.add(window, 300, 300);
		return window;
	}

	/**
	 * Returns the item currently in the hand
	 * @return Item at hand
	 */
	public static PBotItem getItemAtHand() {
		if(PBotAPI.gui.vhand == null)
			return null;
		else
			return new PBotItem(PBotAPI.gui.vhand);
	}


	/**
	 * Returns the item currently in the hand
	 * @return Item at hand
	 */
	public static GItem getGItemAtHand() {
		for (GameUI.DraggedItem item : PBotAPI.gui.hand)
			return item.item;
		return null;
	}


	/**
	 * Drops an item from the hand and waits until it has been dropped
	 * @param mod 1 = shift, 2 = ctrl, 4 = alt
	 */
	public static void dropItemFromHand(int mod) {
		PBotAPI.gui.map.wdgmsg("drop", Coord.z, PBotAPI.gui.map.player().rc.floor(posres), mod);
		while(getItemAtHand() != null)
			sleep(25);
	}

	/**
	 * Activate area selection by dragging.
	 * To get coordinates of the area selected, use getSelectedAreaA and getSelectedAreaB
	 * User can select an area by dragging
	 */
	public static void selectArea() {
		sysMsg("Please select an area by dragging!", Color.ORANGE);
		PBotAPI.gui.map.PBotAPISelect = true;
		while(PBotAPI.gui.map.PBotAPISelect) {
			sleep(25);
		}
	}

	/**
	 * Next click to item in inventory returns the item, the function will wait until this happens
	 */
	public static PBotItem selectItem() {
		synchronized (ItemClickCallback.class) {
			PBotAPI.gui.registerItemCallback(new ItemCb());
		}
		while(itemSelectWait) {
			PBotUtils.sleep(25);
		}
		synchronized(ItemClickCallback.class) {
			PBotAPI.gui.unregisterItemCallback();
		}
		return selectedItem;
	}

	private static class ItemCb implements ItemClickCallback {

		public ItemCb() {
			itemSelectWait = true;
		}

		@Override
		public void itemClick(WItem item) {
			selectedItem = new PBotItem(item);
			itemSelectWait = false;
		}
	}

	/**
	 * Get A point of the rectangle selected with selectArea()
	 * @return A-Point of the rectangle
	 */
	public static Coord getSelectedAreaA() {
		return selectedAreaA;
	}

	/**
	 * Get B point of the rectangle selected with selectArea()
	 * @return B-Point of the rectangle
	 */
	public static Coord getSelectedAreaB() {
		return selectedAreaB;
	}

	/**
	 * Callback for area select
	 */
	public static void areaSelect(Coord a, Coord b) {
		selectedAreaA = a.mul(MCache.tilesz2);
		selectedAreaB = b.mul(MCache.tilesz2).add(11, 11);
		sysMsg("Area selected!", Color.ORANGE);
	}

	/**
	 * Returns a list of gobs in the rectangle between A and B points
	 * @param a A-point of the rectangle
	 * @param b B-point of the rectangle
	 * @return List of gobs in the area, sorted to zig-zag pattern
	 */
	public static ArrayList<PBotGob> gobsInArea(Coord a, Coord b) {
		// Initializes list of crops to harvest between the selected coordinates
		ArrayList<PBotGob> gobs = new ArrayList<PBotGob>();
		double bigX = a.x > b.x ? a.x : b.x;
		double smallX = a.x < b.x ? a.x : b.x;
		double bigY = a.y > b.y ? a.y : b.y;
		double smallY = a.y < b.y ? a.y : b.y;
		synchronized(PBotAPI.gui.ui.sess.glob.oc) {
			for(Gob gob : PBotAPI.gui.ui.sess.glob.oc) {
				if(gob.rc.x <= bigX && gob.rc.x >= smallX && gob.getres() != null && gob.rc.y <= bigY
						&& gob.rc.y >= smallY) {
					gobs.add(new PBotGob(gob));
				}
			}
		}
		gobs.sort(new CoordSort());
		return gobs;
	}

	/**
	 * Resource name of the tile in the given location
	 * @param x X-Coord of the location (rc coord)
	 * @param y Y-Coord of the location (rc coord)
	 * @return
	 */
	public static String tileResnameAt(int x, int y) {
		try {
			Coord loc = new Coord(x, y);
			int t = PBotAPI.gui.map.glob.map.gettile(loc.div(11));
			Resource res = PBotAPI.gui.map.glob.map.tilesetr(t);
			if(res != null)
				return res.name;
			else
				return null;
		} catch(Loading l) {

		}
		return null;
	}

	// Sorts coordinate array to efficient zig-zag-like sequence for farming etc.
	private static class CoordSort implements Comparator<PBotGob> {
		public int compare(PBotGob a, PBotGob b) {
			if (a.gob.rc.floor().x == b.gob.rc.floor().x) {
				if (a.gob.rc.floor().x % 2 == 0)
					return (a.gob.rc.floor().y <b.gob.rc.floor().y) ? 1 : (a.gob.rc.floor().y >b.gob.rc.floor().y) ? -1 : 0;
				else
					return (a.gob.rc.floor().y <b.gob.rc.floor().y) ? -1 : (a.gob.rc.floor().y >b.gob.rc.floor().y) ? 1 : 0;
			} else
				return (a.gob.rc.floor().x <b.gob.rc.floor().x) ? -1 : (a.gob.rc.floor().x > b.gob.rc.floor().x) ? 1 : 0;
		}
	}

	private static Pattern liquidPattern = Pattern.compile(String.format("[0-9.]+ l of (%s)",
			String.join("|", new String[] { "Water", "Piping Hot Tea", "Tea" , "Milk" , "Cowsmilk", "Sheepsmilk", "Goatsmilk"}), Pattern.CASE_INSENSITIVE));

	// Click some object with button and a modifier
	// Button 1 = Left click and 3 = right click
	// Modifier 1 - shift; 2 - ctrl; 4 - alt;
	public static void doClick(Gob gob, int button, int mod) {
		PBotAPI.gui.map.wdgmsg("click", Coord.z, gob.rc.floor(posres), button, 0, mod, (int) gob.id, gob.rc.floor(posres), 0,
				-1);
	}

	public static int getAmount(GItem item) {
		int ret = -1;
		for(ItemInfo o:item.info()) {
			if(o instanceof GItem.Amount)
				ret = ((GItem.Amount) o).itemnum();
		}
		return ret;
	}

	// Returns null if not found
	public static Coord getFreeInvSlot(Inventory inventory) {
		return inventory.getFreeSlot();
	}

	public static Coord getFreeInvSlotAlt(InventoryBelt inventory) {
		return inventory.getFreeSlot();
	}

	public static java.util.List<Coord> getFreeInvSlots(Inventory inventory) {
		return inventory.getFreeSlots();
	}

	public static java.util.List<Coord> getFreeInvSlotsAlt(InventoryBelt inventory) {
		return inventory.getFreeSlots();
	}

	// Finds nearest objects and returns closest one
	public static Gob findObjectByNames(int radius, String... names) {
		Coord2d plc = player().rc;
		double min = radius;
		Gob nearest = null;
		synchronized (PBotAPI.gui.ui.sess.glob.oc) {
			for (Gob gob : PBotAPI.gui.ui.sess.glob.oc) {
				double dist = gob.rc.dist(plc);
				if (dist < min) {
					boolean matches = false;
					for (String name : names) {
						if (gob.getres() != null && gob.getres().name.equals(name)) {
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

	public static Gob findNearestBarrel(int radius, java.util.List<Gob> blacklist){
		Coord2d plc = player().rc;
		double min = radius;
		Gob nearest = null;
		synchronized (PBotAPI.gui.ui.sess.glob.oc){
			for(Gob gob : PBotAPI.gui.ui.sess.glob.oc) {
				double dist = gob.rc.dist(plc);
				if(dist < min) {
					if (gob.getres() != null && gob.getres().name.startsWith("gfx/terobjs/barrel")) {
						System.out.println("Barrel found");
						if(blacklist.size() > 0) {
							if (!blacklist.contains(gob)) {
								System.out.println("BlackList Didn't Contain Gob, adding");
								min = dist;
								nearest = gob;
							}else
								System.out.println("BlackList Did Contain Gob, not adding");
						}else{
							System.out.println("BlackList size 0, adding");
							min = dist;
							nearest = gob;
						}
					}
				}
			}
		}
		return  nearest;
	}

	// Find object by ID, returns null if not found
	public static Gob findObjectById(long id) {
		return PBotAPI.gui.ui.sess.glob.oc.getgob(id);
	}

	// true if player moving
	public static boolean isMoving() {
		if (player().getv() == 0)
			return false;
		else
			return true;
	}

	// Chooses option from flower menu
	public static void Choose(FlowerMenu.Petal option) {
		PBotAPI.gui.wdgmsg("cl", option.num, PBotAPI.gui.ui.modflags());
	}

	// Finds an item from inventory that contains liquids that can be consumed
	public static WItem findDrink(Inventory inv) {
		for (WItem item : inv.children(WItem.class)) {
			if (canDrinkFrom(item))
				return item;
		}
		Equipory e = PBotAPI.gui.getequipory();
		WItem l = e.quickslots[6];
		WItem r = e.quickslots[7];
		if (canDrinkFrom(l))
			return l;
		if (canDrinkFrom(r))
			return r;
		return null;
	}


	// Takes item in hand
	public static void takeItem(Widget item) {
		item.wdgmsg("take", Coord.z);
		while (getItemAtHand() == null) {
			sleep(10);
		}
	}

	// Finds the nearest crop with a name and stage
	public static Gob findNearestStageCrop(int radius, int stage, String... names) {
		Coord2d plc = player().rc;
		double min = radius;
		Gob nearest = null;
		try {
			synchronized (PBotAPI.gui.ui.sess.glob.oc) {
				for (Gob gob : PBotAPI.gui.ui.sess.glob.oc) {
					double dist = gob.rc.dist(plc);
					if (dist < min) {
						boolean matches = false;
						for (String name : names) {
							if (gob.getres() != null && gob.getres().name.contains(name)) {
								if (gob.getStage() == stage) {
									matches = true;
									break;
								}
							}
						}
						if (matches) {
							min = dist;
							nearest = gob;
						}
					}
				}
			}
		}catch(Exception q){q.printStackTrace();}
		return nearest;
	}
	public static Gob findNearestGob(int radius, java.util.List<Gob> blacklist, String... names) {
		Coord2d plc = player().rc;
		double min = radius;
		Gob nearest = null;
		try {
			synchronized (PBotAPI.gui.ui.sess.glob.oc) {
				for (Gob gob : PBotAPI.gui.ui.sess.glob.oc) {
					double dist = gob.rc.dist(plc);
					if (dist < min) {
						boolean matches = false;
						for (String name : names) {
							if (gob.getres() != null && gob.getres().basename().contains(name)) {
								if (!blacklist.contains(gob)) {
									matches = true;
									break;
								}
							}
						}
						if (matches) {
							min = dist;
							nearest = gob;
						}
					}
				}
			}
		}catch(Exception q){q.printStackTrace();}
		return nearest;
	}

	// Checks if the object's name can be found from resources
	public static boolean isObjectName(Gob gob, String name) {
		try {
			Resource res = gob.getres();
			return (res != null) && res.name.contains(name);
		} catch (Loading e) {
			return false;
		}
	}

	// Use item in hand to ground below player, for example, plant carrot
	public static void mapInteractClick() {
		PBotAPI.gui.map.wdgmsg("itemact", getCenterScreenCoord(), player().rc.floor(posres), 3, PBotAPI.gui.ui.modflags());
	}

	public static void mapInteractLeftClick(int mod) {
		PBotAPI.gui.map.wdgmsg("click", getCenterScreenCoord(), player().rc.floor(posres), 1, PBotAPI.gui.ui.modflags());
	}

	// Destroys the given gob
	public static void destroyGob(Gob gob) {
		PBotAPI.gui.menu.wdgmsg("act", new Object[] { "destroy" });
		doClick(gob, 1, 0);
	}

	// Returns witems with specific names from inventory
	public static java.util.List<WItem> getInventoryItemsByNames(Inventory invwdg, java.util.List<String> items) {
		java.util.List<WItem> witems = new ArrayList<WItem>();
		for (WItem wi : getInventoryContents(invwdg)) {
			String resname = wi.item.resource().name;
			for (String s : items) {
				if (resname.equals(s))
					witems.add(wi);
			}
		}
		return witems;
	}

	// Returns witems with specific name from inventory
	public static java.util.List<WItem> getInventoryItemsByName(Inventory invwdg, String item) {
		java.util.List<WItem> witems = new ArrayList<WItem>();
		for (WItem wi : getInventoryContents(invwdg)) {
			String resname = wi.item.resource().name;
			if (resname.equals(item))
				witems.add(wi);
		}
		return witems;
	}



	// Drops item from  hand to given slot in given inventory
	public static void dropItemToInventory(Coord coord, Inventory inventory) {
		inventory.wdgmsg("drop", coord);
	}

	// Drops item from  hand to given slot in given inventory but works on alternate belt window
	public static void dropItemToInventory(Coord coord, InventoryBelt inventory) {
		inventory.wdgmsg("drop", coord);
	}

	// Returns amount of free inventory slots
	public static int invFreeSlots() {
		int takenSlots = 0;
		for (Widget i = PBotAPI.gui.maininv.child ; i != null; i = i.next) {
			if (i instanceof WItem) {
				WItem buf = (WItem) i;
				takenSlots += buf.size().x * buf.size().y;
			}
		}
		int allSlots = PBotAPI.gui.maininv.isz.x * PBotAPI.gui.maininv.isz.y;
		return allSlots - takenSlots;
	}

	public static void sysLogAppend(String msg, String clr){
		try {
			Field field = Color.class.getField(clr);
			Color clr2 = (Color)field.get(null);
			PBotAPI.gui.syslog.append(msg,clr2);
		}catch(Exception e){}
	}

	public static int getState(Fightview fv, int relation){
		return fv.lsrel.get(relation).give.state;
	}

	/* Get tile type
	 * @param x X-Coordinate
	 * @param y Y-Coordinate
	 * @return Tile type (integer)
	 */
	public static int getTile(int x, int y) {
		return PBotAPI.gui.ui.sess.glob.map.gettile(new Coord(x, y));
	}

	/* Teleport to you hearthfire
	 */
	public static void travelHearth() {
		PBotAPI.gui.act("travel", "hearth");
	}


	//Will set player speed to whatever int you send it.
	public static void setSpeed(int speed){
		Speedget speedwdg = PBotAPI.gui.speedget.get();
		if (speedwdg != null)
			speedwdg.set(speed);
	}
	//should return current max move speed? maybe?
	public static int maxSpeed(){
		Speedget speedwdg = PBotAPI.gui.speedget.get();
		if (speedwdg != null)
			return speedwdg.max;
		else
			return 0;
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
		synchronized (PBotAPI.gui.ui.sess.glob.oc) {
			for (Gob gob : PBotAPI.gui.ui.sess.glob.oc) {
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
		return PBotAPI.gui.getmeter("stam", 0).a;
	}

	/**
	 * Get the player energy
	 * @return Returns 0-100
	 */
	public static int getEnergy() {
		return PBotAPI.gui.getmeter("nrj", 0).a;
	}

	/**
	 * Get the player hp
	 * @return Returns 0-100
	 */
	public static int getShp() {
		return PBotAPI.gui.getmeter("hp", 0).a;
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
		PBotAPI.gui.map.purusPfRightClick(gob, -1, 3, mod, null);
	}

	/**
	 * Itemact with gob, to fill trough with item in hand for example
	 * @param gob Gob to interact with
	 * @param mod 1 = shift, 2 = ctrl, 4 = alt
	 */
	public static void itemClick(Gob gob, int mod) {
		PBotAPI.gui.map.wdgmsg("itemact", Coord.z, gob.rc.floor(posres), mod, 0, (int) gob.id, gob.rc.floor(posres), 0, -1);
	}

	/**
	 * Returns the player gob
	 * @return Player gob
	 */
	public static Gob player() {
		if(PBotAPI.gui != null && PBotAPI.gui.map != null) {
			if(PBotAPI.gui.map.player() != null)
				return PBotAPI.gui.map.player();
			else
				return null;
		}
		else
			return null;
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
	 * Get stage of the crop
	 * @param gob Crop to get stage of
	 * @return Stage of the crop
	 */
	public static int getCropStage(Gob gob) {
		return gob.getStage();
	}

	/**
	 * Makes a stockpile from the item in the hand
	 */
	public static void makePile() {
		PBotAPI.gui.map.wdgmsg("itemact", getCenterScreenCoord(), player().rc.floor(posres), 0);
	}

	/**
	 * Use to place something, for example, a stockpile
	 * 11 offset = 1 tile
	 * @param x Offset from player to place stockpile to
	 * @param y Offset from player to place stockpile to
	 */
	public static void placeThing(int x, int y) {
		PBotAPI.gui.map.wdgmsg("place", player().rc.add(x, y).floor(posres), 0, 1, 0);
	}

	/**
	 * Destroys the given gob
	 * @param gob Gob to destroy
	 */
	public static void liftGob(Gob gob) {
		PBotAPI.gui.menu.wdgmsg("act", new Object[] { "carry" });
		doClick(gob, 1, 0);
	}

	/**
	 * Drops an item from the hand
	 * @param mod 1 = shift, 2 = ctrl, 4 = alt
	 */
	public static void dropItem(int mod) {
		PBotAPI.gui.map.wdgmsg("drop", Coord.z, PBotAPI.gui.map.player().rc.floor(posres), mod);
	}

	/**
	 * Drops the given item from the inventory
	 * @param item Item to drop
	 */
	public static void dropItemFromInventory(WItem item) {
		item.item.wdgmsg("drop", Coord.z);
	}

	/**
	 * Wait for a window with a specific name to appear
	 * @param windowName Name of the window
	 */
	public static void waitForWindow(String windowName) {
		while (PBotAPI.gui.getwnd(windowName) == null) {
			sleep(50);
		}
	}

	/**
	 * Get a window with name
	 * @param name Name of the window
	 * @return The window or null if not found
	 */
	public static Window getWindow(String name) {
		return PBotAPI.gui.getwnd(name);
	}

	/**
	 * Close the window
	 * @param window The window to close
	 */
	public static void closeWindow(Window window) {
		window.reqdestroy();
	}


	/**
	 * Returns item with the specific name, or null if not found
	 * @param invwdg Inventory to look items from
	 * @param items Name(s) of the items to look for
	 * @return Item, or null if not found
	 */
	public static WItem getInventoryItemByNames(Inventory invwdg, java.util.List<String> items) {
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
	public static java.util.List<WItem> getInventoryContents(Inventory invwdg) {
		java.util.List<WItem> witems = new ArrayList<WItem>();
		for (Widget witm = invwdg.lchild; witm != null; witm = witm.prev) {
			if (witm instanceof WItem) {
				witems.add((WItem) witm);
			}
		}
		return witems;
	}

	//same as above for returns a list of all WItems from all inventories seen on screen
	public static List<WItem> getallInventoryContents() {
		java.util.List<WItem> witems = new ArrayList<WItem>();
		synchronized (PBotAPI.gui.ui.root.lchild) {
			try {
				for (Widget q = PBotAPI.gui.ui.root.lchild; q != null; q = q.rnext()) {
					if (q instanceof Inventory) {
						witems.addAll(getInventoryContents((Inventory) q));
					}
				}
			} catch (Exception e) { }
		}
		return witems;
	}

	//same as above for returns a list of all WItems from all inventories seen on screen
	public static List<WItem> getallInventoryContentsbyString(String witem) {
		java.util.List<WItem> witems = new ArrayList<WItem>();
		List<WItem> finallist = new ArrayList<>();
		synchronized (PBotAPI.gui.ui.root.lchild) {
			try {
				for (Widget q = PBotAPI.gui.ui.root.lchild; q != null; q = q.rnext()) {
					if (q instanceof Inventory) {
						witems.addAll(getInventoryContents((Inventory) q));
					}
				}
			} catch (Exception e) {}
		}
		for(WItem item: witems){
			if (item.item.getname().contains(witem))
				finallist.add(item);
		}
		return finallist;
	}

	public static List<WItem> getPlayerInvContentsPartial(String witem){
		List<WItem> witems = new ArrayList<>();
		List<WItem> finallist = new ArrayList<>();
		witems.addAll(getInventoryContents(PBotAPI.gui.maininv));
		for(WItem item:witems) {
			if (item.item.getname().contains(witem)) {
				finallist.add(item);
			}
		}
		return finallist;
	}

	public static List<WItem> getPlayerInvContentsExact(String witem){
		List<WItem> witems = new ArrayList<>();
		List<WItem> finallist = new ArrayList<>();
		witems.addAll(getInventoryContents(PBotAPI.gui.maininv));
		for(WItem item:witems) {
			if (item.item.getname().equals(witem)) {
				finallist.add(item);
			}
		}
		return finallist;
	}

	/**
	 * Log out to character selection
	 */
	//public static void logoutChar() {
	//	PBotAPI.gui.act("lo", "cs");
	//}

	public static void logout(){
		if(haven.automation.Discord.jdalogin != null)
			PBotAPI.gui.DiscordToggle();
		PBotAPI.gui.act("lo");
	}
	public static void logoutChar() {
		if(Discord.jdalogin != null)
			PBotAPI.gui.DiscordToggle();
		PBotAPI.gui.act("lo", "cs");
	}

	/**
	 * Check if stockpile is full
	 * @param gob Stockpile gob to check
	 * @return True if stockpile is full, else false
	 */
	public static boolean stockpileIsFull(Gob gob) {
		if(gob.sdt()==31)
			return true;
		else
			return false;
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
	 * List of all gobs visible to the client
	 * @return List of all gobs
	 */
	public static java.util.List<Gob> getGobs() {
		List<Gob> list = new ArrayList<Gob>();
		synchronized (PBotAPI.gui.ui.sess.glob.oc) {
			for (Gob gob : PBotAPI.gui.ui.sess.glob.oc)
				list.add(gob);
		}
		return list;
	}

	/**
	 * Send act message to server
	 * Act can be used for example to choose a cursor
	 * Some acts:
	 * dig, mine, carry, destroy, fish, inspect, repair, crime, swim, tracking, aggro, shoot
	 * @param act Act to choose
	 */
	public static void doAct(String act) {
		PBotAPI.gui.menu.wdgmsg("act", act);
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

}
