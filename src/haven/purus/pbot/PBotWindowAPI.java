package haven.purus.pbot;

import haven.*;

public class PBotWindowAPI {

	/**
	 * Wait for a window with a specific name to appear
	 * @param windowName Name of the window
	 */
	public static void waitForWindow(String windowName) {
		while (PBotAPI.gui.getwnd(windowName) == null) {
			PBotUtils.sleep(50);
		}
	}

	/**
	 * Wait for a window with a specific name to disappear
	 * @param windowName Name of the window
	 */
	public static void waitForWindowClose(String windowName) {
		while (PBotAPI.gui.getwnd(windowName) != null) {
			PBotUtils.sleep(50);
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
	 * Tries to find an inventory attached to the given window, such as cupboard
	 * @param window Window to look inventory from
	 * @return Inventory of the window or null if not found
	 */
	public static PBotInventory getInventory(Window window) {
		for(Widget wdg = window.lchild; wdg!=null; wdg = wdg.prev) {
			if(wdg instanceof Inventory) {
				return new PBotInventory((Inventory) wdg);
			}
		}
		return null;
	}

	/**
	 * Returns total capacity of the stockpile window which is currently open
	 * @return Total capacity, or -1 if stockpile window could not be found
	 */
	public static int getStockpileTotalCapacity() {
		Window wnd = getWindow("Stockpile");
		if(wnd == null) {
			return -1;
		} else {
			for(Widget w = wnd.child; w != null; w = w.next) {
				if(w instanceof ISBox) {
					ISBox sb = (ISBox) w;
					return sb.getTotalCapacity();
				}
			}
		}
		return -1;
	}

	/**
	 * Returns used capacity of the stockpile window which is currently open
	 * @return Used capacity, or -1 if stockpile window could not be found
	 */
	public static int getStockpileUsedCapacity() {
		Window wnd = getWindow("Stockpile");
		if(wnd == null) {
			return -1;
		} else {
			for(Widget w = wnd.child; w != null; w = w.next) {
				if(w instanceof ISBox) {
					ISBox sb = (ISBox) w;
					return sb.getUsedCapacity();
				}
			}
		}
		return -1;
	}

	/**
	 * Attemps to get items from the stockpile that is currently open
	 * @param count How many items to take
	 */
	public static void takeItemsFromStockpile(int count) {
		Window wnd = getWindow("Stockpile");
		if(wnd != null) {
			for(Widget w = wnd.child; w != null; w = w.next) {
				if(w instanceof ISBox) {
					ISBox sb = (ISBox) w;
					for(int i=0; i<count; i++)
						w.wdgmsg("xfer");
					break;
				}
			}
		}
	}

	/**
	 * Put a item from hand to a stockpile window that is currently open
	 */
	public static void putItemFromHandToStockpile() {
		Window wnd = getWindow("Stockpile");
		if(wnd != null) {
			for(Widget w = wnd.child; w != null; w = w.next) {
				if(w instanceof ISBox) {
					ISBox sb = (ISBox) w;
					w.wdgmsg("drop");
					break;
				}
			}
		}
	}

	/**
	 * Get amount at a meter of the window, from 0 to 100, for example, a trough
	 * @return Meter amount, -1 if not meter found
	 */
	public static int getAmount(Window window) {
		VMeter vm = window.getchild(VMeter.class);
		if(vm == null)
			return -1;
		else
			return vm.amount;
	}

	/**
	 * Attempts to put item that fits form inventory to the stockpile, like when scrolling to stockpile
	 * @param count How many items to put into the stockpile
	 */
	public static void putItemFromInventoryToStockpile(int count) {
		Window wnd = getWindow("Stockpile");
		if(wnd != null) {
			for(Widget w = wnd.child; w != null; w = w.next) {
				if(w instanceof ISBox) {
					ISBox sb = (ISBox) w;
					for(int i=0; i<count; i++)
						w.wdgmsg("xfer2", 1, 1);
					break;
				}
			}
		}
	}
}
