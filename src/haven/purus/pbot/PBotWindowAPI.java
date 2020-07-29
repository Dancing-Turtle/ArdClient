package haven.purus.pbot;

import haven.ISBox;
import haven.Inventory;
import haven.Text;
import haven.UI;
import haven.VMeter;
import haven.Widget;
import haven.Window;

import java.util.ArrayList;

public class PBotWindowAPI {

    /**
     * Wait for a window with a specific name to appear
     *
     * @param windowName Name of the window
     * @param timeout    Timeout in milliseconds to wait for the window to appear
     * @return Returns the window or null if not found
     */
    public static Window waitForWindow(UI ui, String windowName, long timeout) {
        Window window;
        int retries = 0;
        while ((window = ui.gui.getwnd(windowName)) == null) {
            if (retries * 25 >= timeout) {
                return null;
            }
            retries++;
            PBotUtils.sleep(25);
        }
        return window;
    }

    /**
     * Wait for a window with a specific name to appear
     *
     * @param windowName Name of the window
     * @return Returns the window or null if not found
     */
    public static Window waitForWindow(UI ui, String windowName) {
        Window window;
        while ((window = ui.gui.getwnd(windowName)) == null) {
            PBotUtils.sleep(25);
        }
        return window;
    }

    /**
     * Wait for a window with a specific name to disappear
     *
     * @param timeout    in milliseconds
     * @param windowName Name of the window
     * @return false if the window did not close before the timeout (it may or may not still close in the future)
     */
    public static boolean waitForWindowClose(UI ui, String windowName, long timeout) {
        int retries = 0;
        while (ui.gui.getwnd(windowName) != null) {
            if (retries * 25 >= timeout) {
                return false;
            }
            retries++;
            PBotUtils.sleep(50);
        }
        return true;
    }

    /**
     * Get a window with name
     *
     * @param name Name of the window
     * @return The window or null if not found
     */
    public static Window getWindow(UI ui, String name) {
        return ui.gui.getwnd(name);
    }

    /**
     * Close the window
     *
     * @param window The window to close
     */
    public static void closeWindow(Window window) {
        window.reqdestroy();
    }


    /**
     * Tries to find an inventory attached to the given window, such as cupboard
     *
     * @param window Window to look inventory from
     * @return Inventory of the window or null if not found
     */
    public static PBotInventory getInventory(Window window) {
        for (Widget wdg = window.lchild; wdg != null; wdg = wdg.prev) {
            if (wdg instanceof Inventory) {
                return new PBotInventory((Inventory) wdg);
            }
        }
        return null;
    }

    /**
     * Tries to find an inventories attached to the given window, such as cupboard
     *
     * @param window Window to check for inventories
     * @return List of inventories of the window, empty if not found
     */
    public static ArrayList<PBotInventory> getInventories(Window window) {
        ArrayList<PBotInventory> inventories = new ArrayList<>();
        for (Widget wdg = window.lchild; wdg != null; wdg = wdg.prev) {
            if (wdg instanceof Inventory) {
                inventories.add(new PBotInventory((Inventory) wdg));
            }
        }
        return inventories;
    }

    /**
     * Returns total capacity of the stockpile window which is currently open
     *
     * @return Total capacity, or -1 if stockpile window could not be found
     */
    public static int getStockpileTotalCapacity(UI ui) {
        Window wnd = getWindow(ui, "Stockpile");
        if (wnd == null) {
            return -1;
        } else {
            for (Widget w = wnd.child; w != null; w = w.next) {
                if (w instanceof ISBox) {
                    ISBox sb = (ISBox) w;
                    return sb.getTotalCapacity();
                }
            }
        }
        return -1;
    }

    /**
     * Returns used capacity of the stockpile window which is currently open
     *
     * @return Used capacity, or -1 if stockpile window could not be found
     */
    public static int getStockpileUsedCapacity(UI ui) {
        Window wnd = getWindow(ui, "Stockpile");
        if (wnd == null) {
            return -1;
        } else {
            for (Widget w = wnd.child; w != null; w = w.next) {
                if (w instanceof ISBox) {
                    ISBox sb = (ISBox) w;
                    return sb.getUsedCapacity();
                }
            }
        }
        return -1;
    }

    /**
     * Attempts to get items from the stockpile that is currently open
     *
     * @param count How many items to take
     */
    public static void takeItemsFromStockpile(UI ui, int count) {
        Window wnd = getWindow(ui, "Stockpile");
        if (wnd != null) {
            for (Widget w = wnd.child; w != null; w = w.next) {
                if (w instanceof ISBox) {
                    ISBox sb = (ISBox) w;
                    for (int i = 0; i < count; i++)
                        w.wdgmsg("xfer");
                    break;
                }
            }
        }
    }

    /**
     * Put an item from the hand to a stockpile window that is currently open
     */
    public static void putItemFromHandToStockpile(UI ui) {
        Window wnd = getWindow(ui, "Stockpile");
        if (wnd != null) {
            for (Widget w = wnd.child; w != null; w = w.next) {
                if (w instanceof ISBox) {
                    ISBox sb = (ISBox) w;
                    w.wdgmsg("drop");
                    break;
                }
            }
        }
    }

    /**
     * Get amount at a meter of the window, from 0 to 100, for example, a trough
     *
     * @return Meter amount, -1 if not meter found
     */
    public static int getAmount(Window window) {
        VMeter vm = window.getchild(VMeter.class);
        if (vm == null)
            return -1;
        else
            return vm.amount;
    }

    /**
     * Get amounts of meters of the window, from 0 to 100, some windows may have more than 1 meter, like chicken coops
     *
     * @return List containing amounts of the meters that were found
     */
    public static ArrayList<Integer> getAmounts(Window window) {
        ArrayList<Integer> amounts = new ArrayList<>();
        for (VMeter vm : window.getchilds(VMeter.class)) {
            amounts.add(vm.amount);
        }
        return amounts;
    }

    /**
     * Get tooltips of widgets in the window Example: Trough
     *
     * @param window Window to check for tooltips
     * @return List containing the tooltips that were found
     */
    public static ArrayList<String> getTooltips(Window window) {
        ArrayList<String> tooltips = new ArrayList<>();
        for (Widget w = window.child; w != null; w = w.next) {
            if (w.tooltip instanceof Text) {
                tooltips.add(((Text) w.tooltip).text);
            }
        }
        return tooltips;
    }

    /**
     * Attempts to put item that fits form inventory to the stockpile, like when scrolling to stockpile
     *
     * @param count How many items to put into the stockpile
     */
    public static void putItemFromInventoryToStockpile(UI ui, int count) {
        Window wnd = getWindow(ui, "Stockpile");
        if (wnd != null) {
            for (Widget w = wnd.child; w != null; w = w.next) {
                if (w instanceof ISBox) {
                    ISBox sb = (ISBox) w;
                    for (int i = 0; i < count; i++)
                        w.wdgmsg("xfer2", 1, 1);
                    break;
                }
            }
        }
    }
}
