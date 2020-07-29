package haven.purus.pbot;

import haven.Coord;
import haven.Coord2d;
import haven.Equipory;
import haven.Fightview;
import haven.FlowerMenu;
import haven.GItem;
import haven.GameUI;
import haven.Gob;
import haven.Inventory;
import haven.InventoryBelt;
import haven.ItemInfo;
import haven.Loading;
import haven.MCache;
import haven.Makewindow;
import haven.Resource;
import haven.Speedget;
import haven.UI;
import haven.WItem;
import haven.Widget;
import haven.Window;
import haven.automation.Discord;
import haven.purus.DrinkWater;
import haven.purus.ItemClickCallback;
import haven.purus.pbot.gui.PBotWindow;
import net.dv8tion.jda.core.entities.TextChannel;

import java.awt.Color;
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
     *
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
     * Kills thread
     */
    public static void stop() {
        try {

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Right click a gob with pathfinder, wait until pathfinder is finished
     *
     * @param gob Gob to right click
     * @param mod 1 = shift, 2 = ctrl, 4 = alt
     * @return False if path was not found, true if it was found
     */
    public static boolean pfRightClick(UI ui, PBotGob gob, int mod) {
        ui.gui.map.purusPfRightClick(gob.gob, -1, 3, mod, null);
        try {
            ui.gui.map.pastaPathfinder.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        synchronized (ui.gui.map) {
            return ui.gui.map.foundPath;
        }
    }

    //new boshaw pf right clicks.
    public static void PathfinderRightClick(UI ui, Gob gob, int mod) {
        ui.gui.map.pathtoRightClick(gob, mod);
    }


    /**
     * Chooses a petal with given label from a flower menu that is currently open
     *
     * @param name Name of petal to open
     * @return False if petal or flower menu with name could not be found
     */
    public static boolean choosePetal(UI ui, String name) {
        FlowerMenu menu = ui.root.findchild(FlowerMenu.class);
        if (menu != null) {
            for (FlowerMenu.Petal opt : menu.opts) {
                if (opt.name.equals(name)) {
                    menu.choose(opt);
                    menu.destroy();
                    return true;
                }
            }
        }
        return false;
    }


    public static boolean petalExists(UI ui) {
        FlowerMenu menu = ui.root.findchild(FlowerMenu.class);
        if (menu != null) {
            return true;
        }
        return false;
    }

    /**
     * Closes flowermenu, if it is open
     */
    public static void waitFlowermenuClose(UI ui) {
        while (ui.root.findchild(FlowerMenu.class) != null)
            sleep(25);
    }

    /**
     * Click some place on map
     *
     * @param x   x-coordinate
     * @param y   y-coordinate
     * @param btn 1 = left click, 3 = right click
     * @param mod 1 = shift, 2 = ctrl, 4 = alt
     */
    public static void mapClick(UI ui, int x, int y, int btn, int mod) {
        ui.gui.map.wdgmsg("click", getCenterScreenCoord(ui), new Coord2d(x, y).floor(posres), btn, mod);
    }

    /**
     * Click some place on map
     *
     * @param x   x-coordinate
     * @param y   y-coordinate
     * @param btn 1 = left click, 3 = right click
     * @param mod 1 = shift, 2 = ctrl, 4 = alt
     */
    public static void mapClick(UI ui, double x, double y, int btn, int mod) {
        ui.gui.map.wdgmsg("click", getCenterScreenCoord(ui), new Coord2d(x, y).floor(posres), btn, mod);
    }

    /**
     * Use item in hand to ground below player, for example, to plant carrot
     */
    /*public static void mapInteractClick() {
        ui.gui.map.wdgmsg("itemact", PBotUtils.getCenterScreenCoord(), PBotGobAPI.player().getRcCoords().floor(posres), 3, ui.modflags());
    }*/


    /**
     * Coordinates of the center of the screen
     *
     * @return Coordinates of the center of the screen
     */
    public static Coord getCenterScreenCoord(UI ui) {
        Coord sc, sz;
        sz = ui.gui.map.sz;
        sc = new Coord((int) Math.round(Math.random() * 200 + sz.x / 2 - 100),
                (int) Math.round(Math.random() * 200 + sz.y / 2 - 100));
        return sc;
    }


    /**
     * Left click to somewhere with pathfinder, wait until pathfinder is finished
     *
     * @param x X-Coordinate
     * @param y Y-Coordinate
     * @return False if path was not found, true if it was found
     */
    public static boolean pfLeftClick(UI ui, double x, double y) {
        ui.gui.map.purusPfLeftClick(new Coord2d(x, y), null);
        try {
            ui.gui.map.pastaPathfinder.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        synchronized (ui.gui.map) {
            return ui.gui.map.foundPath;
        }
    }

    /**
     * Starts crafting item with the given name
     *
     * @param name    Name of the item ie. "clogs"
     * @param makeAll 0 To craft once, 1 to craft all
     */
    public static void craftItem(UI ui, String name, int makeAll) {
        openCraftingWnd(ui, name);
        loop:
        while (true) {
            for (Widget w : ui.widgets.values()) {
                if (w instanceof Makewindow) {
                    ui.gui.wdgmsg(w, "make", makeAll);
                    break loop;
                }
            }
            sleep(25);
        }
    }

    /**
     * Waits for flower menu to appear
     */
    public static void waitForFlowerMenu(UI ui) {
        while (ui.root.findchild(FlowerMenu.class) == null) {
            sleep(15);
        }
    }

    public static void waitForFlowerMenu(UI ui, int limit) {
        int cycles = 0;
        while (ui.root.findchild(FlowerMenu.class) == null) {
            if (cycles == limit) {
                break;
            } else {
                sleep(1000);
                cycles++;
                PBotUtils.sysMsg(ui, "Cycles: " + cycles);
            }
        }
    }

    /**
     * Waits for the flower menu to disappear
     */
    public static void closeFlowermenu(UI ui) {
        FlowerMenu menu = ui.root.findchild(FlowerMenu.class);
        if (menu != null) {
            menu.choose(null);
            menu.destroy();
        }
        while (ui.root.findchild(FlowerMenu.class) != null) {
            sleep(15);
        }
    }

    /**
     * Waits for the hourglass timer when crafting or drinking for example
     * Also waits until the hourglass has been seen to change at least once
     */
    public static void waitForHourglass(UI ui) {
        double prog = ui.gui.prog;
        while (prog == ui.gui.prog) {
            prog = ui.gui.prog;
            sleep(5);
        }
        while (ui.gui.prog >= 0) {
            sleep(50);
        }
    }

    /**
     * Waits for the hourglass timer when crafting or drinking for example
     * Also waits until the hourglass has been seen to change at least once
     * If hourglass does not appear within timeout, returns false, else true
     *
     * @param timeout Timeout in milliseconds
     */
    public static boolean waitForHourglass(UI ui, int timeout) {
        double prog = ui.gui.prog;
        int retries = 0;
        while (prog == ui.gui.prog) {
            if (retries > timeout / 5)
                return false;
            retries++;
            prog = ui.gui.prog;
            sleep(5);
        }
        while (ui.gui.prog >= 0) {
            sleep(25);
        }
        return true;
    }

    //will send a message to the selected channel with message text if discord is connected ingame.
    public static void sendDiscordMsg(UI ui, String channel, String text) {
        if (Discord.jdalogin != null) {
            for (TextChannel loop : haven.automation.Discord.channels)
                if (loop.getName().equals(channel)) {
                    loop.sendMessage(ui.gui.getparent(GameUI.class).buddies.getCharName() + ": " + text).queue();
                    break;
                }
        }
    }

    /**
     * Returns value of hourglass, -1 = no hourglass, else the value between 0.0 and 1.0
     *
     * @return value of hourglass
     */
    public static double getHourglass(UI ui) {
        return ui.gui.prog;
    }

    // TODO: Return false if drinking was not successful (no water found for example)

    /**
     * Attempts to drink water by using the same water drinking script as in extensions
     *
     * @param wait Wait for the drinking to finish
     */
    public static boolean drink(UI ui, boolean wait) {
        if (!ui.gui.drinkingWater) {
            Thread t = new Thread(new DrinkWater(ui.gui));
            t.start();
            if (wait) {
                try {
                    t.join();
                    if (!ui.gui.lastDrinkingSucessful) {
                        sysMsg(ui, "PBotUtils Warning: Couldn't drink, didn't find anything to drink!", Color.ORANGE);
                        return false;
                    }
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                waitForHourglass(ui);
            }
        }
        return true;
    }

    /**
     * Opens the crafting window for given item
     *
     * @param name Name of craft for wdgmsg
     */
    public static void openCraftingWnd(UI ui, String name) {
        // Close current window and wait for it to close
        Window wnd = PBotWindowAPI.getWindow(ui, "Crafting");
        if (wnd != null)
            PBotWindowAPI.closeWindow(wnd);
        PBotWindowAPI.waitForWindowClose(ui, "Crafting", 1000 * 1000);
        ui.gui.wdgmsg("act", "craft", name);
        PBotWindowAPI.waitForWindow(ui, "Crafting", 1000 * 1000);
    }

    /**
     * Send a system message to the user
     *
     * @param str Message to send
     */
    public static void sysMsg(UI ui, String str) {
        ui.gui.msg(str, Color.WHITE);
    }

    /**
     * Send a system message to the user
     *
     * @param str Message to send
     * @param col Color of the text
     */
    public static void sysMsg(UI ui, String str, Color col) {
        ui.gui.msg(str, col);
    }

    /**
     * Send a system message to the user
     *
     * @param str Message to send
     * @param r   Amount of red colour in the text
     * @param g   Amount of green colour in the text
     * @param b   Amount of blue colour in the text
     */
    public static void sysMsg(UI ui, String str, int r, int g, int b) {
        ui.gui.msg(str, new Color(r, g, b));
    }

    /**
     * Returns the players inventory
     *
     * @return Inventory of the player
     */
    public static PBotInventory playerInventory(UI ui) {
        return new PBotInventory(ui.gui.maininv);
    }

    /**
     * Returns all open inventories
     *
     * @return List of inventories
     */
    public static ArrayList<PBotInventory> getAllInventories(UI ui) {
        ArrayList<PBotInventory> ret = new ArrayList<>();
        for (Widget window = ui.gui.lchild; window != null; window = window.prev) {
            if (window instanceof Window) {
                for (Widget wdg = window.lchild; wdg != null; wdg = wdg.prev) {
                    if (wdg instanceof Inventory) {
                        ret.add(new PBotInventory((Inventory) wdg));
                    }
                }
            }
        }
        return ret;
    }

    /**
     * Create a PBotWindow object, See PBotWindow for usage
     *
     * @param title  Title of the window
     * @param height Height of the window
     * @param width  Width of the window
     * @param id     scriptID variable of the script
     * @return PBotWindow object
     */
    public static PBotWindow PBotWindow(UI ui, String title, int height, int width, String id) {
        PBotWindow window = new PBotWindow(new Coord(width, height), title, id);
        ui.gui.add(window, 300, 300);
        return window;
    }

    /**
     * Returns the item currently in the hand
     *
     * @return Item at hand
     */
    public static PBotItem getItemAtHand(UI ui) {
        if (ui.gui.vhand == null)
            return null;
        else
            return new PBotItem(ui.gui.vhand);
    }


    /**
     * Returns the item currently in the hand
     *
     * @return Item at hand
     */
    public static GItem getGItemAtHand(UI ui) {
        for (GameUI.DraggedItem item : ui.gui.hand)
            return item.item;
        return null;
    }


    /**
     * Drops an item from the hand and waits until it has been dropped
     *
     * @param mod 1 = shift, 2 = ctrl, 4 = alt
     */
    public static void dropItemFromHand(UI ui, int mod) {
        ui.gui.map.wdgmsg("drop", Coord.z, ui.gui.map.player().rc.floor(posres), mod);
        while (getItemAtHand(ui) != null)
            sleep(25);
    }

    /**
     * Activate area selection by dragging.
     * To get coordinates of the area selected, use getSelectedAreaA and getSelectedAreaB
     * User can select an area by dragging
     */
    public static void selectArea(UI ui) {
        sysMsg(ui, "Please select an area by dragging!", Color.ORANGE);
        ui.gui.map.PBotAPISelect = true;
        while (ui.gui.map.PBotAPISelect) {
            sleep(25);
        }
    }

    /**
     * Next click to item in inventory returns the item, the function will wait until this happens
     */
    public static PBotItem selectItem(UI ui) {
        synchronized (ItemClickCallback.class) {
            ui.gui.registerItemCallback(new ItemCb());
        }
        while (itemSelectWait) {
            PBotUtils.sleep(25);
        }
        synchronized (ItemClickCallback.class) {
            ui.gui.unregisterItemCallback();
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
     *
     * @return A-Point of the rectangle
     */
    public static Coord getSelectedAreaA() {
        return selectedAreaA;
    }

    /**
     * Get B point of the rectangle selected with selectArea()
     *
     * @return B-Point of the rectangle
     */
    public static Coord getSelectedAreaB() {
        return selectedAreaB;
    }

    /**
     * Callback for area select
     */
    public static void areaSelect(UI ui, Coord a, Coord b) {
        selectedAreaA = a.mul(MCache.tilesz2);
        selectedAreaB = b.mul(MCache.tilesz2).add(11, 11);
        sysMsg(ui, "Area selected!", Color.ORANGE);
    }

    /**
     * Returns a list of gobs in the rectangle between A and B points
     *
     * @param a A-point of the rectangle
     * @param b B-point of the rectangle
     * @return List of gobs in the area, sorted to zig-zag pattern
     */
    public static ArrayList<PBotGob> gobsInArea(UI ui, Coord a, Coord b) {
        // Initializes list of crops to harvest between the selected coordinates
        ArrayList<PBotGob> gobs = new ArrayList<PBotGob>();
        double bigX = a.x > b.x ? a.x : b.x;
        double smallX = a.x < b.x ? a.x : b.x;
        double bigY = a.y > b.y ? a.y : b.y;
        double smallY = a.y < b.y ? a.y : b.y;
        synchronized (ui.sess.glob.oc) {
            for (Gob gob : ui.sess.glob.oc) {
                if (gob.rc.x <= bigX && gob.rc.x >= smallX && gob.getres() != null && gob.rc.y <= bigY
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
     *
     * @param x X-Coord of the location (rc coord)
     * @param y Y-Coord of the location (rc coord)
     * @return
     */
    public static String tileResnameAt(UI ui, int x, int y) {
        try {
            Coord loc = new Coord(x, y);
            int t = ui.gui.map.glob.map.gettile(loc.div(11));
            Resource res = ui.gui.map.glob.map.tilesetr(t);
            if (res != null)
                return res.name;
            else
                return null;
        } catch (Loading l) {

        }
        return null;
    }

    // Sorts coordinate array to efficient zig-zag-like sequence for farming etc.
    private static class CoordSort implements Comparator<PBotGob> {
        public int compare(PBotGob a, PBotGob b) {
            if (a.gob.rc.floor().x == b.gob.rc.floor().x) {
                if (a.gob.rc.floor().x % 2 == 0)
                    return (a.gob.rc.floor().y < b.gob.rc.floor().y) ? 1 : (a.gob.rc.floor().y > b.gob.rc.floor().y) ? -1 : 0;
                else
                    return (a.gob.rc.floor().y < b.gob.rc.floor().y) ? -1 : (a.gob.rc.floor().y > b.gob.rc.floor().y) ? 1 : 0;
            } else
                return (a.gob.rc.floor().x < b.gob.rc.floor().x) ? -1 : (a.gob.rc.floor().x > b.gob.rc.floor().x) ? 1 : 0;
        }
    }

    private static Pattern liquidPattern = Pattern.compile(String.format("[0-9.]+ l of (%s)",
            String.join("|", new String[]{"Water", "Piping Hot Tea", "Tea", "Milk", "Cowsmilk", "Sheepsmilk", "Goatsmilk"}), Pattern.CASE_INSENSITIVE));

    // Click some object with button and a modifier
    // Button 1 = Left click and 3 = right click
    // Modifier 1 - shift; 2 - ctrl; 4 - alt;
    public static void doClick(UI ui, Gob gob, int button, int mod) {
        ui.gui.map.wdgmsg("click", Coord.z, gob.rc.floor(posres), button, 0, mod, (int) gob.id, gob.rc.floor(posres), 0,
                -1);
    }

    public static void doClickCrop(UI ui, Gob gob) {
        ui.gui.map.wdgmsg("click", gob.sc, gob.rc.floor(posres), 3, 0, 0, (int) gob.id, gob.rc.floor(posres), 0, -1);
    }

    public static int getAmount(GItem item) {
        int ret = -1;
        for (ItemInfo o : item.info()) {
            if (o instanceof GItem.Amount)
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
    public static Gob findObjectByNames(UI ui, int radius, String... names) {
        Coord2d plc = player(ui).rc;
        double min = radius;
        Gob nearest = null;
        synchronized (ui.sess.glob.oc) {
            for (Gob gob : ui.sess.glob.oc) {
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

    // Finds nearest objects and returns closest one
    public static Gob findObjectContains(UI ui, int radius, String... names) {
        Coord2d plc = player(ui).rc;
        double min = radius;
        Gob nearest = null;
        synchronized (ui.sess.glob.oc) {
            for (Gob gob : ui.sess.glob.oc) {
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

    public static Gob findNearestBarrel(UI ui, int radius, java.util.List<Gob> blacklist) {
        Coord2d plc = player(ui).rc;
        double min = radius;
        Gob nearest = null;
        synchronized (ui.sess.glob.oc) {
            for (Gob gob : ui.sess.glob.oc) {
                double dist = gob.rc.dist(plc);
                if (dist < min) {
                    if (gob.getres() != null && gob.getres().name.startsWith("gfx/terobjs/barrel")) {
                        System.out.println("Barrel found");
                        if (blacklist.size() > 0) {
                            if (!blacklist.contains(gob)) {
                                System.out.println("BlackList Didn't Contain Gob, adding");
                                min = dist;
                                nearest = gob;
                            } else
                                System.out.println("BlackList Did Contain Gob, not adding");
                        } else {
                            System.out.println("BlackList size 0, adding");
                            min = dist;
                            nearest = gob;
                        }
                    }
                }
            }
        }
        return nearest;
    }

    // Find object by ID, returns null if not found
    public static Gob findObjectById(UI ui, long id) {
        return ui.sess.glob.oc.getgob(id);
    }

    // true if player moving
    public static boolean isMoving(UI ui) {
        if (player(ui).getv() == 0)
            return false;
        else
            return true;
    }

    // Chooses option from flower menu
    public static void Choose(UI ui, FlowerMenu.Petal option) {
        ui.gui.wdgmsg("cl", option.num, ui.modflags());
    }

    // Finds an item from inventory that contains liquids that can be consumed
    public static WItem findDrink(UI ui, Inventory inv) {
        for (WItem item : inv.children(WItem.class)) {
            if (canDrinkFrom(item))
                return item;
        }
        Equipory e = ui.gui.getequipory();
        WItem l = e.quickslots[6];
        WItem r = e.quickslots[7];
        if (canDrinkFrom(l))
            return l;
        if (canDrinkFrom(r))
            return r;
        return null;
    }


    // Takes item in hand
    public static void takeItem(UI ui, Widget item) {
        item.wdgmsg("take", Coord.z);
        while (getItemAtHand(ui) == null) {
            sleep(10);
        }
    }

    // Finds the nearest crop with a name and stage
    public static Gob findNearestStageCrop(UI ui, int radius, int stage, String... names) {
        Coord2d plc = player(ui).rc;
        double min = radius;
        Gob nearest = null;
        try {
            synchronized (ui.sess.glob.oc) {
                for (Gob gob : ui.sess.glob.oc) {
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
        } catch (Exception q) {
            q.printStackTrace();
        }
        return nearest;
    }

    public static Gob findNearestGob(UI ui, int radius, java.util.List<Gob> blacklist, String... names) {
        Coord2d plc = player(ui).rc;
        double min = radius;
        Gob nearest = null;
        try {
            synchronized (ui.sess.glob.oc) {
                for (Gob gob : ui.sess.glob.oc) {
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
        } catch (Exception q) {
            q.printStackTrace();
        }
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
    public static void mapInteractClick(UI ui) {
        ui.gui.map.wdgmsg("itemact", getCenterScreenCoord(ui), player(ui).rc.floor(posres), 3, ui.modflags());
    }

    public static void mapInteractLeftClick(UI ui, int mod) {
        ui.gui.map.wdgmsg("click", getCenterScreenCoord(ui), player(ui).rc.floor(posres), 1, ui.modflags());
    }

    // Destroys the given gob
    public static void destroyGob(UI ui, Gob gob) {
        ui.gui.menu.wdgmsg("act", new Object[]{"destroy"});
        doClick(ui, gob, 1, 0);
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
    public static int invFreeSlots(UI ui) {
        int takenSlots = 0;
        for (Widget i = ui.gui.maininv.child; i != null; i = i.next) {
            if (i instanceof WItem) {
                WItem buf = (WItem) i;
                takenSlots += buf.size().x * buf.size().y;
            }
        }
        int allSlots = ui.gui.maininv.isz.x * ui.gui.maininv.isz.y;
        return allSlots - takenSlots;
    }

    public static void sysLogAppend(UI ui, String msg, String clr) {
        try {
            Field field = Color.class.getField(clr);
            Color clr2 = (Color) field.get(null);
            ui.gui.syslog.append(msg, clr2);
        } catch (Exception e) {
        }
    }

    public static int getState(Fightview fv, int relation) {
        return fv.lsrel.get(relation).give.state;
    }

    /* Get tile type
     * @param x X-Coordinate
     * @param y Y-Coordinate
     * @return Tile type (integer)
     */
    public static int getTile(UI ui, int x, int y) {
        return ui.sess.glob.map.gettile(new Coord(x, y));
    }

    /* Teleport to you hearthfire
     */
    public static void travelHearth(UI ui) {
        ui.gui.act("travel", "hearth");
    }


    //Will set player speed to whatever int you send it.
    public static void setSpeed(UI ui, int speed) {
        Speedget speedwdg = ui.gui.speedget.get();
        if (speedwdg != null)
            speedwdg.set(speed);
    }

    //should return current max move speed? maybe?
    public static int maxSpeed(UI ui) {
        Speedget speedwdg = ui.gui.speedget.get();
        if (speedwdg != null)
            return speedwdg.max;
        else
            return 0;
    }

    /**
     * Get an amount of something such as seeds in a stack
     *
     * @param item Item to check
     * @return Amount of something in the item
     */
    public static int getAmount(WItem item) {
        int ret = -1;
        for (ItemInfo o : item.item.info()) {
            if (o instanceof GItem.Amount)
                ret = ((GItem.Amount) o).itemnum();
        }
        return ret;
    }

    /**
     * Finds the closest object that matches one of the given names
     *
     * @param radius Radius to look for objects in tiles
     * @param names  Name(s) of gobs to look for
     * @return Gob of the object, or null if not found
     */
    public static Gob findObjectByNames(UI ui, double radius, String... names) {
        Coord2d plc = player(ui).rc;
        double min = radius;
        Gob nearest = null;
        synchronized (ui.sess.glob.oc) {
            for (Gob gob : ui.sess.glob.oc) {
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
     *
     * @param gob Gob to get id of
     * @return Id of the gob
     */
    public static long getGobId(Gob gob) {
        return gob.id;
    }

    /**
     * Returns the name of the gobs resource, or null if not found
     *
     * @param gob Gob to get name of
     * @return Name of the gob
     */
    public static String getGobName(Gob gob) {
        if (gob.getres() != null)
            return gob.getres().name;
        else
            return null;
    }

    /**
     * Get the player stamina
     *
     * @return Returns 0-100
     */
    public static int getStamina(UI ui) {
        return ui.gui.getmeter("stam", 0).a;
    }

    /**
     * Get the player energy
     *
     * @return Returns 0-100
     */
    public static int getEnergy(UI ui) {
        return ui.gui.getmeter("nrj", 0).a;
    }

    /**
     * Get the player hp
     *
     * @return Returns 0-100
     */
    public static int getShp(UI ui) {
        return ui.gui.getmeter("hp", 0).a;
    }

    /**
     * Check if the object is moving
     *
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
     *
     * @param gob Gob to right click
     * @param mod 1 = shift, 2 = ctrl, 4 = alt
     */
    public static void pfRightClick(UI ui, Gob gob, int mod) {
        ui.gui.map.purusPfRightClick(gob, -1, 3, mod, null);
    }

    /**
     * Itemact with gob, to fill trough with item in hand for example
     *
     * @param gob Gob to interact with
     * @param mod 1 = shift, 2 = ctrl, 4 = alt
     */
    public static void itemClick(UI ui, Gob gob, int mod) {
        ui.gui.map.wdgmsg("itemact", Coord.z, gob.rc.floor(posres), mod, 0, (int) gob.id, gob.rc.floor(posres), 0, -1);
    }

    /**
     * Returns the player gob
     *
     * @return Player gob
     */
    public static Gob player(UI ui) {
        if (ui.gui != null && ui.gui.map != null) {
            if (ui.gui.map.player() != null)
                return ui.gui.map.player();
            else
                return null;
        } else
            return null;
    }

    /**
     * Returns contents of an item
     *
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
     *
     * @param window Window to look inventory from
     * @return Inventory of the window or null if not found
     */
    public static Inventory getInventory(Window window) {
        for (Widget wdg = window.lchild; wdg != null; wdg = wdg.prev) {
            if (wdg instanceof Inventory) {
                return (Inventory) wdg;
            }
        }
        return null;
    }

    /**
     * Take an item to hand
     *
     * @param item Item to take to hand
     */
    public static void takeItem(UI ui, WItem item) {
        item.item.wdgmsg("take", Coord.z);
        while (getItemAtHand(ui) == null) {
            sleep(50);
        }
    }

    /**
     * Get stage of the crop
     *
     * @param gob Crop to get stage of
     * @return Stage of the crop
     */
    public static int getCropStage(Gob gob) {
        return gob.getStage();
    }

    /**
     * Makes a stockpile from the item in the hand
     */
    public static void makePile(UI ui) {
        ui.gui.map.wdgmsg("itemact", getCenterScreenCoord(ui), player(ui).rc.floor(posres), 0);
    }

    /**
     * Use to place something, for example, a stockpile
     * 11 offset = 1 tile
     *
     * @param x Offset from player to place stockpile to
     * @param y Offset from player to place stockpile to
     */
    public static void placeThing(UI ui, int x, int y) {
        ui.gui.map.wdgmsg("place", player(ui).rc.add(x, y).floor(posres), 0, 1, 0);
    }

    /**
     * Destroys the given gob
     *
     * @param gob Gob to destroy
     */
    public static void liftGob(UI ui, Gob gob) {
        ui.gui.menu.wdgmsg("act", new Object[]{"carry"});
        doClick(ui, gob, 1, 0);
    }

    /**
     * Drops an item from the hand
     *
     * @param mod 1 = shift, 2 = ctrl, 4 = alt
     */
    public static void dropItem(UI ui, int mod) {
        ui.gui.map.wdgmsg("drop", Coord.z, ui.gui.map.player().rc.floor(posres), mod);
    }

    /**
     * Drops the given item from the inventory
     *
     * @param item Item to drop
     */
    public static void dropItemFromInventory(WItem item) {
        item.item.wdgmsg("drop", Coord.z);
    }

    /**
     * Wait for a window with a specific name to appear
     *
     * @param windowName Name of the window
     */
    public static void waitForWindow(UI ui, String windowName) {
        while (ui.gui.getwnd(windowName) == null) {
            sleep(50);
        }
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
     * Returns item with the specific name, or null if not found
     *
     * @param invwdg Inventory to look items from
     * @param items  Name(s) of the items to look for
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
     *
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
    public static List<WItem> getallInventoryContents(UI ui) {
        java.util.List<WItem> witems = new ArrayList<WItem>();
        synchronized (ui.root.lchild) {
            try {
                for (Widget q = ui.root.lchild; q != null; q = q.rnext()) {
                    if (q instanceof Inventory) {
                        witems.addAll(getInventoryContents((Inventory) q));
                    }
                }
            } catch (Exception e) {
            }
        }
        return witems;
    }

    //same as above for returns a list of all WItems from all inventories seen on screen
    public static List<WItem> getallInventoryContentsbyString(UI ui, String witem) {
        java.util.List<WItem> witems = new ArrayList<WItem>();
        List<WItem> finallist = new ArrayList<>();
        synchronized (ui.root.lchild) {
            try {
                for (Widget q = ui.root.lchild; q != null; q = q.rnext()) {
                    if (q instanceof Inventory) {
                        witems.addAll(getInventoryContents((Inventory) q));
                    }
                }
            } catch (Exception e) {
            }
        }
        for (WItem item : witems) {
            if (item.item.getname().contains(witem))
                finallist.add(item);
        }
        return finallist;
    }

    public static List<WItem> getPlayerInvContentsPartial(UI ui, String witem) {
        List<WItem> witems = new ArrayList<>();
        List<WItem> finallist = new ArrayList<>();
        witems.addAll(getInventoryContents(ui.gui.maininv));
        for (WItem item : witems) {
            if (item.item.getname().contains(witem)) {
                finallist.add(item);
            }
        }
        return finallist;
    }

    public static List<WItem> getPlayerInvContentsExact(UI ui, String witem) {
        List<WItem> witems = new ArrayList<>();
        List<WItem> finallist = new ArrayList<>();
        witems.addAll(getInventoryContents(ui.gui.maininv));
        for (WItem item : witems) {
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
    //	ui.gui.act("lo", "cs");
    //}
    public static void logout(UI ui) {
        if (haven.automation.Discord.jdalogin != null)
            ui.gui.DiscordToggle();
        ui.gui.act("lo");
    }

    public static void logoutChar(UI ui) {
        if (Discord.jdalogin != null)
            ui.gui.DiscordToggle();
        ui.gui.act("lo", "cs");
    }

    /**
     * Check if stockpile is full
     *
     * @param gob Stockpile gob to check
     * @return True if stockpile is full, else false
     */
    public static boolean stockpileIsFull(Gob gob) {
        if (gob.sdt() == 31)
            return true;
        else
            return false;
    }

    /**
     * Amount of free slots in the inventory
     *
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
     *
     * @param item Item to check
     * @return True if player can drink, else false
     */
    public static boolean canDrinkFrom(WItem item) {
        Pattern liquidPattern = Pattern.compile(String.format("[0-9.]+ l of (%s)",
                //	String.join("|", new String[] { "Water", "Piping Hot Tea", "Tea" }), Pattern.CASE_INSENSITIVE));
                String.join("|", new String[]{"Water", "Piping Hot Tea", "Tea", "Milk", "Cowsmilk", "Sheepsmilk", "Goatsmilk"}), Pattern.CASE_INSENSITIVE));
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
     *
     * @param item Item to activate
     */
    public static void activateItem(WItem item) {
        item.item.wdgmsg("iact", Coord.z, 3);
    }

    /**
     * List of all gobs visible to the client
     *
     * @return List of all gobs
     */
    public static java.util.List<Gob> getGobs(UI ui) {
        List<Gob> list = new ArrayList<Gob>();
        synchronized (ui.sess.glob.oc) {
            for (Gob gob : ui.sess.glob.oc)
                list.add(gob);
        }
        return list;
    }

    /**
     * Send act message to server
     * Act can be used for example to choose a cursor
     * Some acts:
     * dig, mine, carry, destroy, fish, inspect, repair, crime, swim, tracking, aggro, shoot
     *
     * @param act Act to choose
     */
    public static void doAct(UI ui, String act) {
        ui.gui.menu.wdgmsg("act", act);
    }

    /**
     * Returns coords of the gob
     *
     * @param gob Gob to return coordinates of
     * @return Coords of the gob
     */
    public static Coord2d getCoords(Gob gob) {
        return gob.rc;
    }

    /**
     * Transfer an item to the active inventory
     *
     * @param item Item to transfer
     */
    public static void transferItem(WItem item) {
        item.item.wdgmsg("transfer", Coord.z);
    }

    public static void pfmove(UI ui, int x, int y) {
        ui.gui.map.pathto(new Coord2d(x, y));
    }

    public static void pfmovegob(UI ui, PBotGob gob) {
        ui.gui.map.pathto(gob.gob);
    }

    public static void pfmovegob(UI ui, Gob gob) {
        ui.gui.map.pathto(gob);
    }

    public static String getRes(PBotGob gob) {
        return gob.gob.getres().name;
    }

    public static boolean checkName(PBotGob gob, String s) {
        if (gob.gob.getres().name.contains(s)) {
            return true;
        } else return false;
    }

    static double returnX(PBotGob a) {
        return a.gob.rc.x;
    }

    static double returnY(PBotGob a) {
        return a.gob.rc.y;
    }

}
