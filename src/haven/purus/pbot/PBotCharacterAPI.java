package haven.purus.pbot;

import haven.ChatUI;
import haven.Coord2d;
import haven.UI;
import haven.Widget;
import haven.automation.Discord;

import static haven.OCache.posres;

public class PBotCharacterAPI {

    /**
     * Description for all method
     * @param ui    UI for this script
     */

    /**
     * Get the player stamina
     *
     * @return Returns 0-100
     */
    public static int getStamina(UI ui) {
        return ui.gui.getmeter("stam", 0).a;
    }

    public static int getStamina() {
        return getStamina(PBotAPI.modeui());
    }

    /**
     * Get the player energy
     *
     * @return Returns 0-100
     */
    public static int getEnergy(UI ui) {
        return ui.gui.getmeter("nrj", 0).a;
    }

    public static int getEnergy() {
        return getEnergy(PBotAPI.modeui());
    }

    /**
     * Get the player hp
     *
     * @return Returns 0-100
     */
    public static int getShp(UI ui) {
        return ui.gui.getmeter("hp", 0).a;
    }

    public static int getShp() {
        return getShp(PBotAPI.modeui());
    }

    /**
     * Get the player hp
     *
     * @return Returns 0-100
     */
    public static int getHp(UI ui) {
        return ui.gui.getmeter("hp", 0).a;
    }

    public static int getHp() {
        return getHp(PBotAPI.modeui());
    }

    /**
     * Send act message to server
     * Act can be used for example to choose a cursor
     * Some acts:
     * dig, mine, carry, destroy, fish, inspect, repair, crime, swim, tracking, aggro, shoot
     *
     * @param act Act(s) to choose
     */
    public static void doAct(UI ui, String... act) {
        ui.gui.menu.wdgmsg("act", act);
    }

    public static void doAct(String... act) {
        doAct(PBotAPI.modeui(), act);
    }

    /**
     * Cancels the current act by clicking
     */
    public static void cancelAct(UI ui) {
        ui.gui.map.wdgmsg("click", PBotUtils.getCenterScreenCoord(ui), new Coord2d(0, 0).floor(posres), 3, 0);
    }

    public static void cancelAct() {
        cancelAct(PBotAPI.modeui());
    }

    public static void logout(UI ui) {
        if (haven.automation.Discord.jdalogin != null)
            ui.gui.DiscordToggle();
        ui.gui.act("lo");
    }

    public static void logout() {
        logout(PBotAPI.modeui());
    }

    public static void logoutChar(UI ui) {
        if (Discord.jdalogin != null)
            ui.gui.DiscordToggle();
        ui.gui.act("lo", "cs");

    }

    public static void logoutChar() {
        logoutChar(PBotAPI.modeui());
    }


    /**
     * Log in with a character from character login menu
     *
     * @param charname Name of the character
     */
    public static void loginChar(UI ui, String charname) {
        ui.charlist.wdgmsg("play", charname);
    }

    public static void loginChar(String charname) {
        loginChar(PBotAPI.modeui(), charname);
    }

    /**
     * Set player speed setting
     *
     * @param speed 1 = crawl, 2 = walk, 3 = run, 4 = sprint
     */
    public static void setSpeed(UI ui, int speed) {
        ui.gui.speed.set(speed);
    }

    public static void setSpeed(int speed) {
        setSpeed(PBotAPI.modeui(), speed);
    }

    /**
     * Get current speed setting of player
     *
     * @return 1 = crawl, 2 = walk, 3 = run, 4 = sprint
     */
    public static int getSpeed(UI ui) {
        return ui.gui.speed.cur;
    }

    public static int getSpeed() {
        return getSpeed(PBotAPI.modeui());
    }

    /**
     * Get maximum speed setting that player can be set to
     *
     * @return 1 = crawl, 2 = walk, 3 = run, 4 = sprint
     */
    public static int getMaxSpeed(UI ui) {
        return ui.gui.speed.max;
    }

    public static int getMaxSpeed() {
        return getMaxSpeed(PBotAPI.modeui());
    }

    /**
     * Send message to given chat
     *
     * @param chatName Name of the chat, for example "Area Chat"
     * @param msg      Message to send into the chat
     */
    public static void msgToChat(UI ui, String chatName, String msg) {
        for (Widget w = ui.gui.chat.lchild; w != null; w = w.prev) {
            if (w instanceof ChatUI.EntryChannel) {
                ChatUI.EntryChannel cht = (ChatUI.EntryChannel) w;
                if (cht.name().equals(chatName))
                    cht.send(msg);
            }
        }
    }

    public static void msgToChat(String chatName, String msg) {
        msgToChat(PBotAPI.modeui(), chatName, msg);
    }
}
