package haven.purus.pbot;

import haven.ChatUI;
import haven.Coord2d;
import haven.GameUI;
import haven.Widget;
import haven.automation.Discord;

import static haven.OCache.posres;

public class PBotCharacterAPI {

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
	 * Cancels the current act
	 */
	public static void cancelAct() {
		PBotAPI.gui.map.wdgmsg("click", PBotUtils.getCenterScreenCoord(), new Coord2d(0, 0).floor(posres), 3, 0);

	}

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
	 * Send message to given chat
	 * @param chatName Name of the chat, for example "Area Chat"
	 * @param msg Message to send into the chat
	 */
	public static void msgToChat(String chatName, String msg) {
		for(Widget w = PBotAPI.gui.chat.lchild; w != null; w = w.prev) {
			if(w instanceof ChatUI.EntryChannel) {
				ChatUI.EntryChannel cht = (ChatUI.EntryChannel) w;
				if(cht.name().equals(chatName))
					cht.send(msg);
			}
		}
	}
}
