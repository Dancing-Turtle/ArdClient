package haven.purus.pbot.gui;

import haven.Label;

import java.awt.*;

public class PBotLabel extends Label {

	/**
	 * Created via PBotWindow
	 */
	public PBotLabel(String text) {
		super(text);
	}

	/**
	 * Change the text of the label
	 * @param text Text to change to
	 */
	public void setText(String text) {
		super.settext(text);
	}

	/**
	 * Change color of the label
	 * @param r Amount of red color, between 0-255
	 * @param g Amount of green color, between 0-255
	 * @param b Amount of blue color, between 0-255
	 */
	public void setColor(int r, int g, int b) {
		super.setcolor(new Color(r, g, b));
	}
}
