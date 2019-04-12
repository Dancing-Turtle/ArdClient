package haven.purus.pbot.gui;

import haven.Coord;
import haven.Widget;
import haven.Window;
import haven.purus.pbot.PBotError;
import haven.purus.pbot.PBotScriptmanager;

public class PBotWindow extends Window {

	private String id;
	private boolean closed = false;

	public PBotWindow(Coord sz, String cap, String id) {
		super(sz, cap);
		this.id = id;
	}
	
	/**
	 * Add a button to the window
	 * Invokes function with the given name when clicked
	 * @param name Name of function to call when button is activated
	 * @param label Label of the button
	 * @param width Width of the button
	 * @param x X-Coordinate of the button
	 * @param y Y-Coordinate of the button
	 * @return PBotButton object
	 */
	public PBotButton addButton(String name, String label, int width, int x, int y) {
		PBotButton button = new PBotButton(width, label) {
			@Override
			public void click() {
				Thread t = new Thread(new Runnable() {
					@Override
					public void run() {
						try {
							PBotScriptmanager.getScript(id).context.eval("js", name + "();");
						} catch(Exception e) {
							PBotError.handleException(e);
						}
					}
				});
				t.start();
			}
		};
		add(button, new Coord(x, y));
		return button;
	}
	
	/**
	 * Add a checkbox to the window
	 * Invokes function with the given name with boolean as argument
	 * @param name Name of function to call when checkbox is ticked, value of the checkbox is used as an argument
	 * @param label Label of the checkbox
	 * @param initialState Initial state of the checkbox
	 * @param x X-Coordinate of the checkbox
	 * @param y Y-Coordinate of the checkbox
	 * @return PBotCheckbox object
	 */
	public PBotCheckbox addCheckbox(String name, String label, boolean initialState, int x, int y) {
		PBotCheckbox checkbox = new PBotCheckbox(label, initialState) {
			@Override
            public void set(boolean val) {
            	a = val;
				Thread t = new Thread(new Runnable() {
					@Override
					public void run() {
						PBotScriptmanager.getScript(id).context.eval("js", name + "(" + (val ? "true" : "false") +");");
					}
				});
				t.start();
            }
		};
		add(checkbox, new Coord(x, y));
		return checkbox;
	}
	
	/**
	 * Add a label to the window
	 * @param text Text in the label
	 * @param x X-Coordinate of the checkbox
	 * @param y Y-Coordinate of the checkbox
	 * @return PBotLabel object
	 */
	public PBotLabel addLabel(String text, int x, int y) {
		PBotLabel label = new PBotLabel(text);
		add(label, new Coord(x, y));
		return label;
	}

	/**
	 * Add a TextEntry box to the window
	 * @param width Width of the box
	 * @param initialText Initial text of the box
	 * @param x X-Coordinate of the box
	 * @param y Y-Coordinate of the box
	 * @return Returns TextEntry object
	 */
	public PBotTextEntry addTextEntry(int width, String initialText, int x, int y) {
		PBotTextEntry te = new PBotTextEntry(width, initialText);
		add(te, new Coord(x,y));
		return te;
	}
	
	/**
	 * Has the window been closen by the user or program?
	 * @return True if the window has been closen
	 */
	public boolean closed() {
		return closed;
	}

	/**
	 * Closes the window
	 */
	public void closeWindow() {
		reqdestroy();
		closed = true;
	}

	@Override
	public void close(){
		reqdestroy();
		closed = true;
	}

	@Override
	public void wdgmsg(Widget sender, String msg, Object... args) {
		if(sender == cbtn) {
			reqdestroy();
			closed = true;
		} else {
			super.wdgmsg(sender, msg, args);
		}
	}

}
