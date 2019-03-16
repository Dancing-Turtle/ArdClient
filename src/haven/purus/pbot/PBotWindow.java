package haven.purus.pbot;

import javax.script.Invocable;
import javax.script.ScriptException;

import haven.*;

public class PBotWindow extends Window {

	private String id;
	
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
	 */
	public Button addButton(String name, String label, int width, int x, int y) {
		Button button = new Button(width, label) {
			@Override
			public void click() {
				Thread t = new Thread(new Runnable() {
					@Override
					public void run() {
						try {
							((Invocable)PBotScriptmanager.getScript(id).getEngine()).invokeFunction(name, name);
						} catch (NoSuchMethodException | ScriptException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
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
	 * @param name Name of function to call when checkbox is ticked
	 * @param label Label of the checkbox
	 * @param initialState Initial state of the checkbox
	 * @param x X-Coordinate of the checkbox
	 * @param y Y-Coordinate of the checkbox
	 */
	public CheckBox addCheckbox(String name, String label, boolean initialState, int x, int y) {
		CheckBox checkbox = new CheckBox(label, initialState) {
			@Override
            public void set(boolean val) {
            	a = val;
				Thread t = new Thread(new Runnable() {
					@Override
					public void run() {
						try {
							((Invocable)PBotScriptmanager.getScript(id).getEngine()).invokeFunction(name, val);
						} catch (NoSuchMethodException | ScriptException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
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
	 */
	public Label addLabel(String text, int x, int y) {
		Label label = new Label(text);
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
	public TextEntry addTextEntry(int width, String initialText, int x, int y) {
		TextEntry te = new TextEntry(width, initialText);
		add(te, new Coord(x,y));
		return te;
	}

	/**
	 * Closes the window
	 */
	public void closeWindow() {
		reqdestroy();
	}
	
	@Override
	public void wdgmsg(Widget sender, String msg, Object... args) {
		if (sender == cbtn)
			reqdestroy();
		else
			super.wdgmsg(sender, msg, args);
	}

}
