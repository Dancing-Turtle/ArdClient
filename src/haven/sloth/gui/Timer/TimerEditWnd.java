package haven.sloth.gui.Timer;


import haven.*;
import haven.sloth.io.TimerData;

import java.awt.event.KeyEvent;

public class TimerEditWnd extends Window {
    TimerEditWnd(String cap) {
	super(new Coord(355, 100), cap, cap);

	add(new Label("Name"), new Coord(15, 10));
	final TextEntry txtname = new TextEntry(200, "");
	add(txtname, new Coord(15, 30));

	add(new Label("Hours"), new Coord(225, 10));
	final TextEntry txthours = new TextEntry(50, "") {
	    @Override
	    public boolean type(char c, KeyEvent ev) {
		if (c == 0x8 || c == 0x7f || c == 0x09 || (c >= 0x30 && c <= 0x39 && text.length() <= 2))
		    return super.type(c, ev);
		return true;
	    }
	};
	add(txthours, new Coord(225, 30));

	add(new Label("Minutes"), new Coord(285, 10));
	final TextEntry txtminutes = new TextEntry(50,"") {
	    @Override
	    public boolean type(char c, KeyEvent ev) {
		if (c == 0x8 || c == 0x7f || c == 0x09 || (c >= 0x30 && c <= 0x39 && text.length() <= 1))
		    return super.type(c, ev);
		return true;
	    }
	};
	add(txtminutes, new Coord(285, 30));

	Button add = new Button(60, "Add", () -> {
	    long hours = Long.parseLong(txthours.text.equals("") ? "0" : txthours.text);
	    long minutes = Long.parseLong(txtminutes.text.equals("") ? "0" : txtminutes.text);
	    long duration = ((60 * hours + minutes) * 60) * 3;
	    TimerData.addTimer(txtname.text, duration);
	    ui.destroy(this);
	});
	add(add, new Coord(15, 70));

	Button cancel = new Button(60, "Cancel") {
	    @Override
	    public void click() {
		parent.reqdestroy();
	    }
	};
	add(cancel, new Coord(275, 70));
    }

    public void close() {
	ui.destroy(this);
    }
}
