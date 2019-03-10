package haven;

import java.awt.event.KeyEvent;

public class FilterWnd extends Window {
    private final TextEntry input;
    FilterWnd(String caption) {
	super(Coord.z, caption, caption);
	//cap = null;
	
	input = add(new TextEntry(200, "") {
	    @Override
	    protected void changed() {
		checkInput();
	    }
	});

		addBtn_base("gfx/hud/helpbtn", "Show Filter Help", () -> ItemFilter.showHelp(ui, ItemFilter.FILTER_HELP));

	pack();
    }
    
    @Override
    public boolean type(char key, KeyEvent ev) {
	return !ignoredKey(ev) && super.type(key, ev);
    }
    
    @Override
    public boolean keydown(KeyEvent ev) {
	return !ignoredKey(ev) && super.keydown(ev);
    }
    
    private static boolean ignoredKey(KeyEvent ev){
	int code = ev.getKeyCode();
	int mods = ev.getModifiersEx();
	//any modifier except SHIFT pressed alone is ignored, TAB is also ignored
	return (mods != 0 && mods != KeyEvent.SHIFT_DOWN_MASK)
		|| code == KeyEvent.VK_CONTROL
		|| code == KeyEvent.VK_ALT
		|| code == KeyEvent.VK_META
		|| code == KeyEvent.VK_TAB;
    }
    
    private void setFilter(String text) {
	ItemFilter filter = null;
	if (text != null) {
	    filter = ItemFilter.create(text);
	}
	GItem.setFilter(filter);
    }
    
    private void checkInput() {
	if (input.text.length() >= 2) {
	    setFilter(input.text);
	} else {
	    setFilter(null);
	}
    }
    
    @Override
    public void hide() {
	super.hide();
	setFilter(null);
    }

	@Override
	protected void added() {
		super.added();
	}

	public void close() { hide(); }
    
    @Override
    public void show() {
	super.show();
	setfocus(input);
	checkInput();
	raise();
    }
    
    public void toggle() {
    	show(!visible);
    }
}
