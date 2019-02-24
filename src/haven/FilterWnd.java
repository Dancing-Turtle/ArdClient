package haven;

import java.awt.event.KeyEvent;

public class FilterWnd extends GameUI.Hidewnd {
    private final TextEntry input;
    
    FilterWnd() {
	super(new Coord(120, 200), "Filter");
	//cap = null;
	
	input = add(new TextEntry(200, "") {
	    @Override
	    protected void changed() {
		checkInput();
	    }
	});
    
	addtwdg(add(new IButton("gfx/hud/btn-help", "","-d","-h"){
	    @Override
	    public void click() {
		ItemFilter.showHelp(ui, ItemFilter.FILTER_HELP);
	    }
	}));
	
	pack();
	hide();
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
