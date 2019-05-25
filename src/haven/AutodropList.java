package haven;

import java.util.Collections;
import java.util.Comparator;
import java.util.Map;

import static haven.Config.autodroplist;

public class AutodropList extends WidgetList<AutodropList.Item> {

    public static final Comparator<Item> ITEM_COMPARATOR = new Comparator<Item>() {
	@Override
	public int compare(Item o1, Item o2) {
	    return o1.name.compareTo(o2.name);
	}
    };

    public AutodropList() {
	super(new Coord(200, 25), 10);

	for(Map.Entry<String, Boolean> entry : autodroplist.entrySet()) {
	    additem(new Item(entry.getKey()));
	}

	update();
    }

    @SuppressWarnings("SynchronizeOnNonFinalField")
    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
	switch(msg) {
	    case "changed": {
		String name = (String) args[0];
		boolean val = (Boolean) args[1];
		synchronized(autodroplist) {
			autodroplist.put(name, val);
		}
			Utils.saveAutodropList();
		break;
	    }
	    case "delete": {
		String name = (String) args[0];
		synchronized(autodroplist) {
			autodroplist.remove(name);
		}
			Utils.saveAutodropList();
		removeitem((Item) sender, true);
		update();
		break;
	    }
	    default:
		super.wdgmsg(sender, msg, args);
		break;
	}
    }

    @SuppressWarnings("SynchronizeOnNonFinalField")
    public void add(String name) {
	if(name != null && !name.isEmpty() && !autodroplist.containsKey(name)) {
	    synchronized(autodroplist) {
			autodroplist.put(name, true);
	    }
		Utils.saveAutodropList();
	    additem(new Item(name));
	    update();
	}
    }

    private void update() {
	Collections.sort(list, ITEM_COMPARATOR);
	int n = listitems();
	for(int i = 0; i < n; i++) {
	    listitem(i).c = itempos(i);
	}
    }

    protected static class Item extends Widget {

	public final String name;
	private final CheckBox cb;
	private boolean a = false;
	private UI.Grab grab;

	public Item(String name) {
	    super(new Coord(200, 25));
	    this.name = name;

	    cb = add(new CheckBox(name), 3, 3);
	    cb.a = autodroplist.get(name);
	    cb.canactivate = true;

	    add(new Button(24, "X"){
	    	@Override
			public void click() {
				super.wdgmsg("activate",name);
			}
			@Override
			public boolean mouseup(Coord c, int button) {
				//FIXME:a little hack, because WidgetList does not pass correct click coordinates if scrolled
				return super.mouseup(Coord.z, button);
			}
			}, 175, 0);
	}

	@Override
	public boolean mousedown(Coord c, int button) {
		if(super.mousedown(c, button)) {
			return true;
		}
	    if(button != 1)
		return (false);
	    a = true;
	    grab = ui.grabmouse(this);
	    return (true);
	}

	@Override
	public boolean mouseup(Coord c, int button) {
	    if(a && button == 1) {
		a = false;
		if(grab != null) {
		    grab.remove();
		    grab = null;
		}
		if(c.isect(new Coord(0, 0), sz))
		    click();
		return (true);
	    }
	    return (false);
	}

	private void click() {
	    cb.a = !cb.a;
	    wdgmsg("changed", name, cb.a);
	}

	@Override
	public void wdgmsg(Widget sender, String msg, Object... args) {
	    switch(msg) {
		case "ch":
		    wdgmsg("changed", name, (int) args[0] > 0);
		    break;
		case "activate":
		    wdgmsg("delete", name);
		    break;
		default:
		    super.wdgmsg(sender, msg, args);
		    break;
	    }
	}
    }
}
