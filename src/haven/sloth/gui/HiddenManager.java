package haven.sloth.gui;

import haven.*;
import haven.Button;
import haven.Window;
import haven.sloth.gob.Alerted;
import haven.sloth.gob.Hidden;
import haven.sloth.util.ObservableListener;

import java.awt.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

public class HiddenManager extends Window implements ObservableListener<String> {
    private List<String> hidden = new ArrayList<>();
	private final Listbox<String> defaults;
	private HashMap<String, String> defaultlist = new HashMap<>();
	private List<String> shortvers = new ArrayList<>();

    public HiddenManager() {
	super(Coord.z, "Hidden Manager", "Hidden Manager");
	Coord c = new Coord(0, 0);
	final Listbox<String> lst;
	c.x += add(lst = new Listbox<String>(200, 20, 20) {
	    @Override
	    protected String listitem(int i) {
		return hidden.get(i);
	    }

	    @Override
	    protected int listitems() {
		return hidden.size();
	    }

	    @Override
	    protected void drawitem(GOut g, String item, int i) {
		g.text(item, new Coord(5, 1));
	    }
	}, c.copy()).sz.x;
		c.y += add(defaults = new Listbox<String>(200, 20, 20) {
			@Override
			protected String listitem(int i) {
				return shortvers.get(i);
			}

			@Override
			protected int listitems() { return shortvers.size(); }

			@Override
			protected void drawitem(GOut g, String item, int i) { g.text(item, new Coord(5, 1)); }

			@Override
			public void change(String item){
				if(sel != null && sel.equals(item)){
					System.out.println("doubleclick : "+item);
					defaultlist.forEach((k,v) -> {
						if(v.equals(item)){
							Hidden.add(k);
						}
					});
				}
				super.change(item);
			}
		}, c.copy()).sz.y + 5;
		add(new Button(200, "Stop Hiding", () -> {
			if(lst.sel != null) {
				Hidden.remove(lst.sel);
				ui.sess.glob.oc.unhideAll(lst.sel);
			}
		}), new Coord(0,c.copy().y+5));

	pack();
	Hidden.listen(this);
    }

    @Override
    public void close() {
        hide();
    }

    @Override
    protected void removed() {
	Hidden.unlisten(this);
    }

    @Override
    public void init(Collection<String> base) {
	hidden.addAll(base);
	hidden.sort(String::compareTo);
	defaultlist = Config.defaultitems;
	shortvers.addAll(defaultlist.values());
	shortvers.sort(String::compareTo);
    }

    @Override
    public void added(String item) {
	hidden.add(item);
	hidden.sort(String::compareTo);
    }

    @Override
    public void remove(String item) {
	hidden.remove(item);
    }
}
