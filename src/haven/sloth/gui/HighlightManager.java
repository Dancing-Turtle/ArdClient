package haven.sloth.gui;

import haven.*;
import haven.Button;
import haven.Window;
import haven.sloth.gob.Alerted;
import haven.sloth.io.HighlightData;
import haven.sloth.util.ObservableListener;

import java.awt.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

public class HighlightManager extends Window implements ObservableListener<String> {
    private List<String> highlighted = new ArrayList<>();
    private final TextEntry manualin;
	private final Listbox<String> defaults;
	private HashMap<String, String> defaultlist = new HashMap<>();
	private List<String> shortvers = new ArrayList<>();

    public HighlightManager() {
	super(Coord.z, "Highlight Manager", "Highlight Manager");
	Coord c = new Coord(0, 0);
	final Listbox<String> lst;
	c.x += add(lst = new Listbox<String>(200, 20, 20) {
	    @Override
	    protected String listitem(int i) {
		return highlighted.get(i);
	    }

	    @Override
	    protected int listitems() {
		return highlighted.size();
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
							HighlightData.add(k);
						}
					});
				}
				super.change(item);
			}
	}, c.copy()).sz.y + 5;
		Coord bc = c.copy();
		manualin = add(new TextEntry(200, "", null, (res) -> {
			HighlightData.add(res);
			ui.sess.glob.oc.highlightGobs(res);
		}), new Coord(0,c.copy().y+5));
		c.y += manualin.sz.y;
		add(new Button(200, "Add Highlight", () -> {
			if(!manualin.text.equals("")) {
				HighlightData.add(manualin.text);
				ui.sess.glob.oc.highlightGobs(manualin.text);
			}
		}), new Coord(0,c.copy().y+5));
		add(new Button(200, "Stop Highlighting", () -> {
			if(lst.sel != null) {
				HighlightData.remove(lst.sel);
				ui.sess.glob.oc.unhighlightGobs(lst.sel);
			}
		}), new Coord(0,c.copy().y+27));

	pack();
	HighlightData.listen(this);
    }

    @Override
    public void close() {
	hide();
    }

    @Override
    protected void removed() {
	HighlightData.unlisten(this);
    }

    @Override
    public void init(Collection<String> base) {
	highlighted.addAll(base);
	highlighted.sort(String::compareTo);
	defaultlist = Config.defaultitems;
	shortvers.addAll(defaultlist.values());
	shortvers.sort(String::compareTo);
    }

    @Override
    public void added(String item) {
	highlighted.add(item);
	highlighted.sort(String::compareTo);
    }

    @Override
    public void remove(String item) {
	highlighted.remove(item);
    }
}
