package haven.sloth.gui;

import haven.*;
import haven.sloth.gob.Deleted;
import haven.sloth.util.ObservableListener;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class DeletedManager extends Window implements ObservableListener<String> {
    private List<String> deleted = new ArrayList<>();

    public DeletedManager() {
        super(Coord.z, "Deleted Manager", "Deleted Manager");
        Coord c = new Coord(0, 0);
        final Listbox<String> lst;
	c.y += add(lst = new Listbox<String>(200, 20, 20) {
	    @Override
	    protected String listitem(int i) {
		return deleted.get(i);
	    }

	    @Override
	    protected int listitems() {
		return deleted.size();
	    }

	    @Override
	    protected void drawitem(GOut g, String item, int i) {
		g.text(item, new Coord(5, 1));
	    }
	}, c.copy()).sz.y;
	add(new Button(200, "Stop Deleting", () -> {
	    if(lst.sel != null) {
		Deleted.remove(lst.sel);
	    }
	}), c.copy());
	pack();
	Deleted.listen(this);
    }

    @Override
    public void close() {
        hide();
    }

    @Override
    protected void removed() {
	Deleted.unlisten(this);
    }

    @Override
    public void init(Collection<String> base) {
	deleted.addAll(base);
	deleted.sort(String::compareTo);
    }

    @Override
    public void added(String item) {
	deleted.add(item);
	deleted.sort(String::compareTo);
    }

    @Override
    public void remove(String item) {
	deleted.remove(item);
    }
}
