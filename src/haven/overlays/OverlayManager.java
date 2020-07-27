package haven.overlays;

import haven.Button;
import haven.Config;
import haven.Coord;
import haven.GOut;
import haven.Listbox;
import haven.TextEntry;
import haven.Window;
import haven.sloth.util.ObservableListener;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

public class OverlayManager extends Window implements ObservableListener<OverlayData.OverlayGob> {
    private List<String> highlighted = new ArrayList<>();
    private final TextEntry manualin;
    private final Listbox<String> defaults;
    private HashMap<String, String> defaultlist = new HashMap<>();
    private List<String> shortvers = new ArrayList<>();

    public OverlayManager() {
        super(Coord.z, "Overlay Manager", "Overlay Manager");
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

            @Override
            public void change(String item) {
                if (sel != null && sel.equals(item)) {
                    ui.gui.add(new OverlaySelector(item), ui.mc);
                }
                super.change(item);
            }
        }, c.copy()).sz.x;
        c.y += add(defaults = new Listbox<String>(200, 20, 20) {
            @Override
            protected String listitem(int i) {
                return shortvers.get(i);
            }

            @Override
            protected int listitems() {
                return shortvers.size();
            }

            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, new Coord(5, 1));
            }

            @Override
            public void change(String item) {
                if (sel != null && sel.equals(item)) {
                    defaultlist.forEach((k, v) -> {
                        if (v.equals(item)) {
                            ui.gui.add(new OverlaySelector(v), ui.mc);
                        }
                    });
                }
                super.change(item);
            }
        }, c.copy()).sz.y + 5;
        Coord bc = c.copy();
        manualin = add(new TextEntry(200, "", null, (res) -> {
            ui.gui.add(new OverlaySelector(res), ui.mc);
        }), new Coord(0, c.copy().y + 5));
        c.y += manualin.sz.y;
        add(new Button(200, "Add Overlay", () -> {
            if (!manualin.text.equals("")) {
                ui.gui.add(new OverlaySelector(manualin.text), ui.mc);
            }
        }), new Coord(0, c.copy().y + 5));
        add(new Button(200, "Remove", () -> {
            if (lst.sel != null) {
                OverlayData.remove(lst.sel);
                highlighted.remove(lst.sel);
//                ui.sess.glob.oc.unovHighGobs(lst.sel);
                ui.sess.glob.oc.unovTextGobs(lst.sel);
            }
        }), new Coord(0, c.copy().y + 27));

        pack();
        OverlayData.listen(this);
    }

    @Override
    public void close() {
        hide();
    }

    @Override
    protected void removed() {
        OverlayData.unlisten(this);
    }

    @Override
    public void init(Collection<OverlayData.OverlayGob> base) {
        for (OverlayData.OverlayGob og : base) {
            highlighted.add(og.name);
        }
        highlighted.sort(String::compareTo);
        defaultlist = Config.defaultitems;
        shortvers.addAll(defaultlist.values());
        shortvers.sort(String::compareTo);
    }

    @Override
    public void added(OverlayData.OverlayGob item) {
        highlighted.add(item.name);
        highlighted.sort(String::compareTo);
    }

    @Override
    public void remove(OverlayData.OverlayGob item) {
        highlighted.remove(item.name);
    }
}
