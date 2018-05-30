package haven;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class CheckListbox extends Listbox<CheckListboxItem> {
    private static final Tex chk = Resource.loadtex("gfx/hud/chkmarks");
    public List<CheckListboxItem> items = new ArrayList<CheckListboxItem>() {
        @Override
        public boolean add(CheckListboxItem value) {
            super.add(value);
            for (int i = size()-1; i > 0 && value.compareTo(get(i-1)) < 0; i--)
                Collections.swap(this, i, i-1);
            return true;
        }
    };

    public CheckListbox(int w, int h) {
        super(w, h, 18);
    }

    public CheckListbox(int w, int h, int itemh) {
        super(w, h, itemh);
    }

    protected void itemclick(CheckListboxItem itm, int button) {
        if (button == 1) {
            itm.selected = !itm.selected;
            super.itemclick(itm, button);
        }
    }

    protected CheckListboxItem listitem(int idx) {
        return (items.get(idx));
    }

    protected int listitems() {
        return items.size();
    }

    public void drawbg(GOut g) {
        g.chcolor(0, 0, 0, 128);
        g.frect(Coord.z, sz);
        g.chcolor();
    }

    protected void drawitem(GOut g, CheckListboxItem itm, int idx) {
        if (itm.selected)
            g.image(chk, new Coord(sz.x - sb.sz.x - chk.sz().x - 3, -1), new Coord(itemh, itemh));
        Text t = Text.render(itm.name);
        Tex T = t.tex();
        g.image(T, new Coord(2, 2), t.sz());
        T.dispose();
    }

    public String[] getselected() {
        List<String> sitems = new ArrayList<String>();
        for (CheckListboxItem itm : items) {
            if (itm.selected)
                sitems.add(itm.name);
        }
        return sitems.toArray(new String[sitems.size()]);
    }
}