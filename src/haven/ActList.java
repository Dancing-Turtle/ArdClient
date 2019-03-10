package haven;

import java.util.*;

public class ActList extends Listbox<ActList.ActItem> {
    private static final Text.Foundry font = new Text.Foundry(Text.serif, 15).aa(true);
    private final List<ActItem> items = new ArrayList<>();
    private final Map<MenuGrid.Pagina, ActItem> map = new HashMap<>();

    ActList(int w, int h) {
        super(w, h, font.height() + 2);
    }

    public void add(MenuGrid.Pagina pagina) {
        ActItem item = new ActItem(pagina);
        map.put(pagina, item);
        items.add(item);
    }

    public void remove(MenuGrid.Pagina pagina) {
        ActItem item = map.remove(pagina);
        if (item != null)
            items.remove(item);
    }

    public void clear() {
        map.clear();
        items.clear();
    }

    public void sort(Comparator<ActItem> comparator) {
        items.sort(comparator);
    }

    @Override
    protected ActItem listitem(int i) {
        return items.get(i);
    }

    @Override
    protected int listitems() {
        return items.size();
    }

    @Override
    protected void drawbg(GOut g) {
        g.chcolor(0, 0, 0, 128);
        g.frect(Coord.z, sz);
        g.chcolor();
    }

    @Override
    public Object tooltip(Coord c, Widget prev) {
        final ActItem itm = itemat(c);
        if (itm != null) {
            return itm.pagina.button().rendertt(true);
        } else {
            return super.tooltip(c, prev);
        }
    }

    @Override
    protected void drawitem(GOut g, ActItem item, int i) {
        g.image(item.icon, Coord.z);
        g.aimage(item.name.tex(), new Coord(itemh + 5, itemh / 2), 0, 0.5);
    }

    public class ActItem {
        public final MenuGrid.Pagina pagina;
        public final Text name;
        public final Tex icon;

        private ActItem(MenuGrid.Pagina pagina) {
            this.pagina = pagina;
            //Text.render(res.layer(Resource.action).name).tex();
            this.name = Text.render(this.pagina.act().name);
          //  this.name = font.render(this.pagina.act().name);
            this.icon = new TexI(PUtils.convolvedown(pagina.res.get().layer(Resource.imgc).img,
                    new Coord(itemh, itemh), CharWnd.iconfilter));
        }
    }
}