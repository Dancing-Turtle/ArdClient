package haven;

import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

public class MenuSearch extends GameUI.Hidewnd {
    private TextEntry search;
    private ActionsList list;

    public MenuSearch() {
        super(new Coord(228, 280), "Search...");

        search = new TextEntry(210, "") {
            @Override
            public boolean type(char c, KeyEvent ev) {
                if (!parent.visible)
                    return false;
                if (c == '\n' && list.acts.size() > 0) {
                    list.change(list.acts.get(0));
                    return true;
                }

                boolean ret = buf.key(ev);
                list.changeFilter(text);
                list.sb.val = 0;
                return ret;
            }
        };
        add(search, new Coord(10, 5));

        list = new ActionsList(210, 10);
        add(list, new Coord(10, 35));
    }

    @Override
    public boolean show(boolean show) {
        if (show) {
            search.settext("");
            list.changeFilter("");
        }
        return super.show(show);
    }

    private static class ActionsList extends Listbox<Action> {
        private static final Coord nameoff = new Coord(34, 5);
        public List<Action> acts = new ArrayList<>(50);
        private boolean refresh = true;
        private int pagnum = 0;
        private final Comparator<Action> comp = (a, b) -> (a.sortkey.compareTo(b.sortkey));
        private String filter = "";

        public ActionsList(int w, int h) {
            super(w, h, 24);
        }

        public void changeFilter(String filter) {
            this.filter = filter.toLowerCase();
            refresh = true;
        }

        @Override
        public void tick(double dt) {
            GameUI gui = gameui();
            if (gui == null || gui.menu == null)
                return;

            Set<MenuGrid.Pagina> paginae = gui.menu.paginae;
            if (ui != null && (refresh || pagnum != paginae.size())) {
                refresh = false;
                pagnum = paginae.size();

                acts.clear();
                synchronized (paginae) {
                    for (MenuGrid.Pagina pag : paginae) {
                        try {
                            Resource res = pag.res.get();
                            if (!res.name.startsWith("paginae/bld") && !res.name.startsWith("paginae/craft"))
                                continue;

                            String name = res.layer(Resource.action).name.toLowerCase();
                            if (name.contains(filter))
                                acts.add(new Action(pag, name));
                        } catch (Loading l) {
                            refresh = true;
                        }
                    }
                }

                Collections.sort(acts, comp);
                if (acts.size() > 0)
                    change2(acts.get(0));
            }
        }

        @Override
        protected Action listitem(int idx) {
            return acts.get(idx);
        }

        @Override
        protected int listitems() {
            return acts.size();
        }

        @Override
        protected void drawbg(GOut g) {
            g.chcolor(0, 0, 0, 120);
            g.frect(Coord.z, sz);
            g.chcolor();
        }

        @Override
        protected void drawitem(GOut g, Action a, int idx) {
            try {
                if (a.img == null) {
                    Resource res = a.pagina.res.get();
                    a.img = new TexI(PUtils.convolvedown(res.layer(Resource.imgc).img, new Coord(itemh, itemh), CharWnd.iconfilter));
                    a.name = Text.render(res.layer(Resource.action).name).tex();
                }
                g.image(a.img, Coord.z);
                g.image(a.name, nameoff);
            } catch (Loading e) {
                g.image(WItem.missing.layer(Resource.imgc).tex(), Coord.z, new Coord(itemh, itemh));
            }
        }

        @Override
        public void change(Action a) {
            if (a != null) {
                super.change(a);
                GameUI gui = gameui();
                gui.histbelt.push(a.pagina);
                gui.act(a.pagina.act().ad);
            }
        }
    }

    private static class Action {
        private final MenuGrid.Pagina pagina;
        private Tex img;
        private Tex name;
        private String sortkey = "\uffff";

        private Action(MenuGrid.Pagina pagina, String sortkey) {
            this.pagina = pagina;
            this.sortkey = sortkey;
        }
    }
}
