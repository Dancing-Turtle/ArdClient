package haven;

import java.awt.event.KeyEvent;
import java.util.*;

public class MenuSearch extends Window implements ObservableListener<MenuGrid.Pagina>{
    private static final int WIDTH = 200;
    private final TextEntry entry;
    private final List<MenuGrid.Pagina> all = new ArrayList<>();
    private final ActList list;
    public boolean ignoreinit;
    public MenuSearch(String caption) {
        super(Coord.z, caption, caption);
        setcanfocus(true);
        setfocusctl(true);
        entry = add(new TextEntry(WIDTH, "") {
            public void activate(String text) {
                if(list.sel != null)
                    act(list.sel.pagina);
                MenuSearch.this.hide();
            }

            protected void changed() {
                super.changed();
                refilter();
            }

            public boolean type(char c, KeyEvent ev)
            {
                if(ignoreinit){
                    ignoreinit = false;
                    return false;
                }
                return super.type(c, ev);
            }

            public boolean keydown(KeyEvent e) {
                if(e.getKeyCode() == KeyEvent.VK_UP) {
                    final Optional<Integer> idx = list.selindex();
                    if(idx.isPresent()) {
                        list.change(Math.max(idx.get() - 1, 0));
                    } else {
                        list.change(0);
                    }
                    return true;
                } else if(e.getKeyCode() == KeyEvent.VK_DOWN) {
                    final Optional<Integer> idx = list.selindex();
                    if(idx.isPresent()) {
                        list.change(Math.min(idx.get() + 1, list.listitems() - 1));
                    } else {
                        list.change(0);
                    }
                    return true;
                } else {
                    return super.keydown(e) ;
                }
            }
        });
        setfocus(entry);
        list = add(new ActList(WIDTH, 10) {
            protected void itemclick(ActItem item, int button) {
               // if(sel == item) {
                    act(item.pagina);
                   // MenuSearch.this.hide();
              //  } else {
                    super.itemclick(item, button);
               // }
            }
        }, 0, entry.sz.y + 5);
        pack();
    }

    public void act(MenuGrid.Pagina act) {
        if(ui.gui != null) {
            ui.gui.menu.use(act.button(), false);
        }
    }

    public void show() {
        super.show();
        entry.settext("");
        list.change(0);
        parent.setfocus(this);
    }

    @Override
    protected void added() {
        super.added();
        ui.gui.menu.paginae.addListener(this);
    }

    public void close() { hide(); }

    @Override
    protected void removed() {
        ui.gui.menu.paginae.removeListener(this);
    }

    private void refilter() {
        list.clear();
        for (MenuGrid.Pagina p : all) {
            if (p.res.get().layer(Resource.action).name.toLowerCase().contains(entry.text.toLowerCase()))
                list.add(p);
        }
        list.sort(new ItemComparator());
        if (list.listitems() > 0) {
            final Optional<Integer> idx = list.selindex();
            if(idx.isPresent()) {
                list.change(Math.max(idx.get() - 1, 0));
            } else {
                list.change(0);
            }
        }
    }

    @Override
    public void init(Collection<MenuGrid.Pagina> base) {
        for(final MenuGrid.Pagina pag : base) {
            all.add(pag);
            if(isIncluded(pag)) {
                list.add(pag);
            }
        }
    }

    @Override
    public void added(MenuGrid.Pagina item) {
        all.add(item);
        if(isIncluded(item)) {
            list.add(item);
        }
    }

    @Override
    public void remove(MenuGrid.Pagina item) {
        all.remove(item);
        if(isIncluded(item)) {
            list.remove(item);
        }
    }

    private class ItemComparator implements Comparator<ActList.ActItem> {
        public int compare(ActList.ActItem a, ActList.ActItem b) {
            return a.name.text.compareTo(b.name.text);
        }
    }

    private boolean isIncluded(MenuGrid.Pagina pagina) {
        //ensure it's loaded
        try {
            pagina.res();
        } catch (Loading e) {
            try {
                e.waitfor();
            } catch (InterruptedException ex) {
                //Ignore
            }
        }
        return pagina.res.get().layer(Resource.action).name.toLowerCase().contains(entry.text.toLowerCase());
    }
}
