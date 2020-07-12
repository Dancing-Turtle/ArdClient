package haven.sloth.gui;

import haven.*;
import haven.sloth.util.ObservableListener;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class SessionDisplay extends MovableWidget implements ObservableListener<UI> {
    private static class UIDisplay extends Widget {
        private final UI ui;
        private final Button btn;
        private String nm;

        public UIDisplay(final UI ui) {
            super(Coord.z);
            this.ui = ui;
            this.nm = "Login";
            add(this.btn = new Button(100, nm, this::click));
            pack();
        }

        @Override
        public void tick(double dt) {
            if (nm.equals("Login") && ui.sess != null && ui.sess.username != null) {
                nm = ui.sess.username;
                btn.change(nm);
            }
        }

        private void click() {
            MainFrame.instance.p.setActiveUI(ui);
        }
    }

    private final Map<UI, UIDisplay> uimap = new HashMap<>();
    private final Grouping grp = new GridGrouping("Sessions", 5, 100);
    private final Button add = new Button(100, "New Session", this::newSession);

    public SessionDisplay() {
        super(Coord.z, "Session Display");
        add(grp);
        grp.add(add);
        MainFrame.instance.p.sessions.addListener(this);
    }

    private void newSession() {
        MainFrame.instance.makeNewSession();
    }

    @Override
    protected boolean moveHit(Coord c, int btn) {
        return btn == 3;
    }

    @Override
    protected void removed() {
        MainFrame.instance.p.sessions.removeListener(this);
    }

    public void init(Collection<UI> base) {
        for (final UI lui : base) {
            final UIDisplay display = new UIDisplay(lui);
            uimap.put(lui, display);
            grp.add(display);
        }
        add.destroy();
        grp.add(add);
        grp.pack();
        pack();
    }

    public void added(UI item) {
        final UIDisplay display = new UIDisplay(item);
        uimap.put(item, display);
        grp.add(display);
        add.destroy();
        grp.add(add);
        grp.pack();
        pack();
    }

    public void remove(UI item) {
        final UIDisplay display = uimap.remove(item);
        display.destroy();
        grp.pack();
        pack();
    }
}