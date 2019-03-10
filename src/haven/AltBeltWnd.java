package haven;

public class AltBeltWnd extends Widget implements DTarget {
    public boolean dt = false;
    public final String origcap;
    private UI.Grab dm = null;
    private Coord doff;

    @RName("alt-wnd-belt")
    public static class $_ implements Factory {
        public Widget create(UI ui, Object[] args) {
            Coord sz = (Coord) args[0];
            String cap = (args.length > 1) ? (String) args[1] : null;
            return new AltBeltWnd(sz, cap);
        }
    }

    public AltBeltWnd(Coord sz, String cap) {
        origcap = cap;
	    resize(sz);
    }

    @Override
    public void resize(Coord sz) {
        this.sz = sz;
        for (Widget ch = child; ch != null; ch = ch.next)
            ch.presize();
    }

    @Override
    public void uimsg(String msg, Object... args) {
        if (msg == "pack") {
            pack();
        } else if (msg == "dt") {
            dt = (Integer) args[0] != 0;
        } else if (msg == "cap") {
        } else {
            super.uimsg(msg, args);
        }
    }

    @Override
    public boolean mousedown(Coord c, int button) {
        if (super.mousedown(c, button))
            return true;

        if (button == 1) {
            dm = ui.grabmouse(this);
            doff = c;
        }
        return true;
    }

    @Override
    public boolean mouseup(Coord c, int button) {
        if (dm != null) {
            dm.remove();
            dm = null;
            Utils.setprefc(origcap + "_c", this.c);
        } else {
            super.mouseup(c, button);
        }
        return (true);
    }

    @Override
    public void mousemove(Coord c) {
        if (dm != null) {
            this.c = this.c.add(c.add(doff.inv()));
        } else {
            super.mousemove(c);
        }
    }

    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        super.wdgmsg(sender, msg, args);
    }

    @Override
    public boolean type(char key, java.awt.event.KeyEvent ev) {
        return false;
    }

    @Override
    public boolean drop(Coord cc, Coord ul) {
        if (dt) {
            wdgmsg("drop", cc);
            return (true);
        }
        return (false);
    }

    @Override
    public boolean iteminteract(Coord cc, Coord ul) {
        return (false);
    }

    @Override
    public void destroy() {
        super.destroy();
        ui.beltWndId = -1;
    }
}
