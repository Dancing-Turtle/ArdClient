package haven;

import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;

import static haven.Inventory.invsq;

public class CraftHistoryBelt extends Widget {
    private static final int SIZE = 8;
    private MenuGrid.Pagina[] belt = new MenuGrid.Pagina[SIZE];
    private UI.Grab dragging;
    private Coord dc;
    private static final Coord vsz = new Coord(34, 450);
    private static final Coord hsz = new Coord(450, 34);
    private boolean vertical;

    public CraftHistoryBelt(boolean vertical) {
        super(vertical ? vsz : hsz);
        this.vertical = vertical;
    }

    private Coord beltc(int i) {
        if (vertical)
            return new Coord(0, (invsq.sz().x + 2) * i);
        return new Coord((invsq.sz().x + 2) * i, 0);
    }

    private int beltslot(Coord c) {
        for (int i = 0; i < SIZE; i++) {
            if (c.isect(beltc(i), invsq.sz()))
                return i;
        }
        return -1;
    }

    @Override
    public void draw(GOut g) {
        for (int i = 0; i < SIZE; i++) {
            int slot = i;
            Coord c = beltc(i);
            g.image(invsq, c);
            if (belt[slot] != null)
                g.image(belt[slot].img, c.add(1, 1));
        }
    }

    @Override
    public boolean mousedown(Coord c, int button) {
        int slot = beltslot(c);
        if (slot != -1) {
            if (button == 1 && belt[slot] != null) {
                String[] ad = belt[slot].act().ad;
                if (ad.length > 0 && (ad[0].equals("craft") || ad[0].equals("bp"))) {
                    MenuGrid g = PBotAPI.gui.menu;
                    g.lastCraft = g.getPagina(ad[1]);
                }
                gameui().act(ad);
            } else if (button == 1 && belt[slot] == null) {
                if (ui.modshift) {
                    if (vertical) {
                        sz = hsz;
                        vertical = false;
                    } else {
                        sz = vsz;
                        vertical = true;
                    }
                    Utils.setprefb("histbelt_vertical", vertical);
                } else {
                    dragging = ui.grabmouse(this);
                    dc = c;
                }
                return true;
            }
            return true;
        }
        return false;
    }

    @Override
    public boolean mouseup(Coord c, int button) {
        if (dragging != null) {
            dragging.remove();
            dragging = null;
            Utils.setprefc("histbelt_c", this.c);
            return true;
        }
        return super.mouseup(c, button);
    }

    @Override
    public void mousemove(Coord c) {
        if (dragging != null) {
            this.c = this.c.add(c.x, c.y).sub(dc);
            return;
        }
        super.mousemove(c);
    }

    public void push(MenuGrid.Pagina pagina) {
        for (MenuGrid.Pagina p : belt) {
            if (p == pagina)
                return;
        }
        for (int i = SIZE - 2; i >= 0; i--)
            belt[i + 1] = belt[i];
        belt[0] = pagina;
    }
}