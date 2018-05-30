package haven;

import java.awt.Color;
import java.util.Arrays;


public class CharterList extends Listbox<String> {
    private String[] charters;
    private static final String prefname = "charters";
    private static final Coord txtoff = new Coord(3, 3);
    private Tex xicon = Text.render("\u2716", Color.RED, new Text.Foundry(Text.latin, 14)).tex();
    private static final int itemh = 20;

    public CharterList(int w, int h) {
        super(w, h, itemh);
        charters = Utils.getprefsa(prefname, new String[0]);
    }

    @Override
    protected String listitem(int idx) {
        return charters[idx];
    }

    @Override
    protected int listitems() {
        return charters.length;
    }

    @Override
    protected void drawbg(GOut g) {
        g.chcolor(0, 0, 0, 120);
        g.frect(Coord.z, sz);
        g.chcolor();
    }

    @Override
    protected void drawitem(GOut g, String chrt, int idx) {
        g.text(chrt, txtoff);
        g.image(xicon, new Coord(sz.x - xicon.sz().x - (sb.vis() ? 10 : 1), 3));
    }

    @Override
    public void change(String chrt) {
        if (chrt != null) {
            super.change(chrt);
            TextEntry te = parent.getchild(TextEntry.class);
            te.settext(chrt);
            te.activate(chrt);
        }
    }

    @Override
    public boolean mousedown(Coord c, int button) {
        if (button == 1) {
            int idx = (c.y / itemh);
            if (idx < charters.length && idx >= 0 &&
                    c.x > sz.x - 21 && c.x < sz.x - (sb.vis() ? 12 : 0)) {
                String[] charters = Utils.getprefsa(prefname, new String[0]);
                String[] cleaned = new String[charters.length - 1];
                System.arraycopy(charters, 0, cleaned, 0, idx);
                System.arraycopy(charters, idx + 1, cleaned, idx, charters.length - idx - 1);
                Utils.setprefsa(prefname, cleaned);
                this.charters = cleaned;
                return true;
            }
        }
        return super.mousedown(c, button);
    }

    public static void addCharter(String chrt) {
        String[] charters = Utils.getprefsa(prefname, new String[0]);
        charters = Arrays.copyOf(charters, charters.length + 1);
        charters[charters.length - 1] = chrt;
        Arrays.sort(charters);
        Utils.setprefsa(prefname, charters);
    }
}