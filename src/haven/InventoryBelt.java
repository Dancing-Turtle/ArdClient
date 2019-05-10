package haven;



import haven.purus.pbot.PBotUtils;

import java.awt.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class InventoryBelt extends Widget implements DTarget {
    private static final Tex invsq = Resource.loadtex("gfx/hud/invsq-opaque");
    private static final Coord sqsz = new Coord(36, 33);
    private static final Coord sqsz2 = new Coord(33, 33);
    public boolean dropul = true;
    public Coord isz;
    Map<GItem, WItem> wmap = new HashMap<GItem, WItem>();

    @RName("inv-belt")
    public static class $_ implements Factory {
        public Widget create(UI ui, Object[] args) {
            return new InventoryBelt((Coord) args[0]);
        }
    }

    public void draw(GOut g) {
        Coord c = new Coord();
        for (; c.x < isz.x * isz.y * sqsz.x; c.x += sqsz.x)
            g.image(invsq, c);
        super.draw(g);
    }

    public InventoryBelt(Coord sz) {
        super(invsq.sz().add(new Coord(-1 + 3, -1)).mul(sz.x * sz.y, 1).add(new Coord(1, 1)));
        isz = sz;
    }

    @Override
    public boolean mousewheel(Coord c, int amount) {
        return false;
    }

    @Override
    public void addchild(Widget child, Object... args) {
        add(child);
        Coord c = (Coord) args[0];
        if (child instanceof GItem) {
            // convert multi-row coordinate into single row
            c.x = isz.x * c.y + c.x;
            c.y = 0;
            GItem i = (GItem) child;
            wmap.put(i, add(new WItem(i), c.mul(sqsz).add(1, 1)));
        }
    }

    @Override
    public void cdestroy(Widget w) {
        super.cdestroy(w);
        if (w instanceof GItem) {
            GItem i = (GItem) w;
            ui.destroy(wmap.remove(i));
        }
    }

    @Override
    public boolean drop(Coord cc, Coord ul) {
        Coord dc = dropul ? ul.add(sqsz.div(2)).div(sqsz) : cc.div(sqsz);
        // convert single row coordinate into multi-row
        if (dc.x >= isz.x) {
            dc.y = dc.x / isz.x;
            dc.x = dc.x % isz.x;
        }
        wdgmsg("drop", dc);
        return (true);
    }

    @Override
    public boolean iteminteract(Coord cc, Coord ul) {
        return (false);
    }

    @Override
    public void uimsg(String msg, Object... args) {
        if (msg == "sz") {
            isz = (Coord) args[0];
            resize(invsq.sz().add(new Coord(-1, -1)).mul(isz).add(new Coord(1, 1)));
        } else if(msg == "mode") {
            dropul = (((Integer)args[0]) == 0);
        } else {
            super.uimsg(msg, args);
        }
    }

    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        if(!msg.endsWith("-identical"))
            super.wdgmsg(sender, msg, args);
    }

    public WItem getItemPartial(String name) {
        for (Widget wdg = child; wdg != null; wdg = wdg.next) {
            if (wdg instanceof WItem) {
                String wdgname = ((WItem)wdg).item.getname();
                if (wdgname.contains(name))
                    return (WItem) wdg;
            }
        }
        return null;
    }

    // Null if no free slots found
    public Coord getFreeSlot() {
        int[][] invTable = new int[isz.x * isz.y][1];
        for (Widget wdg = child; wdg != null; wdg = wdg.next) {
            if (wdg instanceof WItem) {
                WItem item = (WItem) wdg;
                for(int i=0; i<item.sz.div(sqsz2).y; i++)
                    for(int j=0; j<item.sz.div(sqsz2).x; j++)
                        invTable[item.c.div(sqsz).x+j][i] = 1;
            }
        }
        for(int i=0; i<(isz.y * isz.x); i++) {
                if(invTable[i][0] == 0)
                    return new Coord(i * 36, 0);
        }
        return null;
    }

    public List<Coord> getFreeSlots() {
        List<Coord> cordlist = new ArrayList<>();
        int[][] invTable = new int[isz.x * isz.y][1];
        for (Widget wdg = child; wdg != null; wdg = wdg.next) {
            if (wdg instanceof WItem) {
                WItem item = (WItem) wdg;
               for(int i=0; i<item.sz.div(sqsz2).y; i++)
                    for(int j=0; j<item.sz.div(sqsz2).x; j++) {
                        invTable[item.c.div(sqsz).x + j][i] = 1;
                    }
            }
        }

        for(int i=0; i<(isz.y * isz.x); i++) {
                if(invTable[i][0] == 0)
                    cordlist.add(new Coord(i * 36,0));
        }
        return cordlist;
    }

    public WItem getItemPartialDrink(String name) {
        for (Widget wdg = child; wdg != null; wdg = wdg.next) {
            if (wdg instanceof WItem) {
                String wdgname = ((WItem) wdg).item.getname();
                if (wdgname.contains(name))
                    if (!PBotUtils.canDrinkFrom((WItem) wdg))
                         return null;
                        if (PBotUtils.canDrinkFrom((WItem) wdg)) {
                            return (WItem) wdg;
                        }
            }
        }
          return null;
    }


}
