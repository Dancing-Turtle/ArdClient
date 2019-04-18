/*
 *  This file is part of the Haven & Hearth game client.
 *  Copyright (C) 2009 Fredrik Tolf <fredrik@dolda2000.com>, and
 *                     Björn Johannessen <johannessen.bjorn@gmail.com>
 *
 *  Redistribution and/or modification of this file is subject to the
 *  terms of the GNU Lesser General Public License, version 3, as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  Other parts of this source tree adhere to other copying
 *  rights. Please see the file `COPYING' in the root directory of the
 *  source tree for details.
 *
 *  A copy the GNU Lesser General Public License is distributed along
 *  with the source tree of which this file is a part in the file
 *  `doc/LPGL-3'. If it is missing for any reason, please see the Free
 *  Software Foundation's website at <http://www.fsf.org/>, or write
 *  to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
 */

package haven;

import haven.purus.pbot.PBotUtils;
import haven.res.ui.tt.q.qbuff.QBuff;

import java.util.*;

public class Inventory extends Widget implements DTarget {
    public static final Tex invsq = Resource.loadtex("gfx/hud/invsq");
    public static final Coord sqsz = new Coord(33, 33);
    public boolean dropul = true;
    public Coord isz;
    public static final Comparator<WItem> ITEM_COMPARATOR_ASC = new Comparator<WItem>() {
	@Override
	public int compare(WItem o1, WItem o2) {
	    QualityList ql1 = o1.itemq.get();
	    double q1 = (ql1 != null && !ql1.isEmpty()) ? ql1.single(QualityList.SingleType.Average).value : 0;

	    QualityList ql2 = o2.itemq.get();
	    double q2 = (ql2 != null && !ql2.isEmpty()) ? ql2.single(QualityList.SingleType.Average).value : 0;

	    return Double.compare(q1, q2);
	}
    };
    public static final Comparator<WItem> ITEM_COMPARATOR_DESC = new Comparator<WItem>() {
	@Override
	public int compare(WItem o1, WItem o2) {
	    return ITEM_COMPARATOR_ASC.compare(o2, o1);
	}
    };

    public boolean locked = false;
    Map<GItem, WItem> wmap = new HashMap<GItem, WItem>();
    @RName("inv")
    public static class $_ implements Factory {
        public Widget create(UI ui, Object[] args) {
            return new Inventory((Coord) args[0]);
        }
    }

    public void draw(GOut g) {
        Coord c = new Coord();
        for (c.y = 0; c.y < isz.y; c.y++) {
            for (c.x = 0; c.x < isz.x; c.x++) {
                g.image(invsq, c.mul(sqsz));
            }
        }
        super.draw(g);
    }

    public Inventory(Coord sz) {
        super(invsq.sz().add(new Coord(-1, -1)).mul(sz).add(new Coord(1, 1)));
        isz = sz;
    }

    public boolean mousewheel(Coord c, int amount) {
        if (ui.modshift) {
            Inventory minv = getparent(GameUI.class).maininv;
            if (minv != this) {
                if (amount < 0)
                    wdgmsg("invxf", minv.wdgid(), 1);
                else if (amount > 0)
                    minv.wdgmsg("invxf", this.wdgid(), 1);
            }
        }
        return (true);
    }

    @Override
    public boolean mousedown(Coord c, int button) {
	return !locked && super.mousedown(c, button);
    }

    public void addchild(Widget child, Object... args) {
        add(child);
        Coord c = (Coord) args[0];
        if (child instanceof GItem) {
            GItem i = (GItem) child;
            wmap.put(i, add(new WItem(i), c.mul(sqsz).add(1, 1)));
        }
    }

    public void cdestroy(Widget w) {
        super.cdestroy(w);
        if (w instanceof GItem) {
            GItem i = (GItem) w;
            ui.destroy(wmap.remove(i));
        }
    }

    public boolean drop(Coord cc, Coord ul) {
        Coord dc = dropul ? ul.add(sqsz.div(2)).div(sqsz) : cc.div(sqsz);
        wdgmsg("drop", dc);
        return(true);
    }

    public boolean iteminteract(Coord cc, Coord ul) {
        return (false);
    }

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
        if(msg.equals("drop-identical")) {
            for (WItem item : getIdenticalItems((GItem) args[0]))
                item.item.wdgmsg("drop", Coord.z);
        } else if(msg.startsWith("transfer-identical")) {
            Window stockpile = gameui().getwnd("Stockpile");
            Window smelter = gameui().getwnd("Ore Smelter");
            Window kiln = gameui().getwnd("Kiln");
            if (stockpile == null || smelter != null || kiln != null) {
                List<WItem> items = getIdenticalItems((GItem) args[0]);
                Collections.sort(items, (a, b) -> {
                    QBuff aq = a.item.quality();
                    QBuff bq = b.item.quality();
                    if (aq == null || bq == null)
                        return 0;
                    else if (aq.q == bq.q)
                        return 0;
                    else if (aq.q > bq.q)
                        return msg.endsWith("asc") ? 1 : -1;
                    else
                        return msg.endsWith("asc") ? -1 : 1;
                });
                for (WItem item : items)
                    item.item.wdgmsg("transfer", Coord.z);
            } else {
                for (Widget w = stockpile.lchild; w != null; w = w.prev) {
                    if (w instanceof ISBox) {
                        ISBox isb = (ISBox) w;
                        int freespace = isb.getfreespace();
                        for (WItem item : getIdenticalItems((GItem) args[0])) {
                            if (freespace-- <= 0)
                                break;
                            item.item.wdgmsg("take", new Coord(item.sz.x / 2, item.sz.y / 2));
                            isb.drop(null, null);
                        }
                        break;
                    }
                }
            }
        } else {
            super.wdgmsg(sender, msg, args);
        }
    }

    public List<WItem> getIdenticalItems(GItem item) {
        List<WItem> items = new ArrayList<WItem>();
        GSprite sprite = item.spr();
        if (sprite != null) {
            String name = sprite.getname();
            String resname = item.resource().name;
            for (Widget wdg = child; wdg != null; wdg = wdg.next) {
                if (wdg instanceof WItem) {
                    sprite = ((WItem) wdg).item.spr();
                    if (sprite != null) {
                        Resource res = ((WItem) wdg).item.resource();
                        if (res != null && res.name.equals(resname) && (name == null || name.equals(sprite.getname())))
                            items.add((WItem) wdg);
                    }
                }
            }
        }
        return items;
    }

    /* Following getItem* methods do partial matching of the name *on purpose*.
       Because when localization is turned on, original English name will be in the brackets
       next to the translation
    */
    public List<WItem> getItemsPartial(String... names) {
        List<WItem> items = new ArrayList<WItem>();
        for (Widget wdg = child; wdg != null; wdg = wdg.next) {
            if (wdg instanceof WItem) {
                String wdgname = ((WItem)wdg).item.getname();
                for (String name : names) {
                    if (wdgname.contains(name)) {
                        items.add((WItem) wdg);
                        break;
                    }
                }
            }
        }
        return items;
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
    public WItem getItemPartialTrays(String name) {
        for (Widget wdg = child; wdg != null; wdg = wdg.next) {
            if (wdg instanceof WItem) {
                String wdgname = ((WItem)wdg).item.getname();
                if (wdgname.contains(name))
                    return (WItem) wdg;
            }
        }
        return null;
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

    public int getItemPartialCount(String name) {
        int count = 0;
        for (Widget wdg = child; wdg != null; wdg = wdg.next) {
            if (wdg instanceof WItem) {
                String wdgname = ((WItem)wdg).item.getname();
                if (wdgname.contains(name))
                    count++;
            }
        }
        return count;
    }

    public int getFreeSpace() {
        int feespace = isz.x * isz.y;
        for (Widget wdg = child; wdg != null; wdg = wdg.next) {
            if (wdg instanceof WItem)
                feespace -= (wdg.sz.x * wdg.sz.y) / (sqsz.x * sqsz.y);
        }
        return feespace;
    }

    // Null if no free slots found
    public Coord getFreeSlot() {
    	int[][] invTable = new int[isz.x][isz.y];
        for (Widget wdg = child; wdg != null; wdg = wdg.next) {
            if (wdg instanceof WItem) {
            	WItem item = (WItem) wdg;
            	for(int i=0; i<item.sz.div(sqsz).y; i++)
            		for(int j=0; j<item.sz.div(sqsz).x; j++)
            			invTable[item.c.div(sqsz).x+j][item.c.div(sqsz).y+i] = 1;
            }
        }
        for(int i=0; i<isz.y; i++) {
        	for(int j=0; j<isz.x; j++) {
        		if(invTable[j][i] == 0)
        			return new Coord(j, i);
        	}
        }
        return null;
    }

    public List<Coord> getFreeSlots() {
        List<Coord> cordlist = new ArrayList<>();
        int[][] invTable = new int[isz.x][isz.y];
        for (Widget wdg = child; wdg != null; wdg = wdg.next) {
            if (wdg instanceof WItem) {
                WItem item = (WItem) wdg;
                for(int i=0; i<item.sz.div(sqsz).y; i++)
                    for(int j=0; j<item.sz.div(sqsz).x; j++)
                        invTable[item.c.div(sqsz).x+j][item.c.div(sqsz).y+i] = 1;
            }
        }
        for(int i=0; i<isz.y; i++) {
            for(int j=0; j<isz.x; j++) {
                if(invTable[j][i] == 0)
                    cordlist.add(new Coord(j,i));
            }
        }
        return cordlist;
    }

    public boolean drink(int threshold) {
        IMeter.Meter stam = gameui().getmeter("stam", 0);
        if (stam == null || stam.a > threshold)
            return false;

        List<WItem> containers = getItemsPartial("Waterskin", "Waterflask", "Kuksa");
        for (WItem wi : containers) {
            ItemInfo.Contents cont = wi.item.getcontents();
            if (cont != null) {
                FlowerMenu.setNextSelection("Drink");
                ui.lcc = wi.rootpos();
                wi.item.wdgmsg("iact", wi.c, 0);
                return true;
            }
        }

        return false;
    }
    public static Coord invsz(Coord sz) { return invsq.sz().add(new Coord(-1, -1)).mul(sz).add(new Coord(1, 1)); }

    public static Coord sqroff(Coord c){ return c.div(invsq.sz()); }

    public static Coord sqoff(Coord c){ return c.mul(invsq.sz()); }
}
