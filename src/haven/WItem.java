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

import static haven.Inventory.sqsz;
import static haven.Text.num10Fnd;
import haven.automation.WItemDestroyCallback;
import haven.ItemInfo.AttrCache;
import haven.res.ui.tt.Wear;
import haven.res.ui.tt.q.qbuff.QBuff;
import static haven.Inventory.sqsz;
import static haven.Text.num10Fnd;


import haven.automation.WItemDestroyCallback;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.*;
import java.util.ArrayList;
import java.util.List;
import java.util.function.*;
import com.sun.java.accessibility.util.GUIInitializedListener;

import com.sun.java.accessibility.util.GUIInitializedListener;

import haven.ItemInfo.AttrCache;
import haven.automation.WItemDestroyCallback;
import haven.res.ui.tt.Wear;
import haven.res.ui.tt.q.qbuff.QBuff;

public class WItem extends Widget implements DTarget {
	public static final Resource missing = Resource.local().loadwait("gfx/invobjs/missing");
	public static final Tex lockt = Resource.loadtex("custom/inv/locked");
	public GItem item;
	public static final Color famountclr = new Color(24, 116, 205);
	private static final Color qualitybg = new Color(20, 20, 20, 255 - Config.qualitybgtransparency);
	public static final Color DURABILITY_COLOR = new Color(214, 253, 255);
	public static final Color ARMOR_COLOR = new Color(255, 227, 191);
	//public static final Color MATCH_COLOR = new Color(255, 32, 255, 255);
	public static final Color MATCH_COLOR = new Color(0, 255, 0, 255);
	public static final Color[] wearclr = new Color[]{
			new Color(233, 0, 14), new Color(218, 128, 87), new Color(246, 233, 87), new Color(145, 225, 60)
	};
	private Resource cspr = null;
	private Message csdt = Message.nil;
	private boolean locked = false;
	private WItemDestroyCallback destroycb;

	public WItem(GItem item) {
		super(sqsz);
		this.item = item;
	}

    public boolean locked() { return locked; }
    public void setLock(final boolean val) { locked = val; }
	public void drawmain(GOut g, GSprite spr) {
		spr.draw(g);
	}

	public static BufferedImage shorttip(List<ItemInfo> info) {
		return (ItemInfo.shorttip(info));
	}

	public static BufferedImage longtip(GItem item, List<ItemInfo> info) {
		BufferedImage img = ItemInfo.longtip(info);
		Resource.Pagina pg = item.res.get().layer(Resource.pagina);
		if (pg != null)
			img = ItemInfo.catimgs(0, img, RichText.render("\n" + pg.text, 200).img);
		return (img);
	}

	public BufferedImage longtip(List<ItemInfo> info) {
		return (longtip(item, info));
	}

	public class ItemTip implements Indir<Tex> {
		private final TexI tex;

		public ItemTip(BufferedImage img) {
			if (img == null)
				throw (new Loading());
			tex = new TexI(img);
		}

		public GItem item() {
			return (item);
		}

		public Tex get() {
			return (tex);
		}
	}

	public class ShortTip extends ItemTip {
		public ShortTip(List<ItemInfo> info) {
			super(shorttip(info));
		}
	}

	public class LongTip extends ItemTip {
		public LongTip(List<ItemInfo> info) {
			super(longtip(info));
		}
	}

	private double hoverstart;
	private ItemTip shorttip = null, longtip = null;
	private List<ItemInfo> ttinfo = null;

	public Object tooltip(Coord c, Widget prev) {
		double now = Utils.rtime();
		if (prev == this) {
		} else if (prev instanceof WItem) {
			double ps = ((WItem)prev).hoverstart;
			if (now - ps < 1.0)
				hoverstart = now;
			else
				hoverstart = ps;
		} else {
			hoverstart = now;
		}
		try {
			List<ItemInfo> info = item.info();
			if (info.size() < 1)
				return (null);
			if (info != ttinfo) {
				shorttip = longtip = null;
				ttinfo = info;
			}
			if (now - hoverstart < 1.0 && !Config.longtooltips) {
				if (shorttip == null)
					shorttip = new ShortTip(info);
				return (shorttip);
			} else {
				if (longtip == null)
					longtip = new LongTip(info);
				return (longtip);
			}
		} catch (Loading e) {
			return ("...");
		}
	}

	public final AttrCache<List<ItemInfo>> gilding = new AttrCache<List<ItemInfo>>(this::info, AttrCache.cache(info -> ItemInfo.findall("Slotted", info)));

	public final AttrCache<List<ItemInfo>> slots = new AttrCache<List<ItemInfo>>(this::info, AttrCache.cache(info -> ItemInfo.findall("ISlots", info)));

	public final AttrCache<Boolean> gildable = new AttrCache<Boolean>(this::info, AttrCache.cache(info -> {
		List<ItemInfo> slots = ItemInfo.findall("ISlots", info);
		for(ItemInfo slot : slots) {
			if(Reflect.getFieldValueInt(slot, "left") > 0) {
				return true;
			}
		}
		return false;
	}));

	public final AttrCache<String> name = new AttrCache<>(this::info, AttrCache.cache(info -> {
		ItemInfo.Name name = ItemInfo.find(ItemInfo.Name.class, info);
		return (name != null && name.str != null && name.str.text != null) ? name.str.text : "";
	}));

	private List<ItemInfo> info() {return(item.info());}
	public final AttrCache<Color> olcol = new AttrCache<>(this::info, info -> {
		Color ret = null;
		for(ItemInfo inf : info) {
			if(inf instanceof GItem.ColorInfo) {
				Color c = ((GItem.ColorInfo)inf).olcol();
				if(c != null)
					ret = (ret == null)?c:Utils.preblend(ret, c);
			}
		}
		Color fret = ret;
		return(() -> fret);
	});

	public final AttrCache<GItem.InfoOverlay<?>[]> itemols = new AttrCache<>(this::info, info -> {
		ArrayList<GItem.InfoOverlay<?>> buf = new ArrayList<>();
		for(ItemInfo inf : info) {
			if(inf instanceof GItem.OverlayInfo)
				buf.add(GItem.InfoOverlay.create((GItem.OverlayInfo<?>)inf));
		}
		GItem.InfoOverlay<?>[] ret = buf.toArray(new GItem.InfoOverlay<?>[0]);
		return(() -> ret);
	});

    public final AttrCache<Double> itemmeter = new AttrCache<>(this::info, AttrCache.map1(GItem.MeterInfo.class, minf -> {
        GItem itm = WItem.this.item;
        if (minf != null) {
            double meter = minf.meter();
            if (itm.studytime > 0 && parent instanceof InventoryStudy) {
                int timeleft = (int) (itm.studytime * (1.0 - meter));
                int hoursleft = timeleft / 60;
                int minutesleft = timeleft - hoursleft * 60;
				if(hoursleft < 1){
					itm.metertex = Text.renderstroked(String.format("%d:%02d", hoursleft, minutesleft), Color.YELLOW, Color.BLACK, num10Fnd).tex();
				}
				else
                itm.metertex = Text.renderstroked(String.format("%d:%02d", hoursleft, minutesleft), Color.WHITE, Color.BLACK, num10Fnd).tex();
            } else {
                itm.metertex = Text.renderstroked(String.format("%d%%", (int) (meter * 100)), Color.WHITE, Color.BLACK, num10Fnd).tex();
	}
            return minf::meter;
	}
        itm.metertex = null;
        return minf::meter;
    }));
	public final AttrCache<QualityList> itemq = new AttrCache<QualityList>(this::info, AttrCache.cache(info -> {
		List<ItemInfo.Contents> contents = ItemInfo.findall(ItemInfo.Contents.class, info);
		List<ItemInfo> qualities = null;
		if(!contents.isEmpty()) {
			for(ItemInfo.Contents content : contents) {
				List<ItemInfo> tmp = ItemInfo.findall(QualityList.classname, content.sub);
				if(!tmp.isEmpty()) {
					qualities = tmp;
				}
			}
		}
		if(qualities == null || qualities.isEmpty()) {
			qualities = ItemInfo.findall(QualityList.classname, info);
		}

		QualityList qualityList = new QualityList(qualities);
		return !qualityList.isEmpty() ? qualityList : null;
	}));

    public final AttrCache<Tex> heurnum = new AttrCache<Tex>(this::info, AttrCache.cache(info -> {
	String num = ItemInfo.getCount(info);
	if(num == null) return null;
	return Text.renderstroked(num, Color.WHITE, Color.BLACK).tex();
    }));

    public final AttrCache<Tex> durability = new AttrCache<Tex>(this::info, AttrCache.cache(info -> {
	Pair<Integer, Integer> wear = ItemInfo.getWear(info);
	if(wear == null) return (null);
	return Text.renderstroked(String.valueOf(wear.b - wear.a), DURABILITY_COLOR, Color.BLACK).tex();
    })) {
	@Override
	public Tex get() {
	    return Config.showwearbars ? super.get() : null;
	}
    };

    public final AttrCache<Pair<Integer, Integer>> wear = new AttrCache<Pair<Integer, Integer>>(this::info, AttrCache.cache(ItemInfo::getWear));

    public final AttrCache<Tex> armor = new AttrCache<Tex>(this::info, AttrCache.cache(info -> {
	Pair<Integer, Integer> armor = ItemInfo.getArmor(info);
	if(armor == null) return (null);
	return Text.renderstroked(String.format("%d/%d", armor.a, armor.b), ARMOR_COLOR, Color.BLACK).tex();
    })) {
	@Override
	public Tex get() {
	    return Config.showwearbars ? super.get() : null;
	}
    };


	private GSprite lspr = null;

	public void tick(double dt) {
		/* XXX: This is ugly and there should be a better way to
		 * ensure the resizing happens as it should, but I can't think
		 * of one yet. */
		GSprite spr = item.spr();
		if ((spr != null) && (spr != lspr)) {
			Coord sz = new Coord(spr.sz());
			if ((sz.x % sqsz.x) != 0)
				sz.x = sqsz.x * ((sz.x / sqsz.x) + 1);
			if ((sz.y % sqsz.y) != 0)
				sz.y = sqsz.y * ((sz.y / sqsz.y) + 1);
			resize(sz);
			lspr = spr;
		}
	}

	public void draw(GOut g) {
		GSprite spr = item.spr();
		if (spr != null) {
			Coord sz = spr.sz();
			g.defstate();
			if (olcol.get() != null)
				g.usestate(new ColorMask(olcol.get()));
			if (item.matches) {
				g.chcolor(MATCH_COLOR);
				g.rect(Coord.z, sz);
				g.chcolor();
			}
			drawmain(g, spr);
			g.defstate();
			GItem.InfoOverlay<?>[] ols = itemols.get();
			if (ols != null) {
				for (GItem.InfoOverlay<?> ol : ols)
					ol.draw(g);
			}
			Double meter = item.meter > 0 ? Double.valueOf(item.meter / 100.0) : itemmeter.get();
			if (Config.itemmeterbar && meter != null && meter > 0) {
				g.chcolor(220, 60, 60, 255);
				g.frect(Coord.z, new Coord((int) (sz.x / (100 / (meter * 100))), 4));
				g.chcolor();
			}

			QBuff quality = item.quality();
			if (Config.showquality) {
				if (quality != null && quality.qtex != null) {
					Coord btm = new Coord(0, sz.y - 12);
					Tex t = Config.qualitywhole ? quality.qwtex : quality.qtex;
					if (Config.qualitybg) {
						g.chcolor(qualitybg);
						g.frect(btm, t.sz().add(1, -1));
						g.chcolor();
					}
					g.image(t, btm);
				}
			}

			if (item.metertex != null)
				g.image(item.metertex, Coord.z);

			ItemInfo.Contents cnt = item.getcontents();
			if (cnt != null && cnt.content > 0)
				drawamountbar(g, cnt.content, cnt.isseeds);

			if (Config.showwearbars) {
				try {
					for (ItemInfo info : item.info()) {
						if (info instanceof Wear) {
							double d = ((Wear) info).d;
							double m = ((Wear) info).m;
							double p = (m - d) / m;
							int h = (int) (p * (double) sz.y);
							g.chcolor(wearclr[p == 1.0 ? 3 : (int) (p / 0.25)]);
							g.frect(new Coord(sz.x - 3, sz.y - h), new Coord(3, h));
							g.chcolor();
							break;
						}
					}
				} catch (Exception e) {
				}
			}
			if (locked) {
				g.image(lockt, Coord.z);
			}
		}
			else
				g.image(missing.layer(Resource.imgc).tex(), Coord.z, sz);

		}



	private void drawamountbar(GOut g, double content, boolean isseeds) {
		double capacity;
		String name = item.getname();
		if (name.contains("Waterskin"))
			capacity = 3.0D;
		else if (name.contains("Bucket"))
			capacity = isseeds ? 1000D : 10.0D;
		else if (name.contains("Waterflask"))
			capacity = 2.0D;
		else
			return;

		int h = (int) (content / capacity * sz.y) - 1;
		if (h < 0)
			return;

		g.chcolor(famountclr);
		g.frect(new Coord(sz.x - 4, sz.y - h - 1), new Coord(3, h));
		g.chcolor();
	}

	public boolean mousedown(Coord c, int btn) {
		if (btn == 1) {
			if(!locked) {
				if (ui.gui.itemClickCallback != null) {
					ui.gui.itemClickCallback.itemClick(this);
					return true;
				}
				if (ui.modctrl && ui.modmeta)
					wdgmsg("drop-identical", this.item);
				else if (ui.modctrl && ui.modshift) {
					String name = ItemInfo.find(ItemInfo.Name.class, item.info()).str.text;
					name = name.replace(' ', '_');
					if (!Resource.language.equals("en")) {
						int i = name.indexOf('(');
						if (i > 0)
							name = name.substring(i + 1, name.length() - 1);
					}
					try {
						WebBrowser.self.show(new URL(String.format("http://ringofbrodgar.com/wiki/%s", name)));
					} catch (MalformedURLException e) {
					} catch (Exception e) {
						getparent(GameUI.class).error("Could not launch web browser.");
					}
				} else if (ui.modshift && !ui.modmeta) {
					// server side transfer all identical: pass third argument -1 (or 1 for single item)
					item.wdgmsg("transfer", c);
				} else if (ui.modctrl)
					item.wdgmsg("drop", c);
				else if (ui.modmeta)
					wdgmsg("transfer-identical", this.item);
				else
					item.wdgmsg("take", c);
				return (true);
			}
		} else if (btn == 3) {
			if(ui.modctrl && ui.modshift) {
				locked = !locked;
				return true;
			}else if(ui.modctrl){
				item.wdgmsg("iact", c, ui.modflags());
			} else if (ui.modmeta && !(parent instanceof Equipory)) {
				wdgmsg("transfer-identical-asc", this.item);
			}
			else
				item.wdgmsg("iact", c, ui.modflags());
			return (true);
		}
		return (false);
	}

	public boolean drop(Coord cc, Coord ul) {
		return (false);
	}

	public boolean iteminteract(Coord cc, Coord ul) {
		item.wdgmsg("itemact", ui.modflags());
		return (true);
	}
	public boolean drop(WItem target, Coord cc, Coord ul) {
		return(false);
	}

	@Override
	public void reqdestroy() {
		super.reqdestroy();
		if (destroycb != null)
			destroycb.notifyDestroy();
	}

	public void registerDestroyCallback(WItemDestroyCallback cb) {
		this.destroycb = cb;
	}
	public boolean iteminteract(WItem target, Coord cc, Coord ul) {
		if(!GildingWnd.processGilding(ui,this, target)) {
			item.wdgmsg("itemact", ui.modflags());
		}
		return(true);
	}

	public Coord size() {
		Indir<Resource> res = item.getres().indir();
		if (res.get() != null) {
			Tex tex = res.get().layer(Resource.imgc).tex();
			if(tex == null)
				return new Coord(1, 1);
			else
				return tex.sz().div(30);
		} else {
			return new Coord(1, 1);
		}
	}
}