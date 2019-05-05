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

package haven.resutil;

import haven.*;
import haven.res.ui.tt.q.qbuff.QBuff;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import static haven.CharWnd.Constipations.*;
import static haven.PUtils.*;
import static haven.QualityList.SingleType.*;

public class FoodInfo extends ItemInfo.Tip {
    public final double end, glut;
    public final Event[] evs;
    public final Effect[] efs;
    public final int[] types;
	public static boolean showbaseq;
    private final CharacterInfo.Constipation constipation;
	private final static DecimalFormat basefepfmt = new DecimalFormat("0.##");

    public FoodInfo(Owner owner, double end, double glut, Event[] evs, Effect[] efs, int[] types) {
	super(owner);
	this.end = end;
	this.glut = glut;
	this.evs = evs;
	this.efs = efs;
	this.types = types;

	CharacterInfo.Constipation constipation = null;
	try {
	    constipation = owner.context(Session.class).character.constipation;
	    if(!constipation.hasRenderer(FoodInfo.class)) {
		constipation.addRenderer(FoodInfo.class, FoodInfo::renderConstipation);
    }
	} catch (NullPointerException | OwnerContext.NoContext ignore) {}
	this.constipation = constipation;
    }


    public static class Event {
	public final CharWnd.FoodMeter.Event ev;
	public final BufferedImage img;
	public final double a;
	private final String res;

	public Event(Resource res, double a) {
	    this.ev = res.layer(CharWnd.FoodMeter.Event.class);
	    this.img = res.layer(Resource.imgc).img;
	    this.a = a;
	    this.res = res.name;
	}
    }

    public static class Effect {
	public final List<ItemInfo> info;
	public final double p;

	public Effect(List<ItemInfo> info, double p) {this.info = info; this.p = p;}
    }

    public BufferedImage tipimg() {
	BufferedImage base = RichText.render(String.format("Energy: $col[128,128,255]{%s%%}, Hunger: $col[255,192,128]{%s%%}", Utils.odformat2(end * 100, 2), Utils.odformat2(glut * 100, 2)), 0).img;
	Collection<BufferedImage> imgs = new LinkedList<BufferedImage>();
	imgs.add(base);
	for(int i = 0; i < evs.length; i++) {
	    Color col = Utils.blendcol(evs[i].ev.col, Color.WHITE, 0.5);
		imgs.add(catimgsh(5, evs[i].img, RichText.render(String.format("%s: $col[%d,%d,%d]{%s}", evs[i].ev.nm, col.getRed(), col.getGreen(), col.getBlue(), Utils.odformat2(evs[i].a, 2)), 0).img));
          /*  String str;
            if (showbaseq && owner instanceof GItem) {
                QBuff q = ((GItem) owner).quality();
                str = String.format("%s: $col[%d,%d,%d]{%s}  $col[%d,%d,%d]{(%s)}",
                        evs[i].ev.nm,
                        col.getRed(), col.getGreen(), col.getBlue(), Utils.odformat2(evs[i].a, 2),
                        col.getRed(), col.getGreen(), col.getBlue(), q != null ? basefepfmt.format(evs[i].a / Math.sqrt(q.q / 10)) : "???");
            } else {
                str = String.format("%s: $col[%d,%d,%d]{%s}", evs[i].ev.nm, col.getRed(), col.getGreen(), col.getBlue(), Utils.odformat2(evs[i].a, 2));
            }
            imgs.add(catimgsh(5, evs[i].img, RichText.render(str, 0).img));*/
	}
	for(int i = 0; i < efs.length; i++) {
	    BufferedImage efi = ItemInfo.longtip(efs[i].info);
	    if(efs[i].p != 1)
		efi = catimgsh(5, efi, RichText.render(String.format("$i{($col[192,192,255]{%d%%} chance)}", (int)Math.round(efs[i].p * 100)), 0).img);
	    imgs.add(efi);
	}
	if(types.length > 0 && constipation != null) {
	    imgs.add(Text.render("Categories:").img);
	    double total = 1;
	    for (int type : types) {
		CharacterInfo.Constipation.Data c = constipation.get(type);
		if(c!=null) {
		    imgs.add(constipation.render(FoodInfo.class, c));
		    total *= c.value;
		}
	    }
	    Color col = color(total);
	    imgs.add(RichText.render(String.format("Total: $col[%d,%d,%d]{%s%%}", col.getRed(), col.getGreen(), col.getBlue(), Utils.odformat2(100 * total, 2)), 0).img);
	}
	return(catimgs(0, imgs.toArray(new BufferedImage[0])));
    }

    private static BufferedImage renderConstipation(CharacterInfo.Constipation.Data data) {
	int h = 14;
	BufferedImage img = data.res.get().layer(Resource.imgc).img;
	String nm = data.res.get().layer(Resource.tooltip).t;
	Color col = color(data.value);
	Text rnm = RichText.render(String.format("%s: $col[%d,%d,%d]{%d%%}", nm, col.getRed(), col.getGreen(), col.getBlue(), (int) (100 * data.value)), 0);
	BufferedImage tip = TexI.mkbuf(new Coord(h + 5 + rnm.sz().x, h));
	Graphics g = tip.getGraphics();
	g.drawImage(convolvedown(img, new Coord(h, h), tflt), 0, 0, null);
	g.drawImage(rnm.img, h + 5, ((h - rnm.sz().y) / 2) + 1, null);
	g.dispose();

	return tip;
    }

    public static class Data implements ItemData.ITipData {
	private final double end;
	private final double glut;
	private final List<Pair<String, Double>> fep;
	private final int[] types;

	public Data(FoodInfo info, QualityList q) {
	    end = info.end;
	    glut = info.glut;
	    QualityList.Quality single = q.single(Quality);
	    if(single == null) {
		single = QualityList.DEFAULT;
	    }
	    double multiplier = single.multiplier;
	    fep = new ArrayList<>(info.evs.length);
	    for (int i = 0; i < info.evs.length; i++) {
		fep.add(new Pair<>(info.evs[i].res, Utils.round(info.evs[i].a / multiplier, 2)));
	    }
	    types  = info.types;
	}

	@Override
	public ItemInfo create(Session sess) {
	    Event[] evs;
	    if (fep == null) {
		evs = new Event[0];
	    } else {
		evs = new Event[fep.size()];
		for (int i = 0; i < fep.size(); i++) {
		    Pair<String, Double> tmp = fep.get(i);
		    evs[i] = new Event(Resource.remote().loadwait(tmp.a), tmp.b);
		}
	    }
	    int[] t;
	    if (types == null) {
		t = new int[0];
	    } else {
		t = types;
	    }

	    return new FoodInfo(null, end, glut, evs, new Effect[0], t);
	}
    }
}
