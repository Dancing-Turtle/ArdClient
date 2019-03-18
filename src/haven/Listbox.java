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

import java.awt.Color;
import java.util.Optional;

public abstract class Listbox<T> extends ListWidget<T> {
    public static final Color selc = new Color(114, 179, 82, 128);
    public static final Color overc = new Color(189, 239, 137, 53);
    public Color bgcolor = Color.BLACK;
    public int h;
    public final Scrollbar sb;
    private T over;
    public int selindex;

    public Listbox(int w, int h, int itemh) {
        super(new Coord(w, h * itemh), itemh);
        this.h = h;
        this.sb = adda(new Scrollbar(sz.y, 0, 0), sz.x, 0, 1, 0);
	selindex = -1;
    }

    protected void drawsel(GOut g) {
	drawsel(g, selc);
    }

    protected void drawsel(GOut g, Color color) {
	g.chcolor(color);
        g.frect(Coord.z, g.sz);
        g.chcolor();
    }

    protected void drawbg(GOut g) {
	if(bgcolor != null) {
	    g.chcolor(bgcolor);
        g.frect(Coord.z, sz);
        g.chcolor();
    }
    }

    public void draw(GOut g) {
        sb.max = listitems() - h;
        drawbg(g);
        int n = listitems();
        for(int i = 0; (i * itemh) < sz.y; i++) {
            int idx = i + sb.val;
            if(idx >= n)
                break;
            T item = listitem(idx);
            int w = sz.x - (sb.vis()?sb.sz.x:0);
            GOut ig = g.reclip(new Coord(0, i * itemh), new Coord(w, itemh));
            if(item == sel)
                drawsel(ig);
	    else if(item == over){
		drawsel(ig, overc);
	    }
            drawitem(ig, item, idx);
        }
        super.draw(g);
    }

    public boolean mousewheel(Coord c, int amount) {
        sb.ch(amount);
        return(true);
    }

    protected void itemclick(T item, int button) {
        if(button == 1)
            change(item);
    }

    public void change(final int idx) {
        if(idx >= 0 && idx < listitems()) {
            sel = listitem(idx);
            selindex = idx;
            showsel();
        }
    }
    public Optional<Integer> selindex() {
        return selindex >= 0 ? Optional.of(selindex) : Optional.empty();
    }
    private Optional<Integer> itemato(Coord c) {
        int idx = (c.y / itemh) + sb.val;
        if(idx >= listitems())
            return Optional.empty();
        return Optional.of(idx);
    }


    protected void itemactivate(T item) {}

    public T itemat(Coord c) {
        int idx = (c.y / itemh) + sb.val;
	if(idx >= listitems() || idx < 0)
	    return(null);
	return(listitem(idx));
    }

    public boolean mousedown(Coord c, int button) {
        if(super.mousedown(c, button))
            return(true);
        final Optional<Integer> idx = itemato(c);
        if(idx.isPresent()) {
            selindex = idx.get();
            T item = listitem(selindex);
            itemclick(item, button);
        } else if(button == 1) {
            change(null);
            selindex = -1;
        }
        return(true);
    }

    @Override
    public void mousemove(Coord c) {
	super.mousemove(c);
	if(c.isect(Coord.z, sz)){
	    over = itemat(c);
	} else{
	    over = null;
        }
    }

    public boolean mouseclick(Coord c, int button, int count) {
        if(super.mouseclick(c, button, count))
            return(true);
        T item = itemat(c);
        if(item != null && button == 1 && count >= 2)
            itemactivate(item);
        return(true);
    }

    // ensures that selected element is visible
    public void showsel() {
	if (sb.val + h - 1 < selindex)
	    sb.val = Math.max(0, selindex - h + 1);
	if (sb.val > selindex)
	    sb.val = Math.max(0, selindex);
    }

    public void display(int idx) {
        if(idx < sb.val) {
            sb.val = idx;
        } else if(idx >= sb.val + h) {
            sb.val = Math.max(idx - (h - 1), 0);
        }
    }

    public void display(T item) {
        int p = find(item);
        if(p >= 0)
            display(p);
    }

    public void display() {
        display(sel);
    }

    public void resize(Coord sz) {
        super.resize(sz);
        this.h = Math.max(sz.y / itemh, 1);
        sb.resize(sz.y);
        sb.c = new Coord(sz.x - sb.sz.x, 0);
    }
}
