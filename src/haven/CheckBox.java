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

import java.util.function.Consumer;

public class CheckBox extends Widget {
    //These are quite only for precautions about res classes
    public static final Tex lbox = Theme.tex("chkbox/large", 0),
            lmark = Theme.tex("chkbox/large", 1);
    public static final Tex sbox = Theme.tex("chkbox/small", 0),
            smark = Theme.tex("chkbox/small", 1);
    private final String type;
    public boolean a = false;
    private Consumer<Boolean> onChange;
    Text lbl;

    @RName("chk")
    public static class $_ implements Factory {
        public Widget create(UI ui, Object[] args) {
            CheckBox ret = new CheckBox((String) args[0]);
            ret.canactivate = true;
            return (ret);
        }
    }

    public CheckBox(String lbl, boolean lg, final Consumer<Boolean> onChange) {
        this.lbl = Text.std.render(Resource.getLocString(Resource.BUNDLE_LABEL, lbl), java.awt.Color.WHITE);
        if (lg) {
	    type = "chkbox/large";
        } else {
	    type = "chkbox/small";
        }
	final Coord boxsz = Theme.timg(type, 0).sz;
	sz = new Coord(boxsz.x + 5 + this.lbl.sz().x, Math.max(boxsz.y, this.lbl.sz().y));
	this.onChange = onChange;
    }

    public CheckBox(String lbl, boolean lg) {
	this(lbl, lg, null);
    }

    public CheckBox(String lbl) {
	this(lbl, false, null);
    }

    public CheckBox(final String lbl, final Consumer<Boolean> onChange, final boolean val) {
        this(lbl, false, onChange);
        this.a = val;
    }

    public boolean mousedown(Coord c, int button) {
        if (button != 1)
            return (false);
        set(!a);
        return (true);
    }

    public void set(boolean a) {
        this.a = a;
        changed(a);
    }

    public void draw(GOut g) {
	final int id = a ? 1 : 0;
	final TextureAtlas.Img chk = Theme.timg(type, id);
	g.image(chk, new Coord(0, sz.y / 2 - chk.sz.y / 2));
	//Draw label
	g.image(lbl.tex(), new Coord(chk.sz.x + 5, sz.y /2 - lbl.sz().y / 2 ));
        super.draw(g);
    }

    public void changed(boolean val) {
        if (canactivate)
	    wdgmsg("ch", a?1:0);
	if(onChange != null)
	    onChange.accept(val);
    }

    public void uimsg(String msg, Object... args) {
	if(msg == "ch") {
	    this.a = ((Integer)args[0]) != 0;
	} else {
	    super.uimsg(msg, args);
	}
    }
}
