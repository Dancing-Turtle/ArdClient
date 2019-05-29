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

import java.awt.*;

import static haven.Gob.SEMISTATIC;

/**
 *
 * TODO: Think of a way to represent the stage in 3D to avoid static/semistatic mess.
 */
public class GobCustomAttr extends GAttrib {
    private static final Tex[] gobhp = new Tex[] {
	    Text.renderstroked("25%", Color.WHITE, Color.BLACK, Gob.gobhpf).tex(),
	    Text.renderstroked("50%", Color.WHITE, Color.BLACK, Gob.gobhpf).tex(),
	    Text.renderstroked("75%", Color.WHITE, Color.BLACK, Gob.gobhpf).tex()
    };
    public static final Text.Foundry nfnd = new Text.Foundry(Text.dfont, Text.cfg.charName);
    private static Tex gobattr = Text.renderstroked("1",Color.WHITE,Color.BLACK,nfnd).tex();
    public String value = "";

    Material.Colors fx;
    public PView.Draw2D qualityfx;

    public GobCustomAttr(Gob g, String value) {
        super(g);
        this.value = value;
        this.fx = new Material.Colors(new Color(255, 0, 0, 255));
        gobattr = Text.renderstroked(value,Color.WHITE,Color.BLACK,nfnd).tex();
        qualityfx = new PView.Draw2D() {
	    public void draw2d(GOut g) {
		if(gob.sc != null && !value.equals("")) {
            g.image(gobattr, gob.sc.sub(15, -100));
		}
	    }
	};
    }

    public GLState getfx() {
        if (!value.equals(""))
            return (GLState.nullstate);
        return (fx);
    }

    public Object staticp() {
        if(!value.equals(""))
	    return super.staticp();
        else
            return SEMISTATIC;
    }
}
