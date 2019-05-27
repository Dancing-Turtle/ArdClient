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
public class GobQuality extends GAttrib {
    private static final Tex[] gobhp = new Tex[] {
	    Text.renderstroked("25%", Color.WHITE, Color.BLACK, Gob.gobhpf).tex(),
	    Text.renderstroked("50%", Color.WHITE, Color.BLACK, Gob.gobhpf).tex(),
	    Text.renderstroked("75%", Color.WHITE, Color.BLACK, Gob.gobhpf).tex()
    };
    public static final Text.Foundry nfnd = new Text.Foundry(Text.dfont, Text.cfg.charName);
    private static Tex gobquality = Text.renderstroked("1",Color.WHITE,Color.BLACK,nfnd).tex();
    public int quality;

    Material.Colors fx;
    public PView.Draw2D qualityfx;

    public GobQuality(Gob g, int quality) {
        super(g);
        this.quality = quality;
        this.fx = new Material.Colors(new Color(255, 0, 0, 255));
        gobquality = Text.renderstroked(String.valueOf(quality),Color.WHITE,Color.BLACK,nfnd).tex();
        qualityfx = new PView.Draw2D() {
	    public void draw2d(GOut g) {
		if(gob.sc != null && quality > 0) {
            g.image(gobquality, gob.sc.sub(15, -100));
		}
	    }
	};
    }

    public GLState getfx() {
        if (quality > 0)
            return (GLState.nullstate);
        return (fx);
    }

    public double asfloat() {
        return (((double) quality / 4.0));
    }

    public Object staticp() {
        if(quality > 0)
	    return super.staticp();
        else
            return SEMISTATIC;
    }
}
