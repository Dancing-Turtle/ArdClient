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

import haven.DefSettings;

import java.awt.Color;

import static haven.Gob.SEMISTATIC;

/**
 *
 * TODO: Think of a way to represent the stage in 3D to avoid static/semistatic mess.
 */
public class GobHealth extends GAttrib {
    private static final Tex[] gobhp = new Tex[] {
	    Text.renderstroked("25%", Color.WHITE, Color.BLACK, Gob.gobhpf).tex(),
	    Text.renderstroked("50%", Color.WHITE, Color.BLACK, Gob.gobhpf).tex(),
	    Text.renderstroked("75%", Color.WHITE, Color.BLACK, Gob.gobhpf).tex()
    };
    public int hp;
    Material.Colors fx;
    public PView.Draw2D hpfx;

    public GobHealth(Gob g, int hp) {
        super(g);
        this.hp = hp;
        this.fx = new Material.Colors(new Color(255, 0, 0, 128 - ((hp * 128) / 4)));
	hpfx = new PView.Draw2D() {
	    public void draw2d(GOut g) {
		if(gob.sc != null && hp < 4) {
		    g.image(gobhp[hp-1], gob.sc.sub(15, 10));
		}
	    }
	};
    }

    public GLState getfx() {
        if (hp >= 4)
            return (GLState.nullstate);
        return (fx);
    }

    public double asfloat() {
        return (((double) hp) / 4.0);
    }

    public Object staticp() {
        if(!DefSettings.SHOWGOBHP.get() || hp >= 4)
	    return super.staticp();
        else
            return SEMISTATIC;
    }
}
