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

import java.awt.image.BufferedImage;

import haven.GItem;
import haven.Glob;
import haven.ItemInfo;
import haven.Resource;
import haven.RichText;
import haven.Utils;

public class Curiosity extends ItemInfo.Tip {
    public final int exp, mw, enc;
    public final double time;

    public Curiosity(Owner owner, int exp, int mw, int enc, int time) {
        super(owner);
        this.exp = exp;
        this.mw = mw;
        this.enc = enc;
        this.time = time / Glob.SERVER_TIME_RATIO / 60;
        if (owner instanceof GItem)
            ((GItem) owner).studytime = this.time;
    }

    private String timefmt() {
        int hours = (int) (time / 60.0);
        int minutes = (int) ((time % 60));

        String fmt = Resource.getLocString(Resource.BUNDLE_LABEL, "Study time: %s (LP/hour: $col[192,192,255]{%d})");
        String hstr = hours > 0 ? String.format(Resource.getLocString(Resource.BUNDLE_LABEL, "$col[192,255,192]{%d} h "), hours) : "";
        String mstr = minutes > 0 ? String.format(Resource.getLocString(Resource.BUNDLE_LABEL, "$col[192,255,192]{%d} m"), minutes) : "";

        return String.format(fmt, hstr + mstr, (int) Math.round(exp / (time / 60)));
    }

    public BufferedImage tipimg() {
        StringBuilder buf = new StringBuilder();
        if (exp > 0)
            buf.append(String.format(Resource.getLocString(Resource.BUNDLE_LABEL, "Learning points: $col[192,192,255]{%s}\n"), Utils.thformat(exp)));
        if (mw > 0)
            buf.append(String.format(Resource.getLocString(Resource.BUNDLE_LABEL, "Mental weight: $col[255,192,255]{%d}\n"), mw));
        if (enc > 0)
            buf.append(String.format(Resource.getLocString(Resource.BUNDLE_LABEL, "Experience cost: $col[255,255,192]{%d}\n"), enc));
        if (time > 0)
            buf.append(timefmt());
        return (RichText.render(buf.toString(), 0).img);
    }
}
