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
import java.awt.Graphics;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.util.function.Consumer;
import java.util.regex.Pattern;

public class TextEntry extends SIWidget {
    public static final Color defcol = new Color(255, 205, 109), dirtycol = new Color(255, 232, 209);
    public static final Text.Foundry fnd = new Text.Foundry(Text.serif, 12).aa(true);
    public static final BufferedImage lcap = Theme.img("textedit", 0);
    public static final BufferedImage mext = Theme.img("textedit", 1);
    public static final BufferedImage rcap = Theme.img("textedit", 2);
    public static final BufferedImage caret = Resource.loadimg("gfx/hud/text/caret");
    public static final Coord toff = new Coord(lcap.getWidth() - 1, 3);
    public static final Coord coff = new Coord(-3, -1);
    public static final int wmarg = lcap.getWidth() + rcap.getWidth() + 1;
    public boolean dshow = false;
    public LineEdit buf;
    public int sx;
    public boolean pw = false;
    private final static Pattern numpat = Pattern.compile("(-)|(-?[0-9]+)");
    public boolean numeric = false;
    public String text;
    private boolean dirty = false;
    private double focusstart;
    private Text.Line tcache = null;

    @RName("text")
    public static class $_ implements Factory {
        public Widget create(UI ui, Object[] args) {
            if (args[0] instanceof Coord)
                return (new TextEntry((Coord) args[0], (String) args[1]));
            else
                return (new TextEntry((Integer) args[0], (String) args[1]));
        }
    }

    public void settext(String text) {
        buf.setline(text);
        redraw();
    }

    public void rsettext(String text) {
        buf = new LineEdit(this.text = text) {
            protected void done(String line) {
                activate(line);
            }

            protected void changed() {
		    if(!numeric || (line.length() == 0 || numpat.matcher(line).matches())) {
                redraw();
                TextEntry.this.text = line;
                TextEntry.this.changed();
		    } else {
		        //deny
		        line = TextEntry.this.text;
		    }
            }
        };
        redraw();
    }

    public void setpw(final boolean val) {
        this.pw = val;
        commit();
    }
    public void commit() {
        dirty = false;
        redraw();
    }

    public void uimsg(String name, Object... args) {
        if (name == "settext") {
            settext((String) args[0]);
        } else if (name == "get") {
            wdgmsg("text", buf.line);
        } else if (name == "pw") {
            pw = ((Integer) args[0]) != 0;
        } else if (name == "dshow") {
            dshow = ((Integer) args[0]) != 0;
        } else if (name == "cmt") {
            commit();
        } else {
            super.uimsg(name, args);
        }
    }

    public int numvalue() {
        if(numeric) {
            if(text.length() == 0)
                return 0;
            else if(text.equals("-"))
                return 0;
            else
                return Integer.parseInt(text);
	} else {
            return 0;
	}
    }
    protected String dtext() {
        if (pw) {
            String ret = "";
            for (int i = 0; i < buf.line.length(); i++)
                ret += "\u2022";
            return (ret);
        } else {
            return (buf.line);
        }
    }

    public void draw(BufferedImage img) {
        Graphics g = img.getGraphics();
        String dtext = dtext();
        tcache = fnd.render(dtext, (dshow && dirty) ? dirtycol : defcol);

        g.drawImage(lcap, 0, 0, null);
	g.drawImage(mext, lcap.getWidth(), 0, sz.x - lcap.getWidth() - rcap.getWidth(), sz.y, null);
        g.drawImage(rcap, sz.x - rcap.getWidth(), 0, null);

	g.drawImage(tcache.img, toff.x - sx, toff.y, null);
        g.dispose();
    }

    public void draw(GOut g) {
	g.chcolor(DefSettings.TXBCOL.get());
        super.draw(g);
	g.chcolor();
        if (hasfocus) {
            int cx = tcache.advance(buf.point);
            int lx = cx - sx + 1;
	    if(cx < sx) {sx = cx; redraw();}
	    if(cx > sx + (sz.x - wmarg)) {sx = cx - (sz.x - wmarg); redraw();}
            if (((Utils.rtime() - focusstart) % 1.0) < 0.5)
                g.image(caret, toff.add(coff).add(lx, 0));
        }
    }

    private final Consumer<String> onChange;
    private final Consumer<String> onActivate;

    public TextEntry(final int w, final String deftext, final Consumer<String> onChange, final Consumer<String> onActivate) {
        super(new Coord(w, mext.getHeight()));
	this.onChange = onChange;
	this.onActivate = onActivate;
        rsettext(deftext);
        setcanfocus(true);
    }

    public TextEntry(int w, String deftext) {
        this(w, deftext, null, null);
    }
    @Deprecated
    public TextEntry(Coord sz, String deftext) {
        this(sz.x, deftext);
    }

    protected void changed() {
        dirty = true;
	if(onChange != null)
	    onChange.accept(text);
    }

    public void activate(String text) {
        if (canactivate)
            wdgmsg("activate", text);
	if(onActivate != null)
	    onActivate.accept(text);
    }

    public boolean type(char c, KeyEvent ev) {
        return (buf.key(ev));
    }

    public boolean keydown(KeyEvent e) {
        if(e.getKeyCode() == KeyEvent.VK_F1 || e.getKeyCode() == KeyEvent.VK_F2|| e.getKeyCode() == KeyEvent.VK_F3|| e.getKeyCode() == KeyEvent.VK_F4|| e.getKeyCode() == KeyEvent.VK_F5
                || e.getKeyCode() == KeyEvent.VK_F6|| e.getKeyCode() == KeyEvent.VK_F7|| e.getKeyCode() == KeyEvent.VK_F8|| e.getKeyCode() == KeyEvent.VK_F9|| e.getKeyCode() == KeyEvent.VK_F10
                || e.getKeyCode() == KeyEvent.VK_F11|| e.getKeyCode() == KeyEvent.VK_F12 || e.getModifiers() == InputEvent.ALT_MASK || e.getModifiers() == InputEvent.CTRL_MASK) {
            return super.keydown(e);
        }
        buf.key(e);
        return (true);
    }

    public boolean mousedown(Coord c, int button) {
        parent.setfocus(this);
        if (tcache != null) {
            buf.point = tcache.charat(c.x + sx);
        }
        return (true);
    }

    public void gotfocus() {
        focusstart = Utils.rtime();
    }

    public void resize(int w) {
        resize(w, sz.y);
    }
}
