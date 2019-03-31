package haven.sloth.gfx;

import haven.Coord;
import haven.GOut;
import haven.Text;
import haven.TextureAtlas;

import java.awt.*;

/**
 * basically the same as FastText, but more customizable
 */
public class TextMap {
    //Todo: Ability to create own instances of FastText on subsets of your own Foundry
    private final haven.TextureAtlas tex;
    private final int height;

    public TextMap(final String id, final Text.Foundry fnd,
		   final Color base, final Color stroke, final String charset) {
	tex = new TextureAtlas("FastText-" + id, new Coord(64, 64));
	for (final char chr : charset.toCharArray()) {
	    tex.add(chr + "-n", fnd.render(chr + "", base).img);
	    tex.add(chr + "-s", fnd.renderstroked(chr + "", base, stroke).img);
	}
	height = fnd.height();
    }

    public haven.TextureAtlas.Img ch(char c, boolean stroke) {
        return tex.get(c+"-"+(!stroke ? "n" : "s"));
    }

    public Coord size(final String text) {
        final Coord c = new Coord(0, height);
        for(final char chr : text.toCharArray()) {
            final haven.TextureAtlas.Img img = ch(chr, false);
            c.x += img.sz.x;
        }
        return c;
    }

    public Coord sizes(final String text) {
	final Coord c = new Coord(0, height);
	for(final char chr : text.toCharArray()) {
	    final haven.TextureAtlas.Img img = ch(chr, true);
	    c.x += img.sz.x;
	}
	return c;
    }

    private void aprint(GOut g, Coord c, double ax, double ay, boolean stroke, String text) {
	Coord lc = new Coord(c);
	if(ax > 0)
	    lc.x -= (!stroke ? size(text).x : sizes(text).x) * ax;
	if(ay > 0)
	    lc.y -= height * ay;
	for(int i = 0; i < text.length(); i++) {
	    TextureAtlas.Img ch = ch(text.charAt(i), stroke);
	    g.image(ch, lc);
	    lc.x += ch.sz.x;
	}
    }

    public void aprint(GOut g, Coord c, double ax, double ay, String text) {
	aprint(g, c, ax, ay, false, text);
    }

    public void aprintf(GOut g, Coord c, double ax, double ay, String fmt, Object... args) {
	aprint(g, c, ax, ay, false, String.format(fmt, args));
    }

    public void print(GOut g, Coord c, String text) {
	aprint(g, c, 0.0, 0.0, false, text);
    }

    public void printf(GOut g, Coord c, String fmt, Object... args) {
	aprint(g, c, 0.0, 0.0, false, String.format(fmt, args));
    }

    public void aprints(GOut g, Coord c, double ax, double ay, String text) {
	aprint(g, c, ax, ay, true, text);
    }

    public void aprintsf(GOut g, Coord c, double ax, double ay, String fmt, Object... args) {
	aprint(g, c, ax, ay, true, String.format(fmt, args));
    }

    public void prints(GOut g, Coord c, String text) {
	aprint(g, c, 0.0, 0.0, true, text);
    }

    public void printsf(GOut g, Coord c, String fmt, Object... args) {
	aprint(g, c, 0.0, 0.0, true, String.format(fmt, args));
    }
}
