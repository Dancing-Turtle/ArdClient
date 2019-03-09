package haven;

import java.awt.*;
import java.util.function.Consumer;

/**
 * Color preview square, only really for options
 */
public class ColorPreview extends Widget {
    private Color col;
    private ColorPicker cp = null;
    private final Consumer<Color> callback;
  
    public ColorPreview(Coord sz, Color cl, Consumer<Color> callback){
	super(sz);
	this.callback = callback;
	col = cl;
	tooltip = RichText.Parser.quote(String.format("Red: %d\nGreen: %d\nBlue: %d\nAlpha: %d",
		col.getRed(),col.getGreen(),col.getBlue(),col.getAlpha()));
	tooltip = RichText.render((String)tooltip,200);
    }

    ColorPreview(Coord sz, Color cl){
        this(sz, cl, null);
    }

    public void draw(GOut g){
	g.chcolor(col);
	g.frect(Coord.z,sz);
	g.chcolor();
    }

    public void setColor(final Color ncol) {
        this.col = ncol;
    }

    public boolean mousedown(Coord c, int btn) {
	return true;
    }

    public boolean mouseup(Coord c, int btn) {
	if(btn == 1 && cp == null && callback != null){
	    cp = new ColorPicker(col, (color -> {
		cp = null;
		col = color;
		tooltip = RichText.Parser.quote(String.format("Red: %d\nGreen: %d\nBlue: %d\nAlpha: %d",
			col.getRed(),col.getGreen(),col.getBlue(),col.getAlpha()));
		tooltip = RichText.render((String)tooltip,200);
		callback.accept(col);
	    }));

	    if(ui.gui != null) {
		ui.gui.add(cp, new Coord(50, 50));
	    } else {
	        ui.root.add(cp, new Coord(50, 50));
	    }
	}
	return true;
    }
}
