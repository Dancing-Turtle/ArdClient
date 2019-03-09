package haven;

import java.awt.*;
import java.util.function.Consumer;

public class IndirColorPreview extends Widget {
    private final IndirSetting<Color> col;
    private ColorPicker cp = null;
    private final Consumer<Color> callback;

    public IndirColorPreview(Coord sz, IndirSetting<Color> cl, Consumer<Color> callback){
	super(sz);
	this.callback = callback;
	col = cl;
	tooltip = RichText.Parser.quote(String.format("Red: %d\nGreen: %d\nBlue: %d\nAlpha: %d",
		col.get().getRed(),col.get().getGreen(),col.get().getBlue(),col.get().getAlpha()));
	tooltip = RichText.render((String)tooltip,200);
    }

    public IndirColorPreview(Coord sz, IndirSetting<Color> cl){
	this(sz, cl, null);
    }

    public void draw(GOut g){
	g.chcolor(col.get());
	g.frect(Coord.z,sz);
	g.chcolor();
    }

    public boolean mousedown(Coord c, int btn) {
	return true;
    }

    public boolean mouseup(Coord c, int btn) {
	if(btn == 1 && cp == null){
	    cp = new ColorPicker(col.get(), (color -> {
		cp = null;
		col.set(color);
		tooltip = RichText.Parser.quote(String.format("Red: %d\nGreen: %d\nBlue: %d\nAlpha: %d",
			col.get().getRed(),col.get().getGreen(),col.get().getBlue(),col.get().getAlpha()));
		tooltip = RichText.render((String)tooltip,200);
		if(callback != null)
		    callback.accept(col.get());
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
