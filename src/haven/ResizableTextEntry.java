package haven;

import java.awt.font.FontRenderContext;
import java.awt.font.TextLayout;

public class ResizableTextEntry extends TextEntry {
    public int addWidth = 30;

    public ResizableTextEntry(String deftext) {
        super(0, deftext, null, null);
    }

    public void draw(GOut g) {
        sz = new Coord((int) getTextWidth(text), mext.getHeight());
        super.draw(g);
    }

    public double getTextWidth(String text) {
        double textWidth;
        if (text.length() > 0) textWidth = Math.ceil(new TextLayout(text, fnd.font, new FontRenderContext(null, true, true)).getBounds().getWidth());
        else textWidth = 0;
        return (int) textWidth + addWidth;
    }
}
