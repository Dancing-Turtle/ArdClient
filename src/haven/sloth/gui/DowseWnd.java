package haven.sloth.gui;

import haven.Coord;
import haven.Label;
import haven.Window;

import java.awt.*;
import java.util.function.Consumer;


public class DowseWnd extends Window {
    private final Runnable onClose;

    public DowseWnd(final double a1, final double a2,
		    final Consumer<Color> changeCol, final Runnable onClose) {
        super(Coord.z, "Dowse", "Dowse");
        this.onClose = onClose;
        final Label la1 = new Label(String.format("Left Angle: %.2f", Math.toDegrees(a1)));
        final Label la2 = new Label(String.format("Right Angle: %.2f", Math.toDegrees(a2)));
        final Label lcol = new Label("Dowse Color:");
        final ColorPreview col = new ColorPreview(new Coord(16, 16),
		new Color(255, 0, 0, (int)(255*.30)),
		changeCol);
        final int spacer = 5;
        add(la1, Coord.z);
        add(la2, new Coord(0, la1.sz.y + spacer));
        add(lcol, la2.c.add(0, la2.sz.y + spacer));
        add(col, lcol.c.add(lcol.sz.x + spacer, 0));
        pack();
    }

    @Override
    public void close() {
        onClose.run();
	ui.destroy(this);
    }
}
