package haven.sloth.gob;

import haven.GAttrib;
import haven.Gob;

public class Holding extends GAttrib {
    public final Gob held;

    public Holding(final Gob g, final Gob held) {
        super(g);
        this.held = held;
    }
}
