package haven.sloth.gob;

import haven.GAttrib;
import haven.Gob;

public class HeldBy extends GAttrib {
    public final Gob holder;

    public HeldBy(final Gob g, final Gob holder) {
        super(g);
        this.holder = holder;
    }
}
