package haven.sloth.script;

import haven.Coord2d;

public class PointerData {
    private String name;
    private Coord2d c;

    public PointerData(final String name, final Coord2d c) {
        this.name = name;
        this.c = c;
    }

    public synchronized void updatec(final Coord2d c) {
        this.c = c;
    }


    public synchronized void updateName(final String name) {
        this.name = name;
    }

    public synchronized Coord2d c() {
        return c;
    }

    public synchronized String name() {
        return name;
    }
}