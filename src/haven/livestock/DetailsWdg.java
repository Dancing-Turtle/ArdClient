package haven.livestock;


import static haven.OCache.posres;

import java.awt.Color;
import java.util.List;
import java.util.Map;
import static haven.OCache.posres;

import haven.*;

public class DetailsWdg extends Widget {
    public final static int HEIGHT = 25;
    private final Coord sepStart = new Coord(0, HEIGHT);
    private final Coord sepEnd = new Coord(800 - 40 - 11, HEIGHT);
    public Animal animal;
    private GameUI gui;
    public Label del;
    public Gob animal2;
    private boolean hover = false;
    public DetailsWdg(Animal animal) {
        this.animal = animal;

        add(new Img(animal.getAvatar()), Coord.z);

        int offx = LivestockManager.COLUMN_TITLE_X - LivestockManager.ENTRY_X;
        for (Map.Entry<String, Integer> entry : animal.entrySet()) {
            Integer val = entry.getValue();
            if (val == null)
                continue;

            String key = entry.getKey();
            Column col = animal.getColumns().get(key);

            String valStr = val.toString();
            if (key.equals(Resource.getLocString(Resource.BUNDLE_LABEL, "Meat quality:")) ||
                    key.equals(Resource.getLocString(Resource.BUNDLE_LABEL, "Milk quality:")) ||
                    key.equals(Resource.getLocString(Resource.BUNDLE_LABEL, "Hide quality:")) ||
                    key.equals(Resource.getLocString(Resource.BUNDLE_LABEL, "Wool quality:")) ||
                    key.equals(Resource.getLocString(Resource.BUNDLE_LABEL, "Endurance:")))
                valStr += "%";

            Label lbl = new Label(valStr, Text.labelFnd);
            add(lbl, new Coord(col.x + offx, 5));
        }

        del = new Label("\u2718", Text.delfnd, Color.RED) {
            @Override
            public boolean mousedown(Coord c, int button) {
                delete();
                return true;
            }
        };
        Column col = animal.getColumns().get("X");
        add(del, new Coord(col.x + offx, 3));
    }

    @Override
    public void draw(GOut g) {
        g.chcolor(255, 255, 255, 128);
        g.line(sepStart, sepEnd, 1);

        if (hover) {
            g.chcolor(255, 255, 255, 40);
            g.frect(Coord.z, sz);
        }

        g.chcolor();

        super.draw(g, true);
    }

    @Override
    public boolean mousedown(Coord c, int button) {
        Gob gob = gameui().map.glob.oc.getgob(animal.gobid);
        if (gob != null) {
            if (button == 3) {
                if (MapView.markedGobs.contains(gob.id)) {
                    MapView.markedGobs.remove(gob.id);
                    del.settext("\u2718");
                    del.setcolor(Color.BLUE);
                }
                else {
                    MapView.markedGobs.add(gob.id);
                    del.settext("\u2620");
                    del.setcolor(Color.RED);
                }
            }
            //gameui().map.wdgmsg("click", gob.sc, gob.rc.floor(posres), 3, 0, 0, (int) gob.id, gob.rc.floor(posres), 0, -1);
            if (button == 1) {
                if(gob.isDead() == Boolean.TRUE){
                    delete();
                }
                gob.delattr(GobHighlight.class);
                gob.setattr(new GobHighlight(gob));
            }

        }
        return super.mousedown(c, button);
    }

    @Override
    public void mousemove(Coord c) {
        hover = c.x > 0 && c.x < sz.x && c.y > 0 && c.y < sz.y;
        super.mousemove(c);
    }

    public void delete() {
        reqdestroy();
        int y = this.c.y;
        for (Widget child = parent.lchild; child != null; child = child.prev) {
            if (child instanceof DetailsWdg && child.c.y > y)
                child.c.y -= HEIGHT;
        }

        ((LivestockManager.Panel)parent.parent.parent).delete(animal);

        ((Scrollport.Scrollcont) parent).update();
    }
}


