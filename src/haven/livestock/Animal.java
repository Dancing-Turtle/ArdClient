package haven.livestock;


import java.awt.Color;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import haven.Coord;
import haven.Label;
import haven.Resource;
import haven.Scrollport;
import haven.Tex;
import haven.Text;
import haven.Widget;


public abstract class Animal extends HashMap<String, Integer> {
    private final static int COL_SPACE;
    public long gobid;
    public long wndid;
    public String name;
    public String type;

    static {
        if (Resource.language.equals("ru"))
            COL_SPACE = 18;
        else
            COL_SPACE = 24;
    }

    public Animal(long wndid, String type) {
        super(13);
        this.wndid = wndid;
        this.type = type;
        put(Resource.getLocString(Resource.BUNDLE_LABEL, "Quality:"), null);
        put(Resource.getLocString(Resource.BUNDLE_LABEL, "Breeding quality:"), null);
        put(Resource.getLocString(Resource.BUNDLE_LABEL, "Meat quantity:"), null);
        put(Resource.getLocString(Resource.BUNDLE_LABEL, "Milk quantity:"), null);
        put(Resource.getLocString(Resource.BUNDLE_LABEL, "Meat quality:"), null);
        put(Resource.getLocString(Resource.BUNDLE_LABEL, "Milk quality:"), null);
        put(Resource.getLocString(Resource.BUNDLE_LABEL, "Hide quality:"), null);
        // horse
        put(Resource.getLocString(Resource.BUNDLE_LABEL, "Endurance:"), null);
        put(Resource.getLocString(Resource.BUNDLE_LABEL, "Stamina:"), null);
        put(Resource.getLocString(Resource.BUNDLE_LABEL, "Metabolism:"), null);
        // sheep
        put(Resource.getLocString(Resource.BUNDLE_LABEL, "Wool quantity:"), null);
        put(Resource.getLocString(Resource.BUNDLE_LABEL, "Wool quality:"), null);
        //pig
        put(Resource.getLocString(Resource.BUNDLE_LABEL,"Truffle snout:"),null);
    }

    public static int addColumn(Map<String, Column> columns, String name, String displayName, int index, int x) {
        String nameloc = Resource.getLocString(Resource.BUNDLE_LABEL, name);
        Label lbl = new Label(displayName, Text.labelFnd) {
            @Override
            public boolean mousedown(Coord c, int button) {
                for (Widget child = parent.lchild; child != null; child = child.prev) {
                    if (child instanceof Label) {
                        Label lbl = ((Label) child);
                        if (lbl.col != Color.WHITE) {
                            lbl.setcolor(Color.WHITE);
                            break;
                        }
                    }
                }
                setcolor(Color.RED);

                Scrollport.Scrollcont cont = ((LivestockManager.Panel) parent).scrollPort.cont;
                List<DetailsWdg> entries = new ArrayList<>();
                for (Widget child = cont.lchild; child != null; child = child.prev)
                    entries.add((DetailsWdg) child);

                Collections.sort(entries, (a, b) -> {
                    int result = a.animal.type.compareTo(b.animal.type);
                    if (result != 0)
                        return result;
                    return a.animal.get(nameloc) < b.animal.get(nameloc) ? -1 : 1;
                });

                int y = 0;
                for (DetailsWdg details : entries) {
                    details.c = new Coord(0, y);
                    y += DetailsWdg.HEIGHT;
                }

                return true;
            }
        };
        columns.put(nameloc, new Column(lbl, index, x));
        return x + lbl.sz.x + COL_SPACE;
    }

    public abstract boolean hasAllAttributes();

    public abstract void attributeResolved();

    public abstract Tex getAvatar();

    public abstract Map<String, Column> getColumns();
}
