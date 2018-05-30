package haven.livestock;


import java.util.HashMap;
import java.util.Map;

import haven.Resource;
import haven.Tex;

public class Horses extends Animal {
    private int attributesRequired = columns.size() + 1;
    private static final Tex texMare = Resource.loadtex("gfx/livestockava/mare");
    private static final Tex texStallion = Resource.loadtex("gfx/livestockava/stallion");

    public static final Map<String, Column> columns = new HashMap<>(11);

    static {
        int x = 0;
        x = addColumn(columns, "Quality:", "Quality", 0, x);
        x = addColumn(columns, "Breeding quality:", "Breeding", 1, x);
        x = addColumn(columns, "Meat quality:", "Meat", 2, x);
        x = addColumn(columns, "Milk quality:", "Milk", 3, x);
        x = addColumn(columns, "Hide quality:", "Hide", 4, x);
        x = addColumn(columns, "Meat quantity:", "Meat #", 5, x);
        x = addColumn(columns, "Milk quantity:", "Milk #", 6, x);
        x = addColumn(columns, "Endurance:", "Endurance", 7, x);
        x = addColumn(columns, "Stamina:", "Stamina", 8, x);
        x = addColumn(columns, "Metabolism:", "Metabolism", 9, x);
            addColumn(columns, "X", "", 10, x);
    }

    public Horses(long wndid, String type) {
        super(wndid, type);
    }

    public Map<String, Column> getColumns() {
        return columns;
    }

    public void attributeResolved() {
        attributesRequired--;
    }

    public boolean hasAllAttributes() {
        return attributesRequired == 0;
    }

    public Tex getAvatar() {
        return type.equals("Mare") ? texMare : texStallion;
    }
}
