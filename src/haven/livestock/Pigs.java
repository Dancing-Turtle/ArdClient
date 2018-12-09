package haven.livestock;


import java.util.HashMap;
import java.util.Map;

import haven.Resource;
import haven.Tex;

public class Pigs extends Animal {
    private int attributesRequired = columns.size() +1;
    private static final Tex texSow = Resource.loadtex("gfx/livestockava/sow");
    private static final Tex texHog = Resource.loadtex("gfx/livestockava/hog");

    public static final Map<String, Column> columns = new HashMap<>(13);

    static {
        int x = 0;
        x = addColumn(columns, "Quality:", "Quality", 0, x);
        x = addColumn(columns, "Breeding quality:", "Breeding", 1, x);
        x = addColumn(columns, "Combined quality:", "Combined", 2, x);
        x = addColumn(columns, "Meat quality:", "Meat", 3, x);
        x = addColumn(columns, "Milk quality:", "Milk", 4, x);
        x = addColumn(columns, "Hide quality:", "Hide", 5, x);
        x = addColumn(columns, "Meat quantity:", "Meat #", 6, x);
        x = addColumn(columns, "Milk quantity:", "Milk #", 7, x);
        x = addColumn(columns,"Truffle snout:","Snout",8,x);

        x = addColumn(columns, "Meat quality2:", "Meat Final", 9, x);
        x = addColumn(columns, "Milk quality2:", "Milk Final", 10, x);
        x = addColumn(columns, "Hide quality2:", "Hide Final", 11, x);

            addColumn(columns, "X", "", 12, x);
    }

    public Pigs(long wndid, String type) {
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
        return type.equals("Sow") ? texSow : texHog;
    }
}
