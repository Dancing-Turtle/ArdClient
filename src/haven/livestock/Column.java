package haven.livestock;

import haven.Label;

public class Column {
    public Label lbl;
    public int index;
    public int x;

    public Column(Label lbl, int index, int x) {
        this.lbl = lbl;
        this.index = index;
        this.x = x;
    }
}