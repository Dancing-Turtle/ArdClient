package haven;

public class WidgetVerticalAppender {
    public WidgetVerticalAppender(Widget widget) {
        this.widget = widget;
        this.verticalMargin = 0;
        this.horizontalMargin = 0;
        this.y = 0;
        this.x = 0;
    }

    public void setX(int value) {
        this.x = value;
    }

    public void setVerticalMargin(int value) {
        verticalMargin = value;
    }

    public void setHorizontalMargin(int value) {
        horizontalMargin = value;
    }

    public <T extends Widget> void add(T child) {
        widget.add(child, new Coord(x, y));
        y += child.sz.y + verticalMargin;
    }

    public void addRow(Widget ... children) {
        int x = this.x;
        int maxHeight = 0;
        for (Widget child : children) {
            widget.add(child, new Coord(x, y));
            x += child.sz.x + horizontalMargin;
            if (maxHeight < child.sz.y) {
                maxHeight = child.sz.y;
            }
        }
        y += maxHeight + verticalMargin;
    }

    private final Widget widget;
    private int verticalMargin;
    private int horizontalMargin;
    private int y;
    private int x;
}
