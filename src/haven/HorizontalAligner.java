package haven;

import java.util.Collections;
import java.util.List;

public class HorizontalAligner {
    static public <T extends Widget> int apply(List<T> children, int space) {
        if (space < 0) {
            throw new IllegalArgumentException("HorizontalAligner.apply: space < 0");
        } else if (children.size() < 1) {
            return 0;
        } else if (children.size() < 2) {
            return children.get(0).sz.x;
        } else if (children.size() < 3) {
            return children.get(1).c.x + children.get(1).sz.x;
        }
        final Widget widest = Collections.max(children, (lhs, rhs) -> Integer.compare(lhs.sz.x, rhs.sz.x));
        final float width = children.get(0).sz.x
                + widest.sz.x * (children.size() - 2)
                + children.get(children.size() - 1).sz.x
                + space * (children.size() - 1);
        final float elementWidth = width / children.size();
        int index = 0;
        for (final Widget child : children) {
            if (0 < index && index < children.size() - 1) {
                child.c.x = Math.round(elementWidth * (index + 1) - elementWidth / 2 - child.sz.x / 2);
            }
            ++index;
        }
        children.get(0).c.x = 0;
        children.get(children.size() - 1).c.x = Math.round(width) - children.get(children.size() - 1).sz.x;
        return Math.round(width);
    }
}
