package haven.sloth.util;

import java.util.Collection;

public interface ObservableListener<T> {
    void init(Collection<T> base);
    void added(T item);
    void remove(T item);
}

