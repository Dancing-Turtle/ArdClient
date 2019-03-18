package haven;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

public class ObservableCollection<T> implements Iterable<T> {
    private final Collection<T> base;
    private final Set<ObservableListener<T>> listeners = new HashSet<>();

    public ObservableCollection(Collection<T> base) {
        this.base = base;
    }

    public boolean add(T item) {
        if (base.add(item)) {
            listeners.forEach((lst) -> lst.added(item));
            return true;
        } else {
            return false;
        }
    }

    public boolean remove(T item) {
        if (base.remove(item)) {
            listeners.forEach((lst) -> lst.remove(item));
            return true;
        } else {
            return false;
        }
    }

    public boolean contains(T other) {
        return base.contains(other);
    }

    public void addListener(final ObservableListener<T> listener) {
        listeners.add(listener);
        listener.init(base);
    }

    public void removeListener(final ObservableListener<T> listener) {
        listeners.remove(listener);
    }

    public Iterator<T> iterator() {
        return base.iterator();
    }
}
