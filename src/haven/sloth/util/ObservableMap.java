package haven.sloth.util;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class ObservableMap<K, V> {
    private final Map<K, V> base;
    private final Set<ObservableMapListener<K, V>> listeners = new HashSet<>();

    public ObservableMap(Map<K, V> base) {
	this.base = base;
    }

    public void put(K key, V val) {
	base.put(key, val);
	listeners.forEach((lst) -> lst.put(key, val));
    }

    public void remove(K key) {
        base.remove(key);
	listeners.forEach((lst) -> lst.remove(key));
    }

    public V get(K key) {
        return base.get(key);
    }

    public V getOrDefault(K key, V def) {
        return base.getOrDefault(key, def);
    }

    public boolean containsKey(K key) {
	return base.containsKey(key);
    }

    public void addListener(final ObservableMapListener<K, V> listener) {
	listeners.add(listener);
	listener.init(base);
    }

    public void removeListener(final ObservableMapListener<K, V> listener) {
	listeners.remove(listener);
    }
}
