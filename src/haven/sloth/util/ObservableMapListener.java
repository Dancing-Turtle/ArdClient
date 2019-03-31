package haven.sloth.util;

import java.util.Map;

public interface ObservableMapListener<K, V> {
    void init(Map<K, V> base);
    void put(K key, V val);
    void remove(K key);
}
