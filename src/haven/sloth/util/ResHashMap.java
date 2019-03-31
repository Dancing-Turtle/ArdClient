package haven.sloth.util;


import java.util.HashMap;
import java.util.Optional;

/**
 * A HashMap for Resource file names.
 * If a specific key fails it'll try to look at the more general key.
 * ie: gfx/terobjs/trees/laurel -> gfx/terobjs/trees -> gfx/terobjs -> gfx -> ''
 *
 */
public class ResHashMap<E> {
    private HashMap<String,E> map = new HashMap<>();

    public void put(final String key, final E value) {
        map.put(key, value);
    }

    public Optional<E> get(String key) {
        if(key != null && !key.equals("")) {
            do {
		if (map.containsKey(key)) {
		    return Optional.of(map.get(key));
		} else if (key.contains("/")) {
		    key = key.substring(0, key.lastIndexOf('/'));
		} else {
		    //One choice left
		    if(map.containsKey(""))
		        return Optional.of(map.get(""));
		    else
		        return Optional.empty();
		}
	    } while(true);
	} else {
            return Optional.empty();
	}
    }
}
