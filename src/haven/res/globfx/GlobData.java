package haven.res.globfx;

import haven.res.lib.globfx.Datum;

public abstract class GlobData implements Datum {
    public int hashCode() {
	return(this.getClass().hashCode());
    }

    public boolean equals(Object o) {
	return(this.getClass() == o.getClass());
    }
}