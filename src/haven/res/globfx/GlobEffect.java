package haven.res.globfx;

import haven.res.lib.globfx.Effect;

public abstract class GlobEffect implements Effect {
    public int hashCode() {
	return(this.getClass().hashCode());
    }

    public boolean equals(Object o) {
	return(this.getClass() == o.getClass());
    }
}
