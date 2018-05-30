package haven.res.lib.globfx;

public abstract class GlobData implements Datum {
    public GlobData() {
    }

    public int hashCode() {
        return this.getClass().hashCode();
    }

    public boolean equals(Object var1) {
        return this.getClass() == var1.getClass();
    }
}
