package haven;

public class CheckListboxItem implements Comparable<CheckListboxItem> {
    public String name;
    public boolean selected;

    public CheckListboxItem(String name) {
        this.name = Resource.getLocString(Resource.BUNDLE_INGREDIENT, name);
    }

    public CheckListboxItem(String name, String locBundle) {
        this.name = Resource.getLocString(locBundle, name);
    }

    @Override
    public int compareTo(CheckListboxItem o) {
        return this.name.compareTo(o.name);
    }
}
