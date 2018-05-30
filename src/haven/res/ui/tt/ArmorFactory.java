package haven.res.ui.tt;

import haven.ItemInfo;
import haven.ItemInfo.InfoFactory;
import haven.ItemInfo.Owner;

public class ArmorFactory implements InfoFactory {
    public ArmorFactory() {
    }

    public ItemInfo build(Owner var1, Object... var2) {
        return new Armor(var1, ((Integer) var2[1]).intValue(), ((Integer) var2[2]).intValue());
    }
}
