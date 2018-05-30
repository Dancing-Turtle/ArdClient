package haven.res.ui.tt;

import haven.ItemInfo;
import haven.ItemInfo.InfoFactory;
import haven.ItemInfo.Owner;

public class WearFactory implements InfoFactory {
    public WearFactory() {
    }

    public ItemInfo build(Owner var1, Object... var2) {
        return new Wear(var1, ((Integer) var2[1]).intValue(), ((Integer) var2[2]).intValue());
    }
}
