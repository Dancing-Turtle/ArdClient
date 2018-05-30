package haven.res.ui.tt.defn;

import haven.GSprite;
import haven.ItemInfo;
import haven.ItemInfo.InfoFactory;
import haven.ItemInfo.Name;
import haven.ItemInfo.Owner;
import haven.ItemInfo.ResOwner;
import haven.ItemInfo.SpriteOwner;
import haven.Resource;
import haven.Resource.Tooltip;

public class DefName implements InfoFactory {
    public DefName() {
    }

    public ItemInfo build(Owner var1, Object... var2) {
        if (var1 instanceof SpriteOwner) {
            GSprite var3 = ((SpriteOwner)var1).sprite();
            if (var3 instanceof DynName) {
                return new Name(var1, ((DynName)var3).name());
            }
        }

        if (!(var1 instanceof ResOwner)) {
            return null;
        } else {
            Resource var5 = ((ResOwner)var1).resource();
            Tooltip var4 = (Tooltip)var5.layer(Resource.tooltip);
            if (var4 == null) {
                throw new RuntimeException("Item resource " + var5 + " is missing default tooltip");
            } else {
                return new Name(var1, var4.t);
            }
        }
    }
}