import java.awt.Graphics;
import java.awt.image.BufferedImage;

import haven.ItemInfo;
import haven.ItemInfo.InfoFactory;
import haven.ItemInfo.Name;
import haven.ItemInfo.Owner;
import haven.PUtils;
import haven.Resource;
import haven.TexI;

public class RealmName implements InfoFactory {
    public RealmName() {
    }

    public ItemInfo build(Owner var1, Object... var2) {
        return new RealmName$1(this, var1, var2[1] + "");
    }

    class RealmName$1 extends Name {
        RealmName$1(RealmName var1, Owner var2, String var3) {
            super(var2, var3);
        }

        public BufferedImage tipimg() {
            BufferedImage var1 = TexI.mkbuf(PUtils.imgsz(this.str.img).add(0, 10));
            Graphics var2 = var1.getGraphics();
            var2.drawImage(this.str.img, 0, 0, null);
            var2.dispose();
            return var1;
        }
    }
}