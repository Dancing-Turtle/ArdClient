package haven.res.fx.floatimg;

import haven.*;

import java.awt.*;
import java.awt.image.BufferedImage;

public class DamageText extends FloatSprite {
    public static final int id = -14115;
    public static final Text.Foundry fnd = new Text.Foundry(Text.sans, 10);
    private static final Color armorcol = new Color(136, 255, 136);
    private static final Color hhpcol = new Color(255, 204, 0);
    private static final Color shpcol = new Color(255,0,0);

    private int shp;
    private int hhp;
    private int armor;

    DamageText(Sprite.Owner owner, Resource res) {
	super(owner, res);
	shp = 0;
	hhp = 0;
	armor = 0;
    }

    private void remake() {
        final BufferedImage img = Utils.hconcat(fnd.render(shp+" ", shpcol).img,
		fnd.render(hhp+" ", hhpcol).img, fnd.render(armor+"", armorcol).img);
        updateTex(new TexI(Utils.outline2(img, Color.BLACK)));
    }

    void incshp(final int shp) {
        this.shp += shp;
        remake();
    }

    void inchhp(final int hhp) {
        this.hhp += hhp;
	remake();
    }

    void incarmor(final int armor) {
        this.armor += armor;
	remake();
    }

    public boolean tick(int dt) {
	//Never delete us
        return false;
    }
}
