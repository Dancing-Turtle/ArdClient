package haven.res.fx.floatimg;

import haven.*;

import java.awt.*;

public class FloatText extends FloatSprite {
    private static final Color
	    shp = new Color(255, 0, 0),
            hhp = new Color(255, 204, 0),
	    armor = new Color(136, 255, 136);
    private static final Object lock = new Object();
    public static final Text.Foundry fnd = new Text.Foundry(Text.sans, 10);

    public FloatText(Sprite.Owner owner, Resource res, String text, Color color) {
	super(owner, res,
		new TexI(Utils.outline2(fnd.render(text, color).img, Utils.contrast(color))),
		2000);

	if(owner instanceof Gob) {
	    final Gob gob = (Gob) owner;
	    synchronized (lock) {
		Gob.Overlay ol = gob.findol(DamageText.id);
		if (ol == null) {
		    //Make a new damagetext
		    ol = gob.daddol(DamageText.id, new DamageText(owner, res));
		}

		final DamageText dmg = (DamageText) ol.spr;
		if (color.equals(shp)) {
		    updateDamageText(dmg, Integer.parseInt(text), 0, 0);
		} else if (color.equals(armor)) {
		    updateDamageText(dmg, 0, Integer.parseInt(text), 0);
		} else if (color.equals(hhp)) {
		    updateDamageText(dmg, 0, 0, Integer.parseInt(text));
		}
	    }
	}
    }

    private void updateDamageText(final DamageText dmg, final int shp, final int armor, final int hhp) {
    	if(shp != 0) {
    	    dmg.incshp(shp);
	} else if(armor != 0) {
    	    dmg.incarmor(armor);
	} else if(hhp != 0) {
    	    dmg.inchhp(hhp);
	}
    }
}
