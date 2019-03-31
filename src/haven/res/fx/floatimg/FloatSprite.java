package haven.res.fx.floatimg;

import haven.*;

public class FloatSprite extends Sprite implements PView.Render2D {
    public final int ms; //How long we last for
    public Tex tex; //Our texture
    int sy; //Our location above the player
    double a = 0.0D; //How long its been.

    public int cury() {
	return this.sy - (int)(10.0D * this.a);
    }

    public FloatSprite(Sprite.Owner owner, Resource res, Tex tex, int time) {
	super(owner, res);
	this.tex = tex;
	this.ms = time;
	this.sy = place((Gob)owner, tex.sz().y);
    }

    FloatSprite(Sprite.Owner owner, Resource res) {
        super(owner, res);
        this.ms = -1;
	this.sy = -50;
    }

    public void updateTex(final Tex tex) {
        this.tex = tex;
    }

    private static int place(Gob gob, int height) {
	int myy = 0;

	for(final Gob.Overlay ol : gob.ols) {
	    if ((ol.spr instanceof FloatSprite)) {
		FloatSprite sprite = (FloatSprite)ol.spr;
		if(sprite.tex != null) {
		    int itsy = sprite.cury();
		    int itssz = sprite.tex.sz().y;
		    if (((itsy >= myy) && (itsy < myy + height)) || ((myy >= itsy) && (myy < itsy + itssz))) {
			myy = itsy - height;
			break;
		    }
		}
	    }
	}

	return myy;
    }

    public void draw2d(GOut g) {
        if(tex != null) {
	    Coord sc = ((Gob) this.owner).sc;
	    if (sc == null) {
		return;
	    }
	    int i;
	    if (this.a < 0.75D) {
		i = 255;
	    } else {
		i = (int) Utils.clip(255.0D * ((1.0D - this.a) / 0.25D), 0.0D, 255.0D);
	    }
	    g.chcolor(255, 255, 255, i);
	    Coord invsz = this.tex.sz().inv();
	    invsz.x /= 2;
	    invsz.y += cury();
	    invsz.y -= 15;
	    g.image(this.tex, sc.add(invsz));
	    g.chcolor();
	}
    }

    public boolean setup(RenderList rl) {
	return true;
    }

    public boolean tick(int dt) {
	if (ms > 0) {
	    this.a += dt / (double)this.ms;
	    return this.a >= 1.0D; //Once we're over 1.0D delete us
	} else {
	    return false;
	}
    }
}
