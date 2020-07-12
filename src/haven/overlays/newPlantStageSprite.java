package haven.overlays;


import haven.*;

import java.awt.*;

public class newPlantStageSprite extends Sprite {
    public int stg;
    private Tex tex;
    private static Matrix4f mv = new Matrix4f();
    private Projection proj;
    private Coord wndsz;
    private Location.Chain loc;
    private Camera camp;
    private final boolean multistg, offsetmultisg;

    Tex stg(int stg, int stgmax, Color clr) {
        Tex tex = Text.renderstroked(String.format("%d/%d",  stg, stgmax), clr, Color.BLACK, Text.num12boldFnd).tex();
        return tex;
    }

    Tex stg(int stg, int stgmax, Color clr, Color border) {
        Tex tex = Text.renderstroked(String.format("%d/%d",  stg, stgmax), clr, border, Text.num12boldFnd).tex();
        return tex;
    }

    public newPlantStageSprite(int stg, int stgmax, boolean multistg, boolean offsetmultisg) {
        super(null, null);
        this.multistg = multistg;
        this.offsetmultisg = offsetmultisg;
        update(stg, stgmax);
    }

    public void draw(GOut g) {
        float[] c = mv.load(camp.fin(Matrix4f.id)).mul1(loc.fin(Matrix4f.id)).homoc();
        Coord sc = proj.get2dCoord(c, wndsz);
        sc.x -= tex.sz().x/2;
        sc.y -= 10;
        g.image(tex, sc);
    }

    public boolean setup(RenderList rl) {
        rl.prepo(last);
        GLState.Buffer buf = rl.state();
        proj = buf.get(PView.proj);
        wndsz = buf.get(PView.wnd).sz();
        loc = buf.get(PView.loc);
        camp = buf.get(PView.cam);
        return true;
    }

    public void update(int stg, int stgmax) {
        this.stg = stg;
        int truestg = stg + 1;
        int truestgmax = stgmax + 1;

        if (stgmax == 0)
            tex = stg(stg, stgmax, Color.gray);
        else if (stg > stgmax)
            tex = stg(truestg, truestgmax, Color.blue);
        else if ((multistg || offsetmultisg) && stg == stgmax - 1)
            tex = stg(truestg, truestgmax, Color.yellow);
        else if (offsetmultisg && stg == stgmax - 2)
            tex = stg(truestg, truestgmax, new Color(255, 100, 50));
        else if (stg == stgmax)
            tex = stg(truestg, truestgmax, Color.green, Color.gray);
        else {
            if (Config.showfreshcropstage)
                tex = stg(truestg, truestgmax, Color.red);
            else
                tex = stg(truestg, truestgmax, Color.red);
        }
    }

    public Object staticp() {
        return CONSTANS;
    }
}
