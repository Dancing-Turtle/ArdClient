package haven;

import java.awt.Color;


public class PlantStageSprite extends Sprite {
    private static final Color stagecolor = new Color(255, 227, 168);
    private static final Tex stgmaxtex = Text.renderstroked("\u25CF", new Color(254, 100, 100), Color.BLACK, Text.num12boldFnd).tex();
    private static final Tex stghrvtex = Text.renderstroked("\u25CF", new Color(201, 180, 0), Color.BLACK, Text.num12boldFnd).tex();
    private static final Tex[] stgtex = new Tex[]{
            Text.renderstroked("2", stagecolor, Color.BLACK, Text.num12boldFnd).tex(),
            Text.renderstroked("3", stagecolor, Color.BLACK, Text.num12boldFnd).tex(),
            Text.renderstroked("4", stagecolor, Color.BLACK, Text.num12boldFnd).tex(),
            Text.renderstroked("5", stagecolor, Color.BLACK, Text.num12boldFnd).tex(),
            Text.renderstroked("6", stagecolor, Color.BLACK, Text.num12boldFnd).tex()
    };
    private static final Tex[] stgtexalt = new Tex[]{
            Text.renderstroked("1", stagecolor, Color.BLACK, Text.num12boldFnd).tex(),
            Text.renderstroked("2", stagecolor, Color.BLACK, Text.num12boldFnd).tex(),
            Text.renderstroked("3", stagecolor, Color.BLACK, Text.num12boldFnd).tex(),
            Text.renderstroked("4", stagecolor, Color.BLACK, Text.num12boldFnd).tex(),
            Text.renderstroked("5", stagecolor, Color.BLACK, Text.num12boldFnd).tex(),
            Text.renderstroked("6", stagecolor, Color.BLACK, Text.num12boldFnd).tex()
    };
    public int stg;
    private Tex tex;
    private static Matrix4f mv = new Matrix4f();
    private Projection proj;
    private Coord wndsz;
    private Location.Chain loc;
    private Camera camp;
    private final boolean multistg, offsetmultisg;

    public PlantStageSprite(int stg, int stgmax, boolean multistg, boolean offsetmultisg) {
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
        if (multistg && stg == stgmax - 1)
            tex = stghrvtex;
        else if(offsetmultisg && stg == stgmax - 2)
            tex = stghrvtex;
        else if (stg == stgmax)
            tex = stgmaxtex;
        else {
            if(Config.showfreshcropstage)
                tex = stgtexalt[stg];
            else
                tex = stgtex[stg - 1];
        }
    }

    public Object staticp() {
        return CONSTANS;
    }
}
