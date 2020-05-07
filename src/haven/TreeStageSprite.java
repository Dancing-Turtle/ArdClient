package haven;

import java.awt.Color;


public class TreeStageSprite extends Sprite {
    private static final Tex[] treestg = new Tex[100];
    private static final Color stagecolor = new Color(120, 255, 255);
    public int val;
    private Tex tex;
    private static Matrix4f mv = new Matrix4f();
    private Projection proj;
    private Coord wndsz;
    private Location.Chain loc;
    private Camera camp;

    static {
        for (int i = 0; i < 100; i++) {
            treestg[i] = Text.renderstroked(i + "", stagecolor, Color.BLACK, Text.num12boldFnd).tex();
        }
    }

    public TreeStageSprite(int val) {
        super(null, null);
        update(val);
    }

    public void draw(GOut g) {
        float[] c = mv.load(camp.fin(Matrix4f.id)).mul1(loc.fin(Matrix4f.id)).homoc();
        Coord sc = proj.get2dCoord(c, wndsz);
        sc.x -= 8;
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

    public void update(int val) {
        this.val = val;
        tex = treestg[val];
    }

    public Object staticp() {
        return CONSTANS;
    }
}
