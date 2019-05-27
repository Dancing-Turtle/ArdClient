package haven;

import java.awt.*;


public class GobQualitySprite extends Sprite {
    private static Tex hlt0 = Text.renderstroked("0", new Color(255, 227, 168), Color.BLACK, Text.num12boldFnd).tex();
    public int val;
    private Tex tex;
    private static Matrix4f mv = new Matrix4f();
    private Projection proj;
    private Coord wndsz;
    private Location.Chain loc;
    private Camera camp;

    public GobQualitySprite(int val) {
        super(null, null);
        update(val);
    }

    public void draw(GOut g) {
        float[] c = mv.load(camp.fin(Matrix4f.id)).mul1(loc.fin(Matrix4f.id)).homoc();
        Coord sc = proj.get2dCoord(c, wndsz);
        sc.x -= tex.sz().x/2;
        sc.y -= 40;
        g.image(tex, sc);
    }

    public boolean setup(RenderList rl) {
        rl.prepo(last);
        GLState.Buffer buf = rl.state();
        proj = buf.get(PView.proj);
        wndsz = buf.get(PView.wnd).sz();
        loc = buf.get(PView.loc);
        camp = buf.get(PView.cam);
        hlt0 = Text.renderstroked(String.valueOf(val), new Color(255, 227, 168), Color.BLACK, Text.num12boldFnd).tex();
        return true;
    }

    public void update(int val) {
        this.val = val;
        hlt0 = Text.renderstroked("Quality "+String.valueOf(val), new Color(255, 227, 168), Color.BLACK, Text.num12boldFnd).tex();
        tex = hlt0;
    }
}
