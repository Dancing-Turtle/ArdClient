package haven;

import java.awt.Color;


public class GobHealthSprite extends Sprite {
    private static final Tex hlt0 = Text.renderstroked("25%", new Color(255, 227, 168), Color.BLACK, Text.num12boldFnd).tex();
    private static final Tex hlt1 = Text.renderstroked("50%", new Color(255, 227, 168), Color.BLACK, Text.num12boldFnd).tex();
    private static final Tex hlt2 = Text.renderstroked("75%", new Color(255, 227, 168), Color.BLACK, Text.num12boldFnd).tex();
    public int val;
    private Tex tex;
    private static Matrix4f mv = new Matrix4f();
    private Projection proj;
    private Coord wndsz;
    private Location.Chain loc;
    private Camera camp;

    public GobHealthSprite(int val) {
        super(null, null);
        update(val);
    }

    public void draw(GOut g) {
        float[] c = mv.load(camp.fin(Matrix4f.id)).mul1(loc.fin(Matrix4f.id)).homoc();
        Coord sc = proj.get2dCoord(c, wndsz);
        sc.x -= 15;
        sc.y -= 20;
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
        switch (val - 1) {
            case 0:
                tex = hlt0;
                break;
            case 1:
                tex = hlt1;
                break;
            case 2:
                tex = hlt2;
                break;
        }
    }
}
