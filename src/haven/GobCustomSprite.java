package haven;

import java.awt.*;


public class GobCustomSprite extends Sprite {
    private static Tex hlt0 = Text.renderstroked("0", new Color(255, 227, 168), Color.BLACK, Text.num12boldFnd).tex();
    public String val;
    private Tex tex;
    private static Matrix4f mv = new Matrix4f();
    private Projection proj;
    private Coord wndsz;
    private Location.Chain loc;
    private Camera camp;
    private int life;
    private boolean haslife;
    private long time;

    public GobCustomSprite(String val, final int life) {
        super(null, null);
        this.life = life;
        haslife = life != -1;
        update(val);
    }

    public void draw(GOut g) {
        float[] c = mv.load(camp.fin(Matrix4f.id)).mul1(loc.fin(Matrix4f.id)).homoc();
        Coord sc = proj.get2dCoord(c, wndsz);
        sc.x -= tex.sz().x/2;
        sc.y -= 80;
        g.image(tex, sc);
    }

    public boolean tick(int dt) {
        super.tick(dt);
        time += dt;
        if(haslife) {
            life -= dt;
            return life <= 0;
        } else {
            return false;
        }
    }

    public boolean setup(RenderList rl) {
        rl.prepo(last);
        GLState.Buffer buf = rl.state();
        proj = buf.get(PView.proj);
        wndsz = buf.get(PView.wnd).sz();
        loc = buf.get(PView.loc);
        camp = buf.get(PView.cam);
        hlt0 = Text.renderstroked(val, new Color(255, 227, 168), Color.BLACK, Text.num12boldFnd).tex();
        return true;
    }

    public void update(String val) {
        this.val = val;
        hlt0 = Text.renderstroked(val, new Color(255, 227, 168), Color.BLACK, Text.num12boldFnd).tex();
        tex = hlt0;
    }
}
