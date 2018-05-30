package haven;

import java.awt.Color;

import javax.media.opengl.GL2;

public class FightCurrentOpp extends Sprite {
    private static final States.ColState clrstate = new States.ColState(new Color(255, 255, 0, 200));

    public FightCurrentOpp() {
        super(null, null);
    }

    public boolean setup(RenderList rl) {
        rl.prepo(clrstate);
        return true;
    }

    public void draw(GOut g) {
        g.apply();
        BGL gl = g.gl;
        gl.glLineWidth(12f);
        gl.glBegin(GL2.GL_LINES);
        gl.glVertex3f(-12, -12, 2);
        gl.glVertex3f(12, 12, 2);
        gl.glVertex3f(12, -12, 2);
        gl.glVertex3f(-12, 12, 2);
        gl.glEnd();
    }
}
