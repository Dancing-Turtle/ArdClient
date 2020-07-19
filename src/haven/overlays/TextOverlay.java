package haven.overlays;

import haven.Camera;
import haven.Coord;
import haven.GLState;
import haven.GOut;
import haven.Gob;
import haven.Location;
import haven.Matrix4f;
import haven.PView;
import haven.Projection;
import haven.RenderList;
import haven.Sprite;
import haven.Tex;
import haven.Text;

import java.awt.*;

public class TextOverlay extends Sprite {
    private Tex tex;
    private static Matrix4f mv = new Matrix4f();
    private Projection proj;
    private Coord wndsz;
    private Location.Chain loc;
    private Camera camp;

    public TextOverlay(OverlayData.OverlayGob overlayGob) {
        super(null, null);
        update(overlayGob);
    }

    public TextOverlay(Gob gob) {
        super(null, null);
        update(OverlayData.get(gob.resname().get()));
    }

    public void draw(GOut g) {
        float[] c = mv.load(this.camp.fin(Matrix4f.id)).mul1(this.loc.fin(Matrix4f.id)).homoc();
        Coord sc = this.proj.get2dCoord(c, this.wndsz);
        sc.x -= this.tex.sz().x / 2;
        sc.y += 10;
        g.image(this.tex, sc);
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

    public void update(OverlayData.OverlayGob overlayGob) {
        String name = overlayGob.name;
        String text = overlayGob.text;
        Color textColor = overlayGob.textColor;
        Color strokeColor = overlayGob.strokeColor;
        int fontSize = overlayGob.fontSize;
        String font = overlayGob.font;
        Text.Foundry textFoundry= new Text.Foundry(Text.cfg.font.get(font), fontSize);

        tex = Text.renderstroked(text, textColor, strokeColor, textFoundry).tex();
    }

    public Object staticp() {
        return CONSTANS;
    }
}
