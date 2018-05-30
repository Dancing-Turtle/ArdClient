import haven.Button;
import haven.Coord;
import haven.DTarget;
import haven.GOut;
import haven.Indir;
import haven.Inventory;
import haven.Label;
import haven.Loading;
import haven.Resource;
import haven.Tex;
import haven.UI;
import haven.Widget;

public class Grainslot extends Widget implements DTarget {
    public final Label lbl;
    public Indir<Resource> icon;
    public Button tbtn;
    public Button pbtn;
    public Button ebtn;
    private Tex iconc;

    public Grainslot() {
        super(new Coord(Resource.language.equals("ru") ? 430 : 390, 40));
        this.lbl = this.adda(new Label(""), 40, this.sz.y / 2, 0.0D, 0.5D);
    }

    public static Widget mkwidget(UI ui, Object... var1) {
        return new Grainslot();
    }

    public void draw(GOut var1) {
        int var2 = (this.sz.y - Inventory.invsq.sz().y) / 2;
        var1.image(Inventory.invsq, new Coord(var2, var2));
        if (this.icon != null) {
            try {
                if (this.iconc == null) {
                    this.iconc = this.icon.get().layer(Resource.imgc).tex();
                }

                var1.image(this.iconc, new Coord(var2 + 1, var2 + 1));
            } catch (Loading var4) {
            }
        }

        super.draw(var1);
    }

    public boolean mousedown(Coord var1, int var2) {
        int var3 = (this.sz.y - Inventory.invsq.sz().y) / 2;
        if (var1.isect(new Coord(var3, var3), Inventory.invsq.sz())) {
            this.wdgmsg("click", new Object[]{Integer.valueOf(var2), Integer.valueOf(this.ui.modflags())});
            return true;
        } else {
            return super.mousedown(var1, var2);
        }
    }

    public boolean drop(Coord var1, Coord var2) {
        this.wdgmsg("drop", new Object[]{Integer.valueOf(this.ui.modflags())});
        return true;
    }

    public boolean iteminteract(Coord var1, Coord var2) {
        this.wdgmsg("itemiact", new Object[]{Integer.valueOf(this.ui.modflags())});
        return true;
    }

    public void uimsg(String var1, Object... var2) {
        if (var1 == "upd") {
            this.lbl.settext(Resource.getLocContent((String) var2[0]));
            this.icon = var2[1] == null ? null : this.ui.sess.getres(((Integer) var2[1]).intValue());
            this.iconc = null;
            int var3 = ((Integer) var2[2]).intValue();

            GrainslotButtom emptybtn = new GrainslotButtom(60, "Empty");
            GrainslotButtom takebtn = new GrainslotButtom(60, "Take");

            if ((var3 & 4) != 0 && this.ebtn == null) {
                this.ebtn = this.adda(emptybtn, this.sz.x - emptybtn.sz.x, this.sz.y / 2, 0.0D, 0.5D);
            } else if ((var3 & 4) == 0 && this.ebtn != null) {
                this.ebtn.destroy();
                this.ebtn = null;
            }

            if ((var3 & 1) != 0 && this.tbtn == null) {

                this.tbtn = this.adda(takebtn, this.sz.x - emptybtn.sz.x - 5 - takebtn.sz.x, this.sz.y / 2, 0.0D, 0.5D);
            } else if ((var3 & 1) == 0 && this.tbtn != null) {
                this.tbtn.destroy();
                this.tbtn = null;
            }

            if ((var3 & 2) != 0 && this.pbtn == null) {
                GrainslotButtom putbtn = new GrainslotButtom(60, "Put");
                this.pbtn = this.adda(putbtn, this.sz.x - emptybtn.sz.x - 5 - takebtn.sz.x - 5 - putbtn.sz.x, this.sz.y / 2, 0.0D, 0.5D);
            } else if ((var3 & 2) == 0 && this.pbtn != null) {
                this.pbtn.destroy();
                this.pbtn = null;
            }
        } else {
            super.uimsg(var1, var2);
        }
    }

    class GrainslotButtom extends Button {
        private final String name;
        GrainslotButtom(int var2, String var3) {
            super(var2, var3);
            name = var3.toLowerCase();
        }

        public void click() {
            parent.wdgmsg(name, new Object[0]);
        }
    }
}
