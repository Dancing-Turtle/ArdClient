import haven.*;

import javax.media.opengl.GL;
import javax.media.opengl.GL2;

import java.util.List;

import static haven.OCache.posres;

public class Pointer extends Widget {
    public static final States.ColState col = new States.ColState(241, 227, 157, 255);
    public Indir<Resource> icon;
    private Tex licon;
    public Coord2d tc;
    public Coord lc;
    public long gobid = -1L;

    public Pointer(Indir<Resource> res) {
	super(Coord.z);
	this.icon = res;
    }

    public static Widget mkwidget(UI ui, Object... args) {
	int i = ((Integer)args[0]);
	return new Pointer(i < 0 ? null : ui.sess.getres(i));
    }

    public void presize()
    {
	resize(this.parent.sz);
    }

    protected void added() {
	presize();
	super.added();
    }

    private int signum(int paramInt) {
	if (paramInt < 0) {
	    return -1;
	}
	if (paramInt > 0) {
	    return 1;
	}
	return 0;
    }

    private void drawarrow(GOut g, Coord sc) {
	Coord hsz = this.sz.div(2);
	sc = sc.sub(hsz);
	if (sc.equals(Coord.z)) {
	    sc = new Coord(1, 1);
	}
	double d = Coord.z.dist(sc);
	Coord localCoord2 = sc.mul((d - 25.0D) / d);
	float f = hsz.y / (float)hsz.x;
	if ((Math.abs(localCoord2.x) > hsz.x) || (Math.abs(localCoord2.y) > hsz.y)) {
	    if (Math.abs(localCoord2.x) * f < Math.abs(localCoord2.y)) {
		localCoord2 = new Coord(localCoord2.x * hsz.y / localCoord2.y, hsz.y).mul(signum(localCoord2.y));
	    } else {
		localCoord2 = new Coord(hsz.x, localCoord2.y * hsz.x / localCoord2.x).mul(signum(localCoord2.x));
	    }
	}
	Coord localCoord3 = localCoord2.sub(sc).norm(30.0D);
	localCoord2 = localCoord2.add(hsz);

	BGL localBGL = g.gl;
	g.state2d();
	g.state(col);
	g.apply();
	localBGL.glEnable(2881);
	localBGL.glBegin(4);
	g.vertex(localCoord2); //The point
	g.vertex(localCoord2.add(localCoord3).add(-localCoord3.y / 3, localCoord3.x / 3)); //one side
	g.vertex(localCoord2.add(localCoord3).add(localCoord3.y / 3, -localCoord3.x / 3)); //The other
	localBGL.glEnd();
	localBGL.glDisable(2881);
	if (this.icon != null) {
	    try
	    {
		if (this.licon == null) {
		    this.licon = ((this.icon.get()).layer(Resource.imgc)).tex();
		}
		g.aimage(this.licon, localCoord2.add(localCoord3), 0.5D, 0.5D);
	    }
	    catch (Loading localLoading) {}
	}
	this.lc = localCoord2.add(localCoord3);
    }

    private void drawarrow(GOut g, double a) {
	Coord hsz = sz.div(2);
	double ca = -Coord.z.angle(hsz);
	Coord ac;
	if((a > ca) && (a < -ca)) {
	    ac = new Coord(sz.x, hsz.y - (int)(Math.tan(a) * hsz.x));
	} else if((a > -ca) && (a < Math.PI + ca)) {
	    ac = new Coord(hsz.x - (int)(Math.tan(a - Math.PI / 2) * hsz.y), 0);
	} else if((a > -Math.PI - ca) && (a < ca)) {
	    ac = new Coord(hsz.x + (int)(Math.tan(a + Math.PI / 2) * hsz.y), sz.y);
	} else {
	    ac = new Coord(0, hsz.y + (int)(Math.tan(a) * hsz.x));
	}
	Coord bc = ac.add(Coord.sc(a, 0));

	g.state2d();
	g.state(col);
	g.apply();
	g.gl.glEnable(GL2.GL_POLYGON_SMOOTH);
	g.gl.glBegin(GL.GL_TRIANGLES);
	g.vertex(bc);
	g.vertex(bc.add(Coord.sc(a + Math.PI / 12, -35)));
	g.vertex(bc.add(Coord.sc(a - Math.PI / 12, -35)));
	g.gl.glEnd();
	g.gl.glDisable(GL2.GL_POLYGON_SMOOTH);

	if (this.icon != null) {
	    try {
		if (this.licon == null) {
		    this.licon = ((this.icon.get()).layer(Resource.imgc)).tex();
		}
		g.aimage(this.licon, bc.add(Coord.sc(a, -30)), 0.5, 0.5);
	    }
	    catch (Loading localLoading) {
	        //Ignore it
	    }
	}
	this.lc = bc.add(Coord.sc(a, -30));
    }

    public void draw(GOut g) {
	this.lc = null;
	if (this.tc == null) {
	    return;
	}

	Gob questgob = this.gobid < 0L ? null : this.ui.sess.glob.oc.getgob(this.gobid);
	final Coord3f gobsc;
	final Coord2d gobrc;
	if (questgob != null) {
	    try {
	    	Coord3f cc = questgob.getc();
			if(Config.disableelev)
				cc.z = 0;
		gobsc = (getparent(GameUI.class)).map.screenxf(cc);
		gobrc = questgob.rc;
	    } catch (Loading localLoading) {
		return;
	    }
	} else {
	    gobsc = ui.gui.map.screenxf(this.tc);
	    gobrc = tc;
	}

	if (gobsc != null) {
	    final Double angle = ui.gui.map.screenangle(gobrc, true);
	    if(!angle.equals(Double.NaN)) {
		drawarrow(g, ui.gui.map.screenangle(gobrc, true));
	    } else {
		drawarrow(g, new Coord(gobsc));
	    }
	}
    }

    public void udpate(Coord2d paramCoord2d, long paramLong) {
	this.tc = paramCoord2d;
	this.gobid = paramLong;
    }

    public void uimsg(String msg, Object... args) {
        switch (msg) {
	    case "upd":
		if (args[0] == null) {
		    this.tc = null;
		} else {
		    this.tc = ((Coord)args[0]).mul(posres);
		}
		if (args[1] == null) {
		    this.gobid = -1L;
		} else {
		    this.gobid = ((Integer)args[1]);
		}
	        break;
	    case "icon":
		int i = ((Integer)args[0]);
		this.icon = i < 0 ? null : this.ui.sess.getres(i);
		this.licon = null;
	        break;
	    default:
		super.uimsg(msg, args);
	}
    }

    @Override
    public boolean mousedown(Coord c, int button) {
  		if(this.lc != null && this.lc.dist(c) < 20.0) {
            	if(gobid > 0) {
						ui.gui.map.wdgmsg("click", rootpos().add(c), this.tc.floor(posres), button, ui.modflags(), 0, (int) gobid, this.tc.floor(posres), 0, -1);
				if(button == 3) {//only add to marker map if map open
					try {
					if(!ui.gui.mapfile.visible)
						ui.gui.toggleMapfile();
					ui.gui.mapfile.selectMarker(((Text.Line)this.tooltip).text);
					ui.gui.map.questQueueAdd(this.tc);
				}catch(Exception e){e.printStackTrace();}
				}
	    } else {
                ui.gui.map.wdgmsg("click", rootpos().add(c), this.tc.floor(posres), button, ui.modflags());
				if(button == 3) {//only add to marker map if map open
					try {
						if (!ui.gui.mapfile.visible)
							ui.gui.toggleMapfile();
						ui.gui.mapfile.selectMarker(((Text.Line) this.tooltip).text);
						ui.gui.map.questQueueAdd(this.tc);
					}catch(Exception e){e.printStackTrace();}
				}
	    }
            return true;
	} else {
            return super.mousedown(c, button);
	}
    }

    private Text.Line tt = null;
    private int dist;
    public Object tooltip(Coord c, Widget wdg) {
		if ((this.lc != null) && (this.lc.dist(c) < 20.0D) && tc != null) {
			if(tooltip instanceof Text.Line) {
				final Gob me = ui.sess.glob.oc.getgob(ui.gui.map.plgob);
				if(me != null) {
					final int cdist = (int) (Math.ceil(me.rc.dist(tc) / 11.0));
					if (cdist != dist) {
						dist = cdist;
						final String extra;
						if (dist >= 1000) {
							extra = " - May be further than the client can see";
						} else {
							extra = "";
						}
						if (tt != null && tt.tex() != null)
							tt.tex().dispose();
						return tt = Text.render(((Text.Line) this.tooltip).text + " - Distance: " + dist + extra);
					} else {
						return tt;
					}
				}
			} else {
				return this.tooltip;
			}
		}
		return null;
	}
}
