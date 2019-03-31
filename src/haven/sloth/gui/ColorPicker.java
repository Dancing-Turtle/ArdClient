package haven.sloth.gui;

import haven.*;
import haven.Label;
import haven.Window;

import java.awt.*;
import java.util.function.Consumer;

/**
 * TODO: redo to make nicer...
 */
public class ColorPicker extends Window {
    private Color col;
    private final HSlider rsb,gsb,bsb,asb;
    private final TextEntry rte,gte,bte,ate;
    private final ColorPreview prev;
    private final Consumer<Color> callback;

    ColorPicker(final Color def, final Consumer<Color> callback){
	super(Coord.z, "Color Picker", "Color Picker");
	this.callback = callback;
	this.col = def;
	int w = 0;
	w = Math.max(w,add(new Label("Red:"), new Coord(0,0)).sz.x);
	w = Math.max(w,add(new Label("Green:"), new Coord(0,20)).sz.x);
	w = Math.max(w,add(new Label("Blue:"), new Coord(0,40)).sz.x);
	w = Math.max(w,add(new Label("Alpha:"), new Coord(0,60)).sz.x);
	w = Math.max(w,add(new Label("Preview:"), new Coord(0,80)).sz.x);
	rsb = new HSlider(255, 0,255, col.getRed()){
		public void changed(){
		    updateColor(val, col.getGreen(), col.getBlue(), col.getAlpha());
		    rte.settext(String.format("%d",val));
		}
	    };
	add(rsb, new Coord(w,0));
	gsb = new HSlider(255,0,255, col.getGreen()){
		public void changed(){
		    updateColor(col.getRed(), val, col.getBlue(), col.getAlpha());
		    gte.settext(String.format("%d",val));
		}
	    };
	add(gsb, new Coord(w,20));
	bsb = new HSlider(255,0,255, col.getBlue()){
		public void changed(){
		    updateColor(col.getRed(), col.getGreen(), val, col.getAlpha());
		    ColorPicker.this.prev.setColor(col);
		    bte.settext(String.format("%d",val));
		}
	    };
	add(bsb, new Coord(w,40));
	asb = new HSlider(255,0,255, col.getAlpha()){
		public void changed(){
		    updateColor(col.getRed(), col.getGreen(), col.getBlue(), val);
		    ColorPicker.this.prev.setColor(col);
		    ate.settext(String.format("%d",val));
		}
	    };
	add(asb, new Coord(w,60));
	w = w + rsb.sz.x + 5;
	rte = add(new TextEntry(50, String.format("%d",col.getRed())),
		  new Coord(w,0));
	rte.canactivate = true;
	gte = add(new TextEntry(50,String.format("%d",col.getGreen())),
		  new Coord(w,20));
	gte.canactivate = true;
	bte = add(new TextEntry(50,String.format("%d",col.getBlue())),
		  new Coord(w, 40));
	bte.canactivate = true;
	ate = add(new TextEntry(50,String.format("%d",col.getAlpha())),
		  new Coord(w, 60));
	ate.canactivate = true;
	prev = add(new ColorPreview(new Coord(w+50, 30), col), new Coord(0, 100));
	pack();
    }

    public void close() {
        callback.accept(col);
	ui.destroy(this);
    }

    private void updateColor(final int r, final int g, final int b, final int a) {
        col = new Color(r, g, b, a);
        prev.setColor(col);
    }

    public void wdgmsg(Widget sender, String msg, Object... args){
	if(sender == rte){
	    if(msg.equals("activate")) {
		int val;
		val = Integer.parseInt((String) args[0]);
		if (val < 0) {
		    val = 0;
		} else if (val > 255) {
		    val = 255;
		}
		rsb.val = val;
		updateColor(val, col.getGreen(), col.getBlue(), col.getAlpha());
	    }
	} else if(sender == gte){
	    if(msg.equals("activate")) {
		int val;
		if (args[0] != null) {
			val = Integer.parseInt((String) args[0]);
		    if (val < 0) {
			val = 0;
		    } else if (val > 255) {
			val = 255;
		    }
		    gsb.val = val;
		    updateColor(col.getRed(), val, col.getBlue(), col.getAlpha());
		}
	    }
	} else if(sender == bte){
	    if(msg.equals("activate")) {
		int val;
		if (args[0] != null) {
		    val = Integer.parseInt((String) args[0]);
		    if (val < 0) {
			val = 0;
		    } else if (val > 255) {
			val = 255;
		    }
		    bsb.val = val;
		    updateColor(col.getRed(), col.getGreen(), val, col.getAlpha());
		}
	    }
	} else if(sender == ate){
	    if(msg.equals("activate")) {
		int val;
		if (args[0] != null) {
		    val = Integer.parseInt((String) args[0]);
		    if (val < 0) {
			val = 0;
		    } else if (val > 255) {
			val = 255;
		    }
		    asb.val = val;
		    updateColor(col.getRed(), col.getGreen(), col.getBlue(), val);
		}
	    }
	} else super.wdgmsg(sender,msg,args);
    }
}
