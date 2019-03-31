package haven.sloth.gui;

import haven.*;
import haven.sloth.gob.Alerted;

/**
 *
 *  +------------------------+
 *  | Select sound for {res} |
 *  | +--------+             |
 *  | | list   |             |
 *  | | of     |   Select    |
 *  | | sounds |   Preview   |
 *  | |        |             |
 *  | +--------+             |
 *  +------------------------+
 *
 */
public class SoundSelector extends Window {
    private final String gobname;
    private final Listbox<Resource.Named> sounds;
    private HSlider volslider;

    public SoundSelector(final String gobname) {
        super(Coord.z, "Sound Selector");
        this.gobname = gobname;

	Coord c = new Coord(0, 0);
	c.y += add(new Label("Select sound for " + gobname)).sz.y;
	final Button select = new Button(50, "Select", this::select);
	final Button preview = new Button(50, "Preview", this::preview);
	final Label volume = new Label("Volume");
	volslider = new HSlider(200, 0, 1000, 0) {
			protected void attach(UI ui) {
				super.attach(ui);
				val = (int) (.8 * 1000);
			}
		};
	sounds = add(new Listbox<Resource.Named>(200, 20, 20) {
	    @Override
	    protected Resource.Named listitem(int i) {
		return Alerted.sounds.get(i);
	    }

	    @Override
	    protected int listitems() {
		return Alerted.sounds.size();
	    }

	    @Override
	    protected void drawitem(GOut g, Resource.Named item, int i) {
		g.text(item.name, new Coord(5, 1));
	    }
	}, c.copy());
	add(select,  c.add(sounds.sz.x +5, sounds.sz.y/2 - select.sz.y));
	add(preview, c.add(sounds.sz.x +5, sounds.sz.y/2 + select.sz.y));
	add(volume,c.add(sounds.sz.x/2 -20, sounds.sz.y + 5));
	add(volslider,c.add(0, sounds.sz.y + 20));
	pack();
    }

    @Override
    public void close() {
	ui.destroy(this);
    }

    private void select() {
        if(sounds.sel != null) {
            Alerted.add(gobname, sounds.sel, volslider.val / 1000.0);
            ui.destroy(this);
	}
    }

    private void preview() {
        if(sounds.sel != null) {
        //    Audio.play(Resource.remote().load(sounds.sel.name));
			Audio.play(Resource.remote().load(sounds.sel.name),volslider.val / 1000.0);
	}
    }
}
