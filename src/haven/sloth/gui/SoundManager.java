package haven.sloth.gui;

import haven.*;
import haven.Button;
import haven.Label;
import haven.Window;
import haven.purus.pbot.PBotUtils;
import haven.sloth.gob.Alerted;
import haven.sloth.util.ObservableMapListener;

import java.awt.*;
import java.util.*;
import java.util.List;

public class SoundManager extends Window implements ObservableMapListener<String, Resource.Named>{
    private Map<String, Resource.Named> map = new TreeMap<>();
    private List<String> keys = new ArrayList<>();
    private final Listbox<Resource.Named> sounds;
    private final Listbox<String> objs;
    private HSlider volslider;
    private Label vollabel;
	private final Listbox<String> defaults;
	public static Button select;
	private HashMap<String, String> defaultlist = new HashMap<>();
	private List<String> shortvers = new ArrayList<>();

    public SoundManager() {
        super(Coord.z, "Sound Manager", "Sound Manager");
	Coord c = new Coord(0, 0);
	c.x += add(objs = new Listbox<String>(200, 20, 20) {
	    @Override
	    protected String listitem(int i) {
		return keys.get(i);
	    }

	    @Override
	    protected int listitems() {
		return keys.size();
	    }

	    @Override
	    protected void drawitem(GOut g, String item, int i) {
		g.text(item, new Coord(5, 1));
	    }

	    @Override
	    public void change(String item) {
	        if(item != null) {
		    sounds.sel = map.get(item);
		    volslider.val = (int) Math.round(Alerted.volmap.get(item) * 1000);
		}
	        super.change(item);
	    }
	}, c.copy()).sz.x + 5;
	c.x += add(sounds = new Listbox<Resource.Named>(200, 20, 20) {
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
		g.text(item.name.replace("custom/sfx/omni/",""), new Coord(5, 1));
	    }
	}, c.copy()).sz.x + 5;

	c.y += add(defaults = new Listbox<String>(200, 20, 20) {
			@Override
			protected String listitem(int i) {
				return shortvers.get(i);
			}

			@Override
			protected int listitems() { return shortvers.size(); }

			@Override
			protected void drawitem(GOut g, String item, int i) { g.text(item, new Coord(5, 1)); }

			@Override
			public void change(String item){
				if(sel != null && sel.equals(item)){
					for(Resource.Named sound : Alerted.sounds) { //iterate the stored sound resources until we find the swag alarm
						if (sound.name.endsWith("Swag")) {
							defaultlist.forEach((k,v) -> { //iterate the default list so we can find the keyvalue for the selection
							if(v.equals(item)){
								Alerted.add(k, sound, .8);  //add the alarm
							}
						});
						}
					}
				}
				super.change(item);
			}
	}, c.copy()).sz.y + 5;
		{
			Coord bc = c.copy();
			bc.y += add(new Button(200, "Load Defaults", this::loadDefaults), bc.copy()).sz.y + 5;
			add(new Button(200, "Preview", this::preview), bc.copy());
		}
	{
	    c.x = 0;
	    add(new Button(200, "Remove Sound Alert", this::removeAlert), c.copy());
		select = add(new Button(200, "Select", this::select), new Coord(c.copy().x+205,c.copy().y));
	}
	{
		vollabel = add(new Label("Selected Alert Volume"),new Coord(c.copy().x+40,c.copy().y+20));
		volslider = add(new HSlider(200, 0, 1000, 0) {
			protected void attach(UI ui) {
				super.attach(ui);
				val = (int) (.8 * 1000);
			}

			public void changed() {
				double vol = val / 1000.0;
				if(objs.sel != null)
					Alerted.add(objs.sel, sounds.sel, vol);
			}
		},new Coord(c.copy().x,c.copy().y+40));
		add(new Label("Only Alarm once, disable to"),new Coord(c.copy().x+220,c.copy().y+25));
		add(new Label("alert if gob is seen again."),new Coord(c.copy().x+220,c.copy().y+35));
		add(new CheckBox("") {
			{
				a = Config.alarmonce;
			}

			public void set(boolean val) {
				Utils.setprefb("alarmonce", val);
				Config.alarmonce = val;
				a = val;
			}
		},new Coord(c.copy().x+375,c.copy().y+30));
	}
	pack();
	Alerted.listen(this);
    }

    private void removeAlert() {
	if (objs.sel != null) {
	    Alerted.remove(objs.sel);
	}
    }

    private void select() {
	if (objs.sel != null) {
	    if (sounds.sel != null) {
			Alerted.add(objs.sel, sounds.sel, volslider.val / 1000.0);
	    }
	}
    }

    private void preview() {
	if(sounds.sel != null) {
	    Audio.play(sounds.sel,volslider.val / 1000.0);
	}
    }

    @Override
    public void close() {
	hide();
    }

    @Override
    protected void removed() {Alerted.unlisten(this);}

    @Override
    public void init(Map<String, Resource.Named> base) {
	map.putAll(base);
	keys.addAll(map.keySet());
	keys.sort(String::compareTo);
	defaultlist = Config.defaultitems;
	shortvers.addAll(defaultlist.values());
	shortvers.sort(String::compareTo);
    }

    public void put(String key, Resource.Named val) {
        if(!map.containsKey(key)) {
	    map.put(key, val);
	    keys.add(key);
	    keys.sort(String::compareTo);
	} else {
            //only an update
            map.put(key, val);
	}
    }

    public void remove(String key) {
        map.remove(key);
        keys.remove(key);
    }

    public void loadDefaults(){
        PBotUtils.sysMsg("Default alarms loaded.",Color.white);
		for(Resource.Named sound : Alerted.sounds){
			if (sound.name.contains("BearRoar"))
				Alerted.add("gfx/kritter/bear/bear",sound,.8);
			else if(sound.name.contains("DangerNoodle"))
				Alerted.add("gfx/kritter/adder/adder",sound,.8);
			else if(sound.name.contains("lynx"))
				Alerted.add("gfx/kritter/lynx/lynx",sound,.8);
			else if(sound.name.contains("Walrus"))
				Alerted.add("gfx/kritter/walrus/walrus",sound,.8);
			else if(sound.name.contains("seal"))
				Alerted.add("gfx/kritter/seal/seal",sound,.8);
			else if(sound.name.contains("troll"))
				Alerted.add("gfx/kritter/troll/troll",sound,.8);
			else if(sound.name.contains("mammoth"))
				Alerted.add("gfx/kritter/mammoth/mammoth",sound,.8);
			else if(sound.name.contains("EagleScreech"))
				Alerted.add("gfx/kritter/goldeneagle/goldeneagle",sound,.8);
			else if(sound.name.contains("WreckingBall"))
				Alerted.add("gfx/terobjs/vehicle/wreckingball",sound,.8);
			else if(sound.name.contains("Doomed"))//bram
				Alerted.add("gfx/terobjs/vehicle/bram",sound,.8);
			else if(sound.name.contains("siege"))//catapult
				Alerted.add("gfx/terobjs/vehicle/catapult",sound,.8);
			else if(sound.name.contains("GhostBusters"))
				Alerted.add("gfx/kritter/nidbane/nidbane",sound,.8);
			else if(sound.name.contains("BeaverDungeon"))
				Alerted.add("gfx/terobjs/beaverdam",sound,.8);
			else if(sound.name.contains("Zelda"))//bat dungeon
				Alerted.add("gfx/terobjs/dng/batcave",sound,.8);
			else if(sound.name.contains("thankyourick"))//ant dungeon
				Alerted.add("gfx/terobjs/dng/antdungeon",sound,.8);
			else if(sound.name.contains("Swag")){
				Alerted.add("gfx/terobjs/saltbasin",sound,.8);
				Alerted.add("gfx/terobjs/abyssalchasm",sound,.8);
				Alerted.add("gfx/terobjs/windthrow",sound,.8);
				Alerted.add("gfx/terobjs/icespire",sound,.8);
				Alerted.add("gfx/terobjs/woodheart",sound,.8);
				Alerted.add("gfx/terobjs/lilypadlotus",sound,.8);
				Alerted.add("gfx/terobjs/fairystone",sound,.8);
				Alerted.add("gfx/terobjs/jotunmussel",sound,.8);
				Alerted.add("gfx/terobjs/guanopile",sound,.8);
				Alerted.add("gfx/terobjs/geyser",sound,.8);
				Alerted.add("gfx/terobjs/claypit",sound,.8);
				Alerted.add("gfx/terobjs/caveorgan",sound,.8);
				Alerted.add("gfx/terobjs/crystalpatch",sound,.8);
			}
		}
	}
}
