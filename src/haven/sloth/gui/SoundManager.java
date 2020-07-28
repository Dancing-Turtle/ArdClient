package haven.sloth.gui;

import haven.Audio;
import haven.Button;
import haven.CheckBox;
import haven.Config;
import haven.Coord;
import haven.GOut;
import haven.HSlider;
import haven.Label;
import haven.Listbox;
import haven.Resource;
import haven.UI;
import haven.Utils;
import haven.Window;
import haven.purus.pbot.PBotUtils;
import haven.sloth.gob.Alerted;
import haven.sloth.util.ObservableListener;
import modification.configuration;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class SoundManager extends Window implements ObservableListener<Alerted.ConnectSound> {
    private Map<String, String> map = new TreeMap<>();
    private List<String> keys = new ArrayList<>();
    private final Listbox<String> sounds;
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
                if (item != null) {
                    sounds.sel = map.get(item);
                    volslider.val = (int) Math.round(Alerted.getVolume(item) * 1000);
                }
                super.change(item);
            }
        }, c.copy()).sz.x + 5;
        c.x += add(sounds = new Listbox<String>(200, 20, 20) {
            @Override
            protected String listitem(int i) {
                return Alerted.custom.get(i);
            }

            @Override
            protected int listitems() {
                return Alerted.custom.size();
            }

            @Override
            protected void drawitem(GOut g, String item, int i) {
                String di = item;
                if (item.contains("custom/sfx/omni/")) di = item.replace("custom/sfx/omni/", "");
                if (item.contains(configuration.soundPath + "\\"))
                    di = item.replace(configuration.soundPath + "\\", "");
                g.text(di, new Coord(5, 1));
            }
        }, c.copy()).sz.x + 5;

        c.y += add(defaults = new Listbox<String>(200, 20, 20) {
            @Override
            protected String listitem(int i) {
                return shortvers.get(i);
            }

            @Override
            protected int listitems() {
                return shortvers.size();
            }

            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, new Coord(5, 1));
            }

            @Override
            public void change(String item) {
                if (sel != null && sel.equals(item)) {
                    for (String sound : Alerted.custom) { //iterate the stored sound resources until we find the swag alarm
                        if (sound.endsWith("Swag")) {
                            defaultlist.forEach((k, v) -> { //iterate the default list so we can find the keyvalue for the selection
                                if (v.equals(item)) {
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
            select = add(new Button(200, "Select", this::select), new Coord(c.copy().x + 205, c.copy().y));
        }
        {
            vollabel = add(new Label("Selected Alert Volume"), new Coord(c.copy().x + 40, c.copy().y + 20));
            volslider = add(new HSlider(200, 0, 1000, 0) {
                protected void attach(UI ui) {
                    super.attach(ui);
                    val = (int) (.8 * 1000);
                }

                public void changed() {
                    double vol = val / 1000.0;
                    if (objs.sel != null)
                        Alerted.add(objs.sel, sounds.sel, vol);
                }
            }, new Coord(c.copy().x, c.copy().y + 40));
            add(new Label("Only Alarm once, disable to"), new Coord(c.copy().x + 220, c.copy().y + 25));
            add(new Label("alert if gob is seen again."), new Coord(c.copy().x + 220, c.copy().y + 35));
            add(new CheckBox("") {
                {
                    a = Config.alarmonce;
                }

                public void set(boolean val) {
                    Utils.setprefb("alarmonce", val);
                    Config.alarmonce = val;
                    a = val;
                }
            }, new Coord(c.copy().x + 375, c.copy().y + 30));
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
        if (sounds.sel != null) {
            if (Alerted.customsort.get(sounds.sel))
                Audio.play(sounds.sel, volslider.val / 1000.0);
            else
                Audio.play(Resource.remote().load(sounds.sel), volslider.val / 1000.0);
        }
    }

    @Override
    public void close() {
        hide();
    }

    @Override
    protected void removed() {
        Alerted.unlisten(this);
    }

    @Override
    public void init(Collection<Alerted.ConnectSound> base) {
        for (Alerted.ConnectSound sound : base) {
            map.put(sound.objName, sound.soundName);
        }
        keys.addAll(map.keySet());
        keys.sort(String::compareTo);
        defaultlist = Config.defaultitems;
        shortvers.addAll(defaultlist.values());
        shortvers.sort(String::compareTo);
    }

    @Override
    public void added(Alerted.ConnectSound item) {
        if (!map.containsKey(item.objName)) {
            map.put(item.objName, item.soundName);
            keys.add(item.objName);
            keys.sort(String::compareTo);
        } else {
            //only an update
            map.put(item.objName, item.soundName);
        }
    }

    public void remove(Alerted.ConnectSound item) {
        map.remove(item.objName);
        keys.remove(item.objName);
    }

    public void put(String key, String val) {
        if (!map.containsKey(key)) {
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

    public void loadDefaults() {
        PBotUtils.sysMsg("Default alarms loaded.", Color.white);
        for (String sound : Alerted.custom) {
            if (sound.contains("BearRoar"))
                Alerted.add("gfx/kritter/bear/bear", sound, .8);
            else if (sound.contains("DangerNoodle"))
                Alerted.add("gfx/kritter/adder/adder", sound, .8);
            else if (sound.contains("lynx"))
                Alerted.add("gfx/kritter/lynx/lynx", sound, .8);
            else if (sound.contains("Walrus"))
                Alerted.add("gfx/kritter/walrus/walrus", sound, .8);
            else if (sound.contains("seal"))
                Alerted.add("gfx/kritter/seal/seal", sound, .8);
            else if (sound.contains("troll"))
                Alerted.add("gfx/kritter/troll/troll", sound, .8);
            else if (sound.contains("mammoth"))
                Alerted.add("gfx/kritter/mammoth/mammoth", sound, .8);
            else if (sound.contains("EagleScreech"))
                Alerted.add("gfx/kritter/goldeneagle/goldeneagle", sound, .8);
            else if (sound.contains("WreckingBall"))
                Alerted.add("gfx/terobjs/vehicle/wreckingball", sound, .8);
            else if (sound.contains("Doomed"))//bram
                Alerted.add("gfx/terobjs/vehicle/bram", sound, .8);
            else if (sound.contains("siege"))//catapult
                Alerted.add("gfx/terobjs/vehicle/catapult", sound, .8);
            else if (sound.contains("GhostBusters"))
                Alerted.add("gfx/kritter/nidbane/nidbane", sound, .8);
            else if (sound.contains("BeaverDungeon"))
                Alerted.add("gfx/terobjs/beaverdam", sound, .8);
            else if (sound.contains("Zelda"))//bat dungeon
                Alerted.add("gfx/terobjs/dng/batcave", sound, .8);
            else if (sound.contains("thankyourick"))//ant dungeon
                Alerted.add("gfx/terobjs/dng/antdungeon", sound, .8);
            else if (sound.contains("Swag")) {
                Alerted.add("gfx/terobjs/saltbasin", sound, .8);
                Alerted.add("gfx/terobjs/abyssalchasm", sound, .8);
                Alerted.add("gfx/terobjs/windthrow", sound, .8);
                Alerted.add("gfx/terobjs/icespire", sound, .8);
                Alerted.add("gfx/terobjs/woodheart", sound, .8);
                Alerted.add("gfx/terobjs/lilypadlotus", sound, .8);
                Alerted.add("gfx/terobjs/fairystone", sound, .8);
                Alerted.add("gfx/terobjs/jotunmussel", sound, .8);
                Alerted.add("gfx/terobjs/guanopile", sound, .8);
                Alerted.add("gfx/terobjs/geyser", sound, .8);
                Alerted.add("gfx/terobjs/claypit", sound, .8);
                Alerted.add("gfx/terobjs/caveorgan", sound, .8);
                Alerted.add("gfx/terobjs/crystalpatch", sound, .8);
            }
        }
    }
}
