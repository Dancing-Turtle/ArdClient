/*
 *  This file is part of the Haven & Hearth game client.
 *  Copyright (C) 2009 Fredrik Tolf <fredrik@dolda2000.com>, and
 *                     Björn Johannessen <johannessen.bjorn@gmail.com>
 *
 *  Redistribution and/or modification of this file is subject to the
 *  terms of the GNU Lesser General Public License, version 3, as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  Other parts of this source tree adhere to other copying
 *  rights. Please see the file `COPYING' in the root directory of the
 *  source tree for details.
 *
 *  A copy the GNU Lesser General Public License is distributed along
 *  with the source tree of which this file is a part in the file
 *  `doc/LPGL-3'. If it is missing for any reason, please see the Free
 *  Software Foundation's website at <http://www.fsf.org/>, or write
 *  to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
 */

package haven;


import haven.automation.Discord;
import haven.purus.BotUtils;
import haven.resutil.BPRadSprite;

import java.awt.Color;
import java.awt.GraphicsEnvironment;
import java.awt.event.KeyEvent;
import java.io.*;
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.cert.CollectionCertStoreParameters;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.function.Consumer;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.prefs.BackingStoreException;
import java.util.stream.Collectors;

import static haven.DefSettings.*;

public class OptWnd extends Window {
    public static final int VERTICAL_MARGIN = 10;
    public static final int HORIZONTAL_MARGIN = 5;
    private static final Text.Foundry fonttest = new Text.Foundry(Text.sans, 10).aa(true);
    public static final int VERTICAL_AUDIO_MARGIN = 5;
    public final Panel main, video, audio, display, map, general, combat, control, uis,uip, quality, flowermenus, soundalarms, hidesettings, studydesksettings, keybindsettings, chatsettings, clearboulders, clearbushes, cleartrees, clearhides;
    public Panel current;

    public void chpanel(Panel p) {
        if (current != null)
            current.hide();
        (current = p).show();
    }

    public class PButton extends Button {
        public final Panel tgt;
        public final int key;

        public PButton(int w, String title, int key, Panel tgt) {
            super(w, title);
            this.tgt = tgt;
            this.key = key;
        }

        public void click() {
            if(tgt == clearboulders){
                final String charname = gameui().chrid;
                for (CheckListboxItem itm : Config.boulders.values())
                    itm.selected = false;
                Utils.setprefchklst("boulderssel_" + charname, Config.boulders);
            }else if(tgt == clearbushes){
                final String charname = gameui().chrid;
                for (CheckListboxItem itm : Config.bushes.values())
                    itm.selected = false;
                Utils.setprefchklst("bushessel_" + charname, Config.bushes);
            }else if(tgt == cleartrees){
                final String charname = gameui().chrid;
                for (CheckListboxItem itm : Config.trees.values())
                    itm.selected = false;
                Utils.setprefchklst("treessel_" + charname, Config.trees);
            }else if(tgt == clearhides){
                final String charname = gameui().chrid;
                for (CheckListboxItem itm : Config.icons.values())
                    itm.selected = false;
                Utils.setprefchklst("iconssel_" + charname, Config.icons);
            }else
            chpanel(tgt);
        }

        public boolean type(char key, java.awt.event.KeyEvent ev) {
            if ((this.key != -1) && (key == this.key)) {
                click();
                return (true);
            }
            return (false);
        }
    }

    public class Panel extends Widget {
        public Panel() {
            visible = false;
            c = Coord.z;
        }
    }

    public class VideoPanel extends Panel {
        public VideoPanel(Panel back) {
            super();
            add(new PButton(200, "Back", 27, back), new Coord(210, 360));
            resize(new Coord(620, 400));
        }

        public class CPanel extends Widget {
            public final GLSettings cf;

            public CPanel(GLSettings gcf) {
                this.cf = gcf;
                final WidgetVerticalAppender appender = new WidgetVerticalAppender(withScrollport(this, new Coord(620, 350)));
                appender.setVerticalMargin(VERTICAL_MARGIN);
                appender.setHorizontalMargin(HORIZONTAL_MARGIN);
                appender.add(new CheckBox("Per-fragment lighting") {
                    {
                        a = cf.flight.val;
                    }

                    public void set(boolean val) {
                        if (val) {
                            try {
                                cf.flight.set(true);
                            } catch (GLSettings.SettingException e) {
                                GameUI gui = getparent(GameUI.class);
                                if (gui != null)
                                    gui.error(e.getMessage());
                                return;
                            }
                        } else {
                            cf.flight.set(false);
                        }
                        a = val;
                        cf.dirty = true;
                    }
                });
                appender.add(new CheckBox("Render shadows") {
                    {
                        a = cf.lshadow.val;
                    }

                    public void set(boolean val) {
                        if (val) {
                            try {
                                cf.lshadow.set(true);
                            } catch (GLSettings.SettingException e) {
                                GameUI gui = getparent(GameUI.class);
                                if (gui != null)
                                    gui.error(e.getMessage());
                                return;
                            }
                        } else {
                            cf.lshadow.set(false);
                        }
                        a = val;
                        cf.dirty = true;
                    }
                });
                appender.add(new CheckBox("Antialiasing") {
                    {
                        a = cf.fsaa.val;
                    }

                    public void set(boolean val) {
                        try {
                            cf.fsaa.set(val);
                        } catch (GLSettings.SettingException e) {
                            GameUI gui = getparent(GameUI.class);
                            if (gui != null)
                                gui.error(e.getMessage());
                            return;
                        }
                        a = val;
                        cf.dirty = true;
                    }
                });
                appender.add(new Label("Anisotropic filtering"));
                if (cf.anisotex.max() <= 1) {
                    appender.add(new Label("(Not supported)"));
                } else {
                    final Label dpy = new Label("");
                    appender.addRow(
                            new HSlider(160, (int) (cf.anisotex.min() * 2), (int) (cf.anisotex.max() * 2), (int) (cf.anisotex.val * 2)) {
                                protected void added() {
                                    dpy();
                                }

                                void dpy() {
                                    if (val < 2)
                                        dpy.settext("Off");
                                    else
                                        dpy.settext(String.format("%.1f\u00d7", (val / 2.0)));
                                }

                                public void changed() {
                                    try {
                                        cf.anisotex.set(val / 2.0f);
                                    } catch (GLSettings.SettingException e) {
                                        getparent(GameUI.class).error(e.getMessage());
                                        return;
                                    }
                                    dpy();
                                    cf.dirty = true;
                                }
                            },
                            dpy);
                }
                appender.add(new CheckBox("Lower terrain draw distance - Will increase performance, but look like shit. (requires logout)") {
                    {
                        a = Config.lowerterraindistance;
                    }
                    public void set(boolean val) {
                        Config.lowerterraindistance = val;
                        Utils.setprefb("lowerterraindistance", val);
                        a = val;
                    }
                });
                appender.add(new CheckBox("Disable biome tile transitions (requires logout)") {
                    {
                        a = Config.disabletiletrans;
                    }
                    public void set(boolean val) {
                        Config.disabletiletrans = val;
                        Utils.setprefb("disabletiletrans", val);
                        a = val;
                    }
                });
                appender.add(new CheckBox("Disable terrain smoothing (requires logout)") {
                    {
                        a = Config.disableterrainsmooth;
                    }
                    public void set(boolean val) {
                        Config.disableterrainsmooth = val;
                        Utils.setprefb("disableterrainsmooth", val);
                        a = val;
                    }
                });
                appender.add(new CheckBox("Disable terrain elevation (requires logout)") {
                    {
                        a = Config.disableelev;
                    }
                    public void set(boolean val) {
                        Config.disableelev = val;
                        Utils.setprefb("disableelev", val);
                        a = val;
                    }
                });
                appender.add(new CheckBox("Disable flavor objects including ambient sounds") {
                    {
                        a = Config.hideflocomplete;
                    }

                    public void set(boolean val) {
                        Utils.setprefb("hideflocomplete", val);
                        Config.hideflocomplete = val;
                        a = val;
                    }
                });
                appender.add(new CheckBox("Hide flavor objects but keep sounds (requires logout)") {
                    {
                        a = Config.hideflovisual;
                    }

                    public void set(boolean val) {
                        Utils.setprefb("hideflovisual", val);
                        Config.hideflovisual = val;
                        a = val;
                    }
                });
                appender.add(new CheckBox("Show weather - This will also enable/disable Weed/Opium effects") {
                    {
                        a = Config.showweather;
                    }

                    public void set(boolean val) {
                        Utils.setprefb("showweather", val);
                        Config.showweather = val;
                        a = val;
                    }
                });
                appender.add(new CheckBox("Simple crops (req. logout)") {
                    {
                        a = Config.simplecrops;
                    }

                    public void set(boolean val) {
                        Utils.setprefb("simplecrops", val);
                        Config.simplecrops = val;
                        a = val;
                    }
                });
                appender.add(new CheckBox("Disable ALL animations") {
                    {
                        a = Config.disableAllAnimations;
                    }

                    public void set(boolean val) {
                        Utils.setprefb("disableAllAnimations", val);
                        Config.disableAllAnimations = val;
                        a = val;
                    }
                });
                appender.add(new CheckBox("Simple foragables (req. logout)") {
                    {
                        a = Config.simpleforage;
                    }

                    public void set(boolean val) {
                        Utils.setprefb("simpleforage", val);
                        Config.simpleforage = val;
                        a = val;
                    }
                });
                /*appender.add(new CheckBox("Hide crops") {
                    {
                        a = Config.hidecrops;
                    }

                    public void set(boolean val) {
                        Utils.setprefb("hidecrops", val);
                        Config.hidecrops = val;
                        a = val;
                    }
                });*/
                appender.add(new CheckBox("Show FPS") {
                    {
                        a = Config.showfps;
                    }

                    public void set(boolean val) {
                        Utils.setprefb("showfps", val);
                        Config.showfps = val;
                        a = val;
                    }
                });
                appender.add(new CheckBox("Disable black load screens. - Can cause issues loading the map, setting not for everyone.") {
                    {
                        a = Config.noloadscreen;
                    }

                    public void set(boolean val) {
                        Utils.setprefb("noloadscreen", val);
                        Config.noloadscreen = val;
                        a = val;
                    }
                });

                appender.add(new Label("Disable animations (req. restart):"));
                CheckListbox disanimlist = new CheckListbox(320, Math.min(8, Config.disableanim.values().size()), 18 + Config.fontadd) {
                    @Override
                    protected void itemclick(CheckListboxItem itm, int button) {
                        super.itemclick(itm, button);
                        Utils.setprefchklst("disableanim", Config.disableanim);
                    }
                };
                for (CheckListboxItem itm : Config.disableanim.values())
                    disanimlist.items.add(itm);
                appender.add(disanimlist);

                pack();
            }
        }

        private CPanel curcf = null;

        public void draw(GOut g) {
            if ((curcf == null) || (g.gc.pref != curcf.cf)) {
                if (curcf != null)
                    curcf.destroy();
                curcf = add(new CPanel(g.gc.pref), Coord.z);
            }
            super.draw(g);
        }
    }
    private Widget ColorPreWithLabel(final String text, final IndirSetting<Color> cl) {
        final Widget container = new Widget();
        final Label lbl = new Label(text);
        final IndirColorPreview pre = new IndirColorPreview(new Coord(16, 16), cl);
        final int height = Math.max(lbl.sz.y, pre.sz.y) / 2;
        container.add(lbl, new Coord(0, height - lbl.sz.y/2));
        container.add(pre, new Coord(lbl.sz.x, height - pre.sz.y/2));
        container.pack();
        return container;
    }

    public OptWnd(boolean gopts) {
        super(new Coord(620, 400), "Options", true);

        main = add(new Panel());
        video = add(new VideoPanel(main));
        audio = add(new Panel());
        display = add(new Panel());
        map = add(new Panel());
        general = add(new Panel());
        combat = add(new Panel());
        control = add(new Panel());
        uis = add(new Panel());
        uip = add(new Panel());
        quality = add(new Panel());
        flowermenus = add(new Panel());
        soundalarms = add(new Panel());
        hidesettings = add(new Panel());
        studydesksettings = add(new Panel());
        keybindsettings = add(new Panel());
        chatsettings = add(new Panel());
        clearboulders = add(new Panel());
        clearbushes = add(new Panel());
        cleartrees = add(new Panel());
        clearhides = add(new Panel());

        initMain(gopts);
        initAudio();
        initDisplay();
        initMap();
        initGeneral();
        initCombat();
        initControl();
        initUis();
        initTheme();
        initQuality();
        initFlowermenus();
        initSoundAlarms();
        initHideMenu();
        initstudydesksettings();
        initkeybindsettings();
        initchatsettings();
        
        chpanel(main);
    }

    private void initMain(boolean gopts) {
        main.add(new PButton(200, "Video settings", 'v', video), new Coord(0, 0));
        main.add(new PButton(200, "Audio settings", 'a', audio), new Coord(0, 30));
        main.add(new PButton(200, "Display settings", 'd', display), new Coord(0, 60));
        main.add(new PButton(200, "Map settings", 'm', map), new Coord(0, 90));
        main.add(new PButton(200, "General settings", 'g', general), new Coord(210, 0));
        main.add(new PButton(200, "Combat settings", 'c', combat), new Coord(210, 30));
        main.add(new PButton(200, "Control settings", 'k', control), new Coord(210, 60));
        main.add(new PButton(200, "UI settings", 'u', uis), new Coord(210, 90));
        main.add(new PButton(200, "Quality settings", 'q', quality), new Coord(420, 0));
        main.add(new PButton(200, "Menu settings", 'f', flowermenus), new Coord(420, 30));
        main.add(new PButton(200, "Sound alarms", 's', soundalarms), new Coord(420, 60));
        main.add(new PButton(200, "Hide settings", 'h', hidesettings), new Coord(420, 90));
        main.add(new PButton(200, "Study Desk Options", 'o', studydesksettings), new Coord(0, 120));
        main.add(new PButton(200, "Keybind Options", 'p', keybindsettings), new Coord(210, 120));
        main.add(new PButton(200,"Chat Settings",'c', chatsettings), new Coord(420,120));
        main.add(new PButton(200,"Theme Settings",'t', uip), new Coord(0,150));
        if (gopts) {
            main.add(new Button(200, "Disconnect Discord") {
                public void click() {
                    if(Discord.jdalogin != null) {
                        BotUtils.sysMsg("Discord Disconnected",Color.white);
                        gameui().discordconnected = false;
                        Discord.jdalogin.shutdownNow();
                        Discord.jdalogin = null;
                        for(int i=0;i<15;i++) {
                            for (Widget w = gameui().chat.lchild; w != null; w = w.prev) {
                                if (w instanceof ChatUI.DiscordChat)
                                    w.destroy();
                            }
                        }
                    }else
                        BotUtils.sysMsg("Not currently connected.",Color.white);
                }
            }, new Coord(210, 150));
            main.add(new Button(200, "Join Village Discord") {
                public void click() {
                    if(!gameui().discordconnected) {
                        if (Resource.getLocString(Resource.BUNDLE_LABEL, Config.discordbotkey) != null) {
                            new Thread(new Discord(gameui(), "normal")).start();
                            gameui().discordconnected = true;
                        }
                        else
                            BotUtils.sysMsg("No Key Detected, if there is one in chat settings you might need to relog.",Color.white);
                    }else if(gameui().discordconnected)
                        BotUtils.sysMsg("Already connected.",Color.white);
                }
            }, new Coord(210, 180));
            main.add(new Button(200, "Join Ingame Discord") {
                public void click() {
                    if(gameui().discordconnected)
                        BotUtils.sysMsg("Already Connected.",Color.white);
                    else {
                        new Thread(new Discord(gameui(), "ard")).start();
                        gameui().discordconnected = true;
                    }
                }
            }, new Coord(210, 210));
            main.add(new Button(200, "Join ArdClient Discord") {
                public void click() {
                    try {
                        WebBrowser.self.show(new URL(String.format("https://disc"+"ord.gg/Rx"+"gVh5j")));
                    } catch (WebBrowser.BrowserException e) {
                        getparent(GameUI.class).error("Could not launch web browser.");
                    } catch (MalformedURLException e) {
                    }
                }
            }, new Coord(210, 240));
            main.add(new Button(200, "Show Client Changelog") {
                public void click() {
                   showChangeLog();
                }
            }, new Coord(210, 270));
            main.add(new Button(200, "Switch character") {
                public void click() {
                    GameUI gui = gameui();
                    if(Discord.jdalogin != null)
                    gui.DiscordToggle();
                    gui.act("lo", "cs");
                    if (gui != null & gui.map != null)
                        gui.map.canceltasks();
                }
            }, new Coord(210, 300));
            main.add(new Button(200, "Log out") {
                public void click() {
                    GameUI gui = gameui();
                    if(Discord.jdalogin !=null)
                    gui.DiscordToggle();
                    gui.act("lo");
                    if (gui != null & gui.map != null)
                        gui.map.canceltasks();
                }
            }, new Coord(210, 330));
        }
        main.add(new Button(200, "Close") {
            public void click() {
                OptWnd.this.hide();
            }
        }, new Coord(210, 360));
        main.pack();
    }

    private void initAudio() {
        initAudioFirstColumn();
        audio.add(new PButton(200, "Back", 27, main), new Coord(210, 360));
        audio.pack();
    }

    private void initAudioFirstColumn() {
        final WidgetVerticalAppender appender = new WidgetVerticalAppender(withScrollport(audio, new Coord(620, 350)));
        appender.setVerticalMargin(0);
        appender.add(new Label("Master audio volume"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, (int) (Audio.volume * 1000)) {
            public void changed() {
                Audio.setvolume(val / 1000.0);
            }
        });
        appender.setVerticalMargin(0);
        appender.add(new Label("In-game event volume"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (ui.audio.pos.volume * 1000);
            }

            public void changed() {
                ui.audio.pos.setvolume(val / 1000.0);
            }
        });
        appender.setVerticalMargin(0);
        appender.add(new Label("Ambient volume"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (ui.audio.amb.volume * 1000);
            }

            public void changed() {
                ui.audio.amb.setvolume(val / 1000.0);
            }
        });
        appender.addRow(new Label("Cleave Sound"), makeDropdownCleave());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int)(Config.cleavesoundvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.cleavesoundvol = vol;
                Utils.setprefd("cleavesoundvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.add(new Label("Timers alarm volume"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.timersalarmvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.timersalarmvol = vol;
                Utils.setprefd("timersalarmvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.add(new Label("'Chip' sound volume"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.sfxchipvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.sfxchipvol = vol;
                Utils.setprefd("sfxchipvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.add(new Label("Quern sound volume"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.sfxquernvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.sfxquernvol = vol;
                Utils.setprefd("sfxquernvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.add(new Label("'Whip' sound volume"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.sfxwhipvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.sfxwhipvol = vol;
                Utils.setprefd("sfxwhipvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.add(new Label("Fireplace sound volume (req. restart)"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.sfxfirevol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.sfxfirevol = vol;
                Utils.setprefd("sfxfirevol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.add(new Label("Clapping sound volume"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.sfxclapvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.sfxclapvol = vol;
                Utils.setprefd("sfxclapvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.add(new Label("Cauldron sound volume - Changes are not immediate, will trigger on next cauldon sound start."));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.sfxcauldronvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.sfxcauldronvol = vol;
                Utils.setprefd("sfxcauldronvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.add(new Label("Whistling sound volume"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.sfxwhistlevol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.sfxwhistlevol = vol;
                Utils.setprefd("sfxwhistlevol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.add(new Label("Beehive sound volume"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.sfxbeehivevol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.sfxbeehivevol = vol;
                Utils.setprefd("sfxbeehivevol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.add(new Label("Chat message volume"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.sfxchatvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.sfxchatvol = vol;
                Utils.setprefd("sfxchatvol", vol);
            }
        });

        appender.add(new CheckBox("Enable error sounds.") {
            {
                a = Config.errorsounds;
            }

            public void set(boolean val) {
                Utils.setprefb("errorsounds", val);
                Config.errorsounds = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Enable Cleave sound.") {
            {
                a = Config.cleavesound;
            }

            public void set(boolean val) {
                Utils.setprefb("cleavesound", val);
                Config.cleavesound = val;
                a = val;
            }
        });
    }

    private void initDisplay() {
        initDisplayFirstColumn();
        display.add(new PButton(200, "Back", 27, main), new Coord(210, 360));
        display.pack();
    }

    private void initTheme(){
        final WidgetVerticalAppender appender = new WidgetVerticalAppender(withScrollport(uip, new Coord(620, 350)));
        appender.setVerticalMargin(VERTICAL_MARGIN);
           { //Theme
            final IndirRadioGroup<String> rgrp = new IndirRadioGroup<>("Main Hud Theme (requires restart)", HUDTHEME);
            for(final String name : THEMES.get()) {
                rgrp.add(name, name);
            }
            appender.add(rgrp);
            appender.add(new IndirLabel(() -> String.format("Settings for %s", HUDTHEME.get())));
            appender.add(ColorPreWithLabel("Window Color: ", WNDCOL));
            appender.add(ColorPreWithLabel("Button Color: ", BTNCOL));
            appender.add(ColorPreWithLabel("Textbox Color: ", TXBCOL));
            appender.add(ColorPreWithLabel("Slider Color: ", SLIDERCOL));
            uip.add(new PButton(200, "Back", 27, main), new Coord(210, 380));
            uip.pack();
        }
    }

    private void initDisplayFirstColumn() {
        final WidgetVerticalAppender appender = new WidgetVerticalAppender(withScrollport(display, new Coord(620, 350)));
        appender.setVerticalMargin(VERTICAL_MARGIN);
        appender.add(new CheckBox("Flatten Cupboards - Requires Restart") {
            {
                a = Config.flatcupboards;
            }

            public void set(boolean val) {
                Utils.setprefb("flatcupboards", val);
                Config.flatcupboards = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Always display long tooltips.") {
            {
                a = Config.longtooltips;
            }

            public void set(boolean val) {
                Utils.setprefb("longtooltips", val);
                Config.longtooltips = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Display kin names") {
            {
                a = Config.showkinnames;
            }

            public void set(boolean val) {
                Utils.setprefb("showkinnames", val);
                Config.showkinnames = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Display item completion progress bar") {
            {
                a = Config.itemmeterbar;
            }

            public void set(boolean val) {
                Utils.setprefb("itemmeterbar", val);
                Config.itemmeterbar = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Show hourglass percentage") {
            {
                a = Config.showprogressperc;
            }

            public void set(boolean val) {
                Utils.setprefb("showprogressperc", val);
                Config.showprogressperc = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Show attributes & softcap values in craft window") {
            {
                a = Config.showcraftcap;
            }

            public void set(boolean val) {
                Utils.setprefb("showcraftcap", val);
                Config.showcraftcap = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Show objects health") {
            {
                a = Config.showgobhp;
            }

            public void set(boolean val) {
                Utils.setprefb("showgobhp", val);
                Config.showgobhp = val;
                a = val;

                GameUI gui = gameui();
                if (gui != null && gui.map != null) {
                    if (val)
                        gui.map.addHealthSprites();
                    else
                        gui.map.removeCustomSprites(Sprite.GOB_HEALTH_ID);
                }
            }
        });
        appender.add(new CheckBox("Show player's path") {
            {
                a = Config.showplayerpaths;
            }

            public void set(boolean val) {
                Utils.setprefb("showplayerpaths", val);
                Config.showplayerpaths = val;
                a = val;
            }
        });
       // appender.add(new IndirCheckBox("Show Gob Paths", SHOWGOBPATH));
       // appender.add(ColorPreWithLabel("Minimap path color: ", MMPATHCOL));
        //appender.add(ColorPreWithLabel("Unknown gob path color: ", GOBPATHCOL));
       // appender.add(ColorPreWithLabel("Vehicle path color: ", VEHPATHCOL));
        appender.add(new CheckBox("Show wear bars") {
            {
                a = Config.showwearbars;
            }

            public void set(boolean val) {
                Utils.setprefb("showwearbars", val);
                Config.showwearbars = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Show animal radius - Restart if changing colors") {
            {
                a = Config.showanimalrad;
            }

            public void set(boolean val) {
                Utils.setprefb("showanimalrad", val);
                Config.showanimalrad = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Colorful Cave Dust") {
            {
                a = Config.colorfulcaveins;
            }

            public void set(boolean val) {
                Utils.setprefb("colorfulcaveins", val);
                Config.colorfulcaveins = val;
                a = val;
            }
        });
        appender.addRow(new Label("Cave-in Warning Dust Duration in Minutes"),makeCaveInDropdown());
        appender.add(new CheckBox("Double animal radius size.") {
            {
                a = Config.doubleradius;
            }

            public void set(boolean val) {
                Utils.setprefb("doubleradius", val);
                Config.doubleradius = val;
                a = val;
            }
        });
        appender.add(ColorPreWithLabel("Deep Ocean Color: (requires relog)", DEEPWATERCOL));
        appender.add(ColorPreWithLabel("All Other Water Color: (requires relog)", ALLWATERCOL));
        appender.add(new Label("Radius changes require game restart to apply."));
        appender.add(new Label("Radius RGB Red Animals"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.smatdangerred);
            }

            public void changed() {
                int vol = val;
                Config.smatdangerred = vol;
                Utils.setprefi("smatdangerred", vol);
                BPRadSprite.smatDanger = new States.ColState(new Color(Config.smatdangerred, Config.smatdangergreen, Config.smatdangerblue, 100));
            }
        });

        appender.setVerticalMargin(0);
        appender.add(new Label("Radius RGB Green Animals"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.smatdangergreen);
            }

            public void changed() {
                int vol = val;
                Config.smatdangergreen = vol;
                Utils.setprefi("smatdangergreen", vol);
                BPRadSprite.smatDanger = new States.ColState(new Color(Config.smatdangerred, Config.smatdangergreen, Config.smatdangerblue, 100));
            }
        });

        appender.setVerticalMargin(0);
        appender.add(new Label("Radius RGB Blue Animals"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.smatdangerblue);
            }
            public void changed() {
                int vol = val;
                Config.smatdangerblue = vol;
                Utils.setprefi("smatdangerblue", vol);
                BPRadSprite.smatDanger = new States.ColState(new Color(Config.smatdangerred, Config.smatdangergreen, Config.smatdangerblue, 100));
            }
        });
        appender.add(new Label("Radius RGB Red Mine Supports"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.smatSupportsred);
            }

            public void changed() {
                int vol = val;
                Config.smatSupportsred = vol;
                Utils.setprefi("smatSupportsred", vol);
                BPRadSprite.smatSupports = new States.ColState(new Color(Config.smatSupportsred, Config.smatSupportsgreen, Config.smatSupportsblue, 100));
            }
        });

        appender.setVerticalMargin(0);
        appender.add(new Label("Radius RGB Green Mine Supports"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.smatSupportsgreen);
            }

            public void changed() {
                int vol = val;
                Config.smatSupportsgreen = vol;
                Utils.setprefi("smatSupportsgreen", vol);
                BPRadSprite.smatSupports = new States.ColState(new Color(Config.smatSupportsred, Config.smatSupportsgreen, Config.smatSupportsblue, 100));
            }
        });

        appender.setVerticalMargin(0);
        appender.add(new Label("Radius RGB Blue Mine Supports"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.smatSupportsblue);
            }
            public void changed() {
                int vol = val;
                Config.smatSupportsblue = vol;
                Utils.setprefi("smatSupportsblue", vol);
                BPRadSprite.smatSupports = new States.ColState(new Color(Config.smatSupportsred, Config.smatSupportsgreen, Config.smatSupportsblue, 100));
            }
        });
        appender.add(new CheckBox("Show aggro radius on bats, disable if you dont care if they aggro you.") {
            {
                a = Config.batcircle;
            }

            public void set(boolean val) {
                Utils.setprefb("batcircle", val);
                Config.batcircle = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Show aggro radius on cave slimes, disable if you dont care if they aggro you.") {
            {
                a = Config.slimecircle;
            }

            public void set(boolean val) {
                Utils.setprefb("slimecircle", val);
                Config.slimecircle = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Highlight empty/finished drying frames and full/empty tanning tubs. Requires restart.") {
            {
                a = Config.showdframestatus;
            }

            public void set(boolean val) {
                Utils.setprefb("showdframestatus", val);
                Config.showdframestatus = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Highlight chicken coops based on food/water needs.") {
            {
                a = Config.showcoopstatus;
            }

            public void set(boolean val) {
                Utils.setprefb("showcoopstatus", val);
                Config.showcoopstatus = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Highlight rabbit hutches based on food/water needs.") {
            {
                a = Config.showhutchstatus;
            }

            public void set(boolean val) {
                Utils.setprefb("showhutchstatus", val);
                Config.showhutchstatus = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Highlight full cupboards with a slight red tint. Requires restart.") {
            {
                a = Config.showcupboardstatus;
            }

            public void set(boolean val) {
                Utils.setprefb("showcupboardstatus", val);
                Config.showdframestatus = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Highlight empty/full cheese racks. Requires restart.") {
            {
                a = Config.showrackstatus;
            }

            public void set(boolean val) {
                Utils.setprefb("showrackstatus", val);
                Config.showrackstatus = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Highlight partially full cheese racks.") {
            {
                a = Config.cRackmissing;
            }

            public void set(boolean val) {
                Utils.setprefb("cRackmissing", val);
                Config.cRackmissing = val;
                a = val;
            }
        });
        appender.add(new Label("Radius RGB Red Cheese Rack Missing Color"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.cRackred);
            }

            public void changed() {
                int vol = val;
                Config.cRackred = vol;
                Utils.setprefi("cRackred", vol);
                BPRadSprite.cRackMissing = new Material.Colors(new Color(Config.cRackred, Config.cRackgreen, Config.cRackblue, 255));
            }
        });

        appender.setVerticalMargin(0);
        appender.add(new Label("Radius RGB Green Cheese Rack Missing Color"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.cRackgreen);
            }

            public void changed() {
                int vol = val;
                Config.cRackgreen = vol;
                Utils.setprefi("cRackgreen", vol);
                BPRadSprite.cRackMissing = new Material.Colors(new Color(Config.cRackred, Config.cRackgreen, Config.cRackblue, 255));
            }
        });

        appender.setVerticalMargin(0);
        appender.add(new Label("Radius RGB Blue Cheese Rack Missing Color"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.cRackblue);
            }
            public void changed() {
                int vol = val;
                Config.cRackblue = vol;
                Utils.setprefi("cRackblue", vol);
                BPRadSprite.cRackMissing = new Material.Colors(new Color(Config.cRackred, Config.cRackgreen, Config.cRackblue, 255));
            }
        });
        appender.add(new CheckBox("Highlight finished garden pots. Requires restart.") {
            {
                a = Config.highlightpots;
            }

            public void set(boolean val) {
                Utils.setprefb("highlightpots", val);
                Config.highlightpots = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Draw circles around party members.") {
            {
                a = Config.partycircles;
            }

            public void set(boolean val) {
                Utils.setprefb("partycircles", val);
                Config.partycircles = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Draw circles around kinned players") {
            {
                a = Config.kincircles;
            }

            public void set(boolean val) {
                Utils.setprefb("kincircles", val);
                Config.kincircles = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Draw circle on ground around yourself.") {
            {
                a = Config.playercircle;
            }

            public void set(boolean val) {
                Utils.setprefb("playercircle", val);
                Config.playercircle = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Draw green circle around paving stranglevines") {
            {
                a = Config.stranglevinecircle;
            }

            public void set(boolean val) {
                Utils.setprefb("stranglevinecircle", val);
                Config.stranglevinecircle = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Show last used curios in study window") {
            {
                a = Config.studyhist;
            }

            public void set(boolean val) {
                Utils.setprefb("studyhist", val);
                Config.studyhist = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Display buff icon when study has free slots") {
            {
                a = Config.studybuff;
            }

            public void set(boolean val) {
                Utils.setprefb("studybuff", val);
                Config.studybuff = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Miniature trees (req. logout)") {
            {
                a = Config.bonsai;
            }

            public void set(boolean val) {
                Utils.setprefb("bonsai", val);
                Config.bonsai = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Hide skybox") {
            {
                a = Config.hidesky;
            }
            
            public void set(boolean val) {
                Utils.setprefb("hidesky", val);
                Config.hidesky = val;
                a = val;
            }
        });
        Button OutputSettings = new Button(220, "Output Light Settings") {
            @Override
            public void click() {
                BotUtils.sysLogAppend("Ambient Red "+Config.AmbientRed,"white");
                BotUtils.sysLogAppend("Ambient Green "+Config.AmbientGreen,"white");
                BotUtils.sysLogAppend("Ambient Blue "+Config.AmbientBlue,"white");

                BotUtils.sysLogAppend("Diffuse Red "+Config.DiffuseRed,"white");
                BotUtils.sysLogAppend("Diffuse Green "+Config.DiffuseGreen,"white");
                BotUtils.sysLogAppend("Diffuse Blue "+Config.DiffuseBlue,"white");

                BotUtils.sysLogAppend("Specular Red "+Config.SpecRed,"white");
                BotUtils.sysLogAppend("Specular Green "+Config.SpecGreen,"white");
                BotUtils.sysLogAppend("Specular Blue "+Config.SpecBlue,"white");
            }
        };
        appender.add(OutputSettings);
        appender.add(new Label("Ghandhi Lighting Presets"));
        Button Preset1 = new Button(220, "Friday Evening") {
            @Override
            public void click() {
                Config.AmbientRed = 51;
                Utils.setprefi("AmbientRed", 51);
                Config.AmbientGreen = 59;
                Utils.setprefi("AmbientGreen", 59);
                Config.AmbientBlue = 119;
                Utils.setprefi("AmbientBlue", 119);

                Config.DiffuseRed = 20;
                Utils.setprefi("DiffuseRed", 20);
                Config.DiffuseGreen = 28;
                Utils.setprefi("DiffuseGreen", 28);
                Config.DiffuseBlue = 127;
                Utils.setprefi("DiffuseBlue", 127);

                Config.SpecRed = 167;
                Utils.setprefi("SpecRed", 167);
                Config.SpecGreen = 117;
                Utils.setprefi("SpecGreen", 117);
                Config.SpecBlue = 103;
                Utils.setprefi("SpecBlue", 103);
            }
        };
        appender.add(Preset1);
        Button Preset2 = new Button(220, "Thieving Night") {
            @Override
            public void click() {
                Config.AmbientRed = 5;
                Utils.setprefi("AmbientRed", 5);
                Config.AmbientGreen = 10;
                Utils.setprefi("AmbientGreen", 10);
                Config.AmbientBlue = 51;
                Utils.setprefi("AmbientBlue", 51);

                Config.DiffuseRed = 0;
                Utils.setprefi("DiffuseRed", 0);
                Config.DiffuseGreen = 31;
                Utils.setprefi("DiffuseGreen", 31);
                Config.DiffuseBlue = 50;
                Utils.setprefi("DiffuseBlue", 50);

                Config.SpecRed = 138;
                Utils.setprefi("SpecRed", 138);
                Config.SpecGreen = 64;
                Utils.setprefi("SpecGreen", 64);
                Config.SpecBlue = 255;
                Utils.setprefi("SpecBlue", 255);
            }
        };
        appender.add(Preset2);
        Button Preset3 = new Button(220, "Hunting Dusk") {
            @Override
            public void click() {
                Config.AmbientRed = 165;
                Utils.setprefi("AmbientRed", 165);
                Config.AmbientGreen = 213;
                Utils.setprefi("AmbientGreen", 213);
                Config.AmbientBlue = 255;
                Utils.setprefi("AmbientBlue", 255);

                Config.DiffuseRed = 160;
                Utils.setprefi("DiffuseRed", 160);
                Config.DiffuseGreen = 193;
                Utils.setprefi("DiffuseGreen", 193);
                Config.DiffuseBlue = 255;
                Utils.setprefi("DiffuseBlue", 255);

                Config.SpecRed = 138;
                Utils.setprefi("SpecRed", 138);
                Config.SpecGreen = 64;
                Utils.setprefi("SpecGreen", 64);
                Config.SpecBlue = 255;
                Utils.setprefi("SpecBlue", 255);
            }
        };
        appender.add(Preset3);
        Button Preset4 = new Button(220, "Sunny Morning") {
            @Override
            public void click() {
                Config.AmbientRed = 211;
                Utils.setprefi("AmbientRed", 211);
                Config.AmbientGreen = 180;
                Utils.setprefi("AmbientGreen", 180);
                Config.AmbientBlue = 72;
                Utils.setprefi("AmbientBlue", 72);

                Config.DiffuseRed = 255;
                Utils.setprefi("DiffuseRed", 255);
                Config.DiffuseGreen = 178;
                Utils.setprefi("DiffuseGreen", 178);
                Config.DiffuseBlue = 169;
                Utils.setprefi("DiffuseBlue", 169);

                Config.SpecRed = 255;
                Utils.setprefi("SpecRed", 255);
                Config.SpecGreen = 255;
                Utils.setprefi("SpecGreen", 255);
                Config.SpecBlue = 255;
                Utils.setprefi("SpecBlue", 255);
            }
        };
        appender.add(Preset4);
        appender.add(new Label("Default Lighting"));
        Button Preset5 = new Button(220, "Amber Default") {
            @Override
            public void click() {
                Config.AmbientRed = 200;
                Utils.setprefi("AmbientRed", 200);
                Config.AmbientGreen = 200;
                Utils.setprefi("AmbientGreen", 200);
                Config.AmbientBlue = 200;
                Utils.setprefi("AmbientBlue", 200);

                Config.DiffuseRed = 200;
                Utils.setprefi("DiffuseRed", 200);
                Config.DiffuseGreen = 200;
                Utils.setprefi("DiffuseGreen", 200);
                Config.DiffuseBlue = 200;
                Utils.setprefi("DiffuseBlue", 200);

                Config.SpecRed = 255;
                Utils.setprefi("SpecRed", 255);
                Config.SpecGreen = 255;
                Utils.setprefi("SpecGreen", 255);
                Config.SpecBlue = 255;
                Utils.setprefi("SpecBlue", 255);
            }
        };
        appender.add(Preset5);
        appender.add(new IndirCheckBox("Dark Mode (overrides custom global light)", DARKMODE));
        appender.add(new Label("Night Vision Ambient Red"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.AmbientRed);
            }

            public void changed() {
                int vol = val;
                Config.AmbientRed = vol;
                Utils.setprefi("AmbientRed", vol);
            }
        });
        appender.add(new Label("Night Vision Ambient Green"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.AmbientGreen);
            }

            public void changed() {
                int vol = val;
                Config.AmbientGreen = vol;
                Utils.setprefi("AmbientGreen", vol);
            }
        });
        appender.add(new Label("Night Vision Ambient Blue"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.AmbientBlue);
            }

            public void changed() {
                int vol = val;
                Config.AmbientBlue = vol;
                Utils.setprefi("AmbientBlue", vol);
            }
        });
        appender.add(new Label("Night Vision Diffuse Red"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.DiffuseRed);
            }

            public void changed() {
                int vol = val;
                Config.DiffuseRed = vol;
                Utils.setprefi("DiffuseRed", vol);
            }
        });
        appender.add(new Label("Night Vision Diffuse Green"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.DiffuseGreen);
            }

            public void changed() {
                int vol = val;
                Config.DiffuseGreen = vol;
                Utils.setprefi("DiffuseGreen", vol);
            }
        });
        appender.add(new Label("Night Vision Diffuse Blue"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.DiffuseBlue);
            }

            public void changed() {
                int vol = val;
                Config.DiffuseBlue = vol;
                Utils.setprefi("DiffuseBlue", vol);
            }
        });
        appender.add(new Label("Night Vision Specular Red"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.SpecRed);
            }

            public void changed() {
                int vol = val;
                Config.SpecRed = vol;
                Utils.setprefi("SpecRed", vol);
            }
        });
        appender.add(new Label("Night Vision Specular Green"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.SpecGreen);
            }

            public void changed() {
                int vol = val;
                Config.SpecGreen = vol;
                Utils.setprefi("SpecGreen", vol);
            }
        });
        appender.add(new Label("Night Vision Specular Blue"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.SpecBlue);
            }

            public void changed() {
                int vol = val;
                Config.SpecBlue = vol;
                Utils.setprefi("SpecBlue", vol);
            }
        });

    }

    private void initMap() {
        map.add(new Label("Show boulders:"), new Coord(10, 0));
        map.add(new Label("Show bushes:"), new Coord(165, 0));
        map.add(new Label("Show trees:"), new Coord(320, 0));
        map.add(new Label("Hide icons:"), new Coord(475, 0));

        map.add(new PButton(200, "Back", 27, main), new Coord(210, 380));
        map.pack();
    }

    private void initGeneral() {
        final WidgetVerticalAppender appender = new WidgetVerticalAppender(withScrollport(general, new Coord(620, 350)));

        appender.setVerticalMargin(VERTICAL_MARGIN);
        appender.setHorizontalMargin(HORIZONTAL_MARGIN);

        appender.add(new CheckBox("Save chat logs to disk") {
            {
                a = Config.chatsave;
            }

            public void set(boolean val) {
                Utils.setprefb("chatsave", val);
                Config.chatsave = val;
                a = val;
                if (!val && Config.chatlog != null) {
                    try {
                        Config.chatlog.close();
                        Config.chatlog = null;
                    } catch (Exception e) {
                    }
                }
            }
        });
        appender.add(new CheckBox("Save map tiles to disk - No performance benefit, this is only for creating your own maps or uploading.") {
            {
                a = Config.savemmap;
            }

            public void set(boolean val) {
                Utils.setprefb("savemmap", val);
                Config.savemmap = val;
                MapGridSave.mgs = null;
                a = val;
            }
        });
        appender.add(new CheckBox("Show timestamps in chats") {
            {
                a = Config.chattimestamp;
            }

            public void set(boolean val) {
                Utils.setprefb("chattimestamp", val);
                Config.chattimestamp = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Notify when kin comes online") {
            {
                a = Config.notifykinonline;
            }

            public void set(boolean val) {
                Utils.setprefb("notifykinonline", val);
                Config.notifykinonline = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Abandon quests on right click") {
            {
                a = Config.abandonrightclick;
            }

            public void set(boolean val) {
                Utils.setprefb("abandonrightclick", val);
                Config.abandonrightclick = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Auto hearth") {
            {
                a = Config.autohearth;
            }

            public void set(boolean val) {
                Utils.setprefb("autohearth", val);
                Config.autohearth = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Auto logout on unknown/red players") {
            {
                a = Config.autologout;
            }

            public void set(boolean val) {
                Utils.setprefb("autologout", val);
                Config.autologout = val;
                a = val;
            }
        });
        appender.addRow(new Label("Auto Logout after x Minutes - 0 means never"), makeafkTimeDropdown());
        appender.add(new CheckBox("Repeat Starvation Alert Warning/Sound") {
            {
                a = Config.StarveAlert;
            }

            public void set(boolean val) {
                Utils.setprefb("StarveAlert", val);
                Config.StarveAlert = val;
                a = val;
            }
        });
        appender.addRow(new Label("Attribute Increase per mouse scroll"), makeStatGainDropdown());
        appender.add(new CheckBox("Run on login") {
            {
                a = Config.runonlogin;
            }

            public void set(boolean val) {
                Utils.setprefb("runonlogin", val);
                Config.runonlogin = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Character Equipment on Login") {
            {
                a = Config.logincharsheet;
            }

            public void set(boolean val) {
                Utils.setprefb("logincharsheet", val);
                Config.logincharsheet = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Open Belt on login") {
            {
                a = Config.loginbelt;
            }

            public void set(boolean val) {
                Utils.setprefb("loginbelt", val);
                Config.loginbelt = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Show inventory on login") {
            {
                a = Config.showinvonlogin;
            }

            public void set(boolean val) {
                Utils.setprefb("showinvonlogin", val);
                Config.showinvonlogin = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Study Window on Login") {
            {
                a = Config.loginstudywnd;
            }

            public void set(boolean val) {
                Utils.setprefb("loginstudywnd", val);
                Config.loginstudywnd = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Show server time") {
            {
                a = Config.showservertime;
            }

            public void set(boolean val) {
                Utils.setprefb("showservertime", val);
                Config.showservertime = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Drop leeches automatically") {
            {
                a = Config.leechdrop;
            }

            public void set(boolean val) {
                Utils.setprefb("leechdrop", val);
                Config.leechdrop = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Auto switch to speed 3 on horse") {
            {
                a = Config.horseautorun;
            }

            public void set(boolean val) {
                Utils.setprefb("horseautorun", val);
                Config.horseautorun = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Enable tracking on login") {
            {
                a = Config.enabletracking;
            }

            public void set(boolean val) {
                Utils.setprefb("enabletracking", val);
                Config.enabletracking = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Enable swimming on login") {
            {
                a = Config.enableswimming;
            }

            public void set(boolean val) {
                Utils.setprefb("enableswimming", val);
                Config.enableswimming = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Enable criminal acts on login") {
            {
                a = Config.enablecrime;
            }

            public void set(boolean val) {
                Utils.setprefb("enablecrime", val);
                Config.enablecrime = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Drop mined stones") {
            {
                a = Config.dropMinedStones;
            }

            public void set(boolean val) {
                Utils.setprefb("dropMinedStones", val);
                Config.dropMinedStones = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Drop mined ore") {
            {
                a = Config.dropMinedOre;
            }

            public void set(boolean val) {
                Utils.setprefb("dropMinedOre", val);
                Config.dropMinedOre = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Drop mined silver/gold ore") {
            {
                a = Config.dropMinedOrePrecious;
            }

            public void set(boolean val) {
                Utils.setprefb("dropMinedOrePrecious", val);
                Config.dropMinedOrePrecious = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Drop mined Cat Gold.") {
            {
                a = Config.dropMinedCatGold;
            }

            public void set(boolean val) {
                Utils.setprefb("dropMinedCatGold", val);
                Config.dropMinedCatGold = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Drop mined Petrified SeaShells.") {
            {
                a = Config.dropMinedSeaShells;
            }

            public void set(boolean val) {
                Utils.setprefb("dropMinedSeaShells", val);
                Config.dropMinedSeaShells = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Drop mined Strange Crystals.") {
            {
                a = Config.dropMinedCrystals;
            }

            public void set(boolean val) {
                Utils.setprefb("dropMinedCrystals", val);
                Config.dropMinedCrystals = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Drop Gneiss from Ore Smelters Automatically if Coal script window is open.") {
            {
                a = Config.dropsmelterstones;
            }

            public void set(boolean val) {
                Utils.setprefb("dropsmelterstones", val);
                Config.dropsmelterstones = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Shoo animals with Ctrl+Left Click") {
            {
                a = Config.shooanimals;
            }

            public void set(boolean val) {
                Utils.setprefb("shooanimals", val);
                Config.shooanimals = val;
                a = val;
            }
        });
        general.add(new PButton(200, "Back", 27, main), new Coord(210, 360));
        general.pack();
    }

    private void initCombat() {
        final WidgetVerticalAppender appender = new WidgetVerticalAppender(withScrollport(combat, new Coord(620, 350)));

        appender.setVerticalMargin(VERTICAL_MARGIN);
        appender.setHorizontalMargin(HORIZONTAL_MARGIN);

        appender.add(new CheckBox("Display damage") {
            {
                a = Config.showdmgop;
            }

            public void set(boolean val) {
                Utils.setprefb("showdmgop", val);
                Config.showdmgop = val;
                a = val;
            }
        });

        appender.add(new CheckBox("Display info above untargeted enemies") {
            {
                a = Config.showothercombatinfo;
            }

            public void set(boolean val) {
                Utils.setprefb("showothercombatinfo", val);
                Config.showothercombatinfo = val;
                a = val;
            }
        });
        appender.addRow(new Label("Combat Start Sound"), makeDropdownCombat());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int)(Config.attackedvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.attackedvol = vol;
                Utils.setprefd("attackedvol", vol);
            }
        });
        appender.add(new CheckBox("Highlight current opponent") {
            {
                a = Config.hlightcuropp;
            }

            public void set(boolean val) {
                Utils.setprefb("hlightcuropp", val);
                Config.hlightcuropp = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Display cooldown time") {
            {
                a = Config.showcooldown;
            }

            public void set(boolean val) {
                Utils.setprefb("showcooldown", val);
                Config.showcooldown = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Show arrow vectors") {
            {
                a = Config.showarchvector;
            }

            public void set(boolean val) {
                Utils.setprefb("showarchvector", val);
                Config.showarchvector = val;
                a = val;
            }
        });
        /*appender.add(new CheckBox("Show attack cooldown delta") {
            {
                a = Config.showcddelta;
            }

            public void set(boolean val) {
                Utils.setprefb("showcddelta", val);
                Config.showcddelta = val;
                a = val;
            }
        });*/
        appender.add(new CheckBox("Log combat actions to system log") {
            {
                a = Config.logcombatactions;
            }

            public void set(boolean val) {
                Utils.setprefb("logcombatactions", val);
                Config.logcombatactions = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Alternative combat UI") {
            {
                a = Config.altfightui;
            }

            public void set(boolean val) {
                Utils.setprefb("altfightui", val);
                Config.altfightui = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Simplified opening indicators") {
            {
                a = Config.combaltopenings;
            }

            public void set(boolean val) {
                Utils.setprefb("combaltopenings", val);
                Config.combaltopenings = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Show key bindings in combat UI") {
            {
                a = Config.combshowkeys;
            }

            public void set(boolean val) {
                Utils.setprefb("combshowkeys", val);
                a = val;
            }
        });
        appender.add(new CheckBox("Aggro players in proximity to the mouse cursor") {
            {
                a = Config.proximityaggropvp;
            }

            public void set(boolean val) {
                Utils.setprefb("proximityaggropvp", val);
                Config.proximityaggropvp = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Aggro animals in proximity to the mouse cursor") {
            {
                a = Config.proximityaggro;
            }

            public void set(boolean val) {
                Utils.setprefb("proximityaggro", val);
                Config.proximityaggro = val;
                a = val;
            }
        });
        appender.addRow(new Label("Combat key bindings:"), combatkeysDropdown());

        combat.add(new PButton(200, "Back", 27, main), new Coord(210, 360));
        combat.pack();
    }

    private void initControl() {
        final WidgetVerticalAppender appender = new WidgetVerticalAppender(withScrollport(control, new Coord(620, 350)));

        appender.setVerticalMargin(VERTICAL_MARGIN);
        appender.setHorizontalMargin(HORIZONTAL_MARGIN);

        appender.addRow(new Label("Bad camera scrolling sensitivity"),
                new HSlider(50, 0, 50, 0) {
                    protected void attach(UI ui) {
                        super.attach(ui);
                        val = Config.badcamsensitivity;
                    }

                    public void changed() {
                        Config.badcamsensitivity = val;
                        Utils.setprefi("badcamsensitivity", val);
                    }
                });
        appender.add(new CheckBox("Use French (AZERTY) keyboard layout") {
            {
                a = Config.userazerty;
            }

            public void set(boolean val) {
                Utils.setprefb("userazerty", val);
                Config.userazerty = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Reverse bad camera MMB x-axis") {
            {
                a = Config.reversebadcamx;
            }

            public void set(boolean val) {
                Utils.setprefb("reversebadcamx", val);
                Config.reversebadcamx = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Reverse bad camera MMB y-axis") {
            {
                a = Config.reversebadcamy;
            }

            public void set(boolean val) {
                Utils.setprefb("reversebadcamy", val);
                Config.reversebadcamy = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Force hardware cursor (req. restart)") {
            {
                a = Config.hwcursor;
            }

            public void set(boolean val) {
                Utils.setprefb("hwcursor", val);
                Config.hwcursor = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Disable dropping items over water (overridable with Ctrl)") {
            {
                a = Config.nodropping;
            }

            public void set(boolean val) {
                Utils.setprefb("nodropping", val);
                Config.nodropping = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Disable dropping items over anywhere (overridable with Ctrl)") {
            {
                a = Config.nodropping_all;
            }

            public void set(boolean val) {
                Utils.setprefb("nodropping_all", val);
                Config.nodropping_all = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Enable full zoom-out in Ortho cam") {
            {
                a = Config.enableorthofullzoom;
            }

            public void set(boolean val) {
                Utils.setprefb("enableorthofullzoom", val);
                Config.enableorthofullzoom = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Disable hotkey (tilde/back-quote key) for drinking") {
            {
                a = Config.disabledrinkhotkey;
            }

            public void set(boolean val) {
                Utils.setprefb("disabledrinkhotkey", val);
                Config.disabledrinkhotkey = val;
                a = val;
            }
        });
        appender.add(new Label("Disable Shift Right Click for :"));
        CheckListbox disableshiftclick = new CheckListbox(320, Math.min(8, Config.disableshiftclick.values().size()), 18 + Config.fontadd) {
            @Override
            protected void itemclick(CheckListboxItem itm, int button) {
                super.itemclick(itm, button);
                Utils.setprefchklst("disableshiftclick", Config.disableshiftclick);
            }
        };
        for (CheckListboxItem itm : Config.disableshiftclick.values())
            disableshiftclick.items.add(itm);
        appender.add(disableshiftclick);


        control.add(new PButton(200, "Back", 27, main), new Coord(210, 360));
        control.pack();
    }

    private void initUis() {
        final WidgetVerticalAppender appender = new WidgetVerticalAppender(withScrollport(uis, new Coord(620, 310)));

        appender.setVerticalMargin(VERTICAL_MARGIN);
        appender.setHorizontalMargin(HORIZONTAL_MARGIN);

        appender.addRow(new Label("Language (req. restart):"), langDropdown());
        appender.add(new CheckBox("Disable all menugrid hotkeys (Bottom Right grid)") {
            {
                a = Config.disablemenugrid;
            }

            public void set(boolean val) {
                Utils.setprefb("disablemenugrid", val);
                Config.disablemenugrid = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Show quick hand slots") {
            {
                a = Config.quickslots;
            }

            public void set(boolean val) {
                Utils.setprefb("quickslots", val);
                Config.quickslots = val;
                a = val;

                try {
                    Widget qs = ((GameUI) parent.parent.parent).quickslots;
                    if (qs != null) {
                        if (val)
                            qs.show();
                        else
                            qs.hide();
                    }
                } catch (ClassCastException e) { // in case we are at the login screen
                }
            }
        });
        appender.add(new CheckBox("Alternative equipment belt window") {
            {
                a = Config.quickbelt;
            }

            public void set(boolean val) {
                Utils.setprefb("quickbelt", val);
                Config.quickbelt = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Hide Calendar Widget on login.") {
            {
                a = Config.hidecalendar;
            }

            public void set(boolean val) {
                Utils.setprefb("hidecalendar", val);
                Config.hidecalendar = val;
                a = val;
                if(gameui() != null)
                gameui().cal.visible = !Config.hidecalendar;
            }
        });
        appender.add(new CheckBox("Close windows with escape key.") {
            {
                a = Config.escclosewindows;
            }

            public void set(boolean val) {
                Utils.setprefb("escclosewindows", val);
                Config.escclosewindows = val;
                a = val;
            }
        });
        appender.add(new IndirCheckBox("Show F Key Belt", SHOWFKBELT, val -> {
            if(ui.gui != null && ui.gui.fbelt != null) {
                ui.gui.fbelt.setVisibile(val);
            }
        }));
        appender.add(new IndirCheckBox("Show NumPad Key Belt", SHOWNPBELT, val -> {
            if(ui.gui != null && ui.gui.npbelt != null) {
                ui.gui.npbelt.setVisibile(val);
            }
        }));
        appender.add(new IndirCheckBox("Show Number Key Belt", SHOWNBELT, val -> {
            if(ui.gui != null && ui.gui.nbelt != null) {
                ui.gui.nbelt.setVisibile(val);
            }
        }));
        appender.add(new CheckBox("Show hungermeter") {
            {
                a = Config.hungermeter;
            }

            public void set(boolean val) {
                Utils.setprefb("hungermeter", val);
                Config.hungermeter = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Show fepmeter") {
            {
                a = Config.fepmeter;
            }

            public void set(boolean val) {
                Utils.setprefb("fepmeter", val);
                Config.fepmeter = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Hide quests panel") {
            {
                a = Config.noquests;
            }

            public void set(boolean val) {
                Utils.setprefb("noquests", val);
                Config.noquests = val;
                try {
                    if (val)
                        gameui().questwnd.hide();
                    else
                        gameui().questwnd.show();
                } catch (NullPointerException npe) { // ignored
                }
                a = val;
            }
        });
        appender.add(new CheckBox("Show Craft/Build history toolbar") {
            {
                a = Config.histbelt;
            }

            public void set(boolean val) {
                Utils.setprefb("histbelt", val);
                Config.histbelt = val;
                a = val;
                GameUI gui = gameui();
                if (gui != null) {
                    CraftHistoryBelt histbelt = gui.histbelt;
                    if (histbelt != null) {
                        if (val)
                            histbelt.show();
                        else
                            histbelt.hide();
                    }
                }
            }
        });
        appender.add(new CheckBox("Display confirmation dialog when using magic") {
            {
                a = Config.confirmmagic;
            }

            public void set(boolean val) {
                Utils.setprefb("confirmmagic", val);
                Config.confirmmagic = val;
                a = val;
            }
        });
        appender.addRow(new Label("Tree bounding box color (6-digit HEX):"),
                new TextEntry(85, Config.treeboxclr) {
                    @Override
                    public boolean type(char c, KeyEvent ev) {
                        if (!parent.visible)
                            return false;

                        boolean ret = buf.key(ev);
                        if (text.length() == 6) {
                            Color clr = Utils.hex2rgb(text);
                            if (clr != null) {
                                GobHitbox.fillclrstate = new States.ColState(clr);
                                Utils.setpref("treeboxclr", text);
                            }
                        }
                        return ret;
                    }
                }
        );

        appender.addRow(new Label("Chat font size (req. restart):"), makeFontSizeChatDropdown());
        appender.add(new CheckBox("Font antialiasing") {
            {
                a = Config.fontaa;
            }

            public void set(boolean val) {
                Utils.setprefb("fontaa", val);
                Config.fontaa = val;
                a = val;
            }
        });
        appender.addRow(new CheckBox("Custom interface font (req. restart):") {
            {
                a = Config.usefont;
            }

            public void set(boolean val) {
                Utils.setprefb("usefont", val);
                Config.usefont = val;
                a = val;
            }
        },
                makeFontsDropdown());
        appender.add(new CheckBox("Larger quality/quantity text (req. restart):") {
            {
                a = Config.largeqfont;
            }

            public void set(boolean val) {
                Utils.setprefb("largeqfont", val);
                Config.largeqfont = val;
                a = val;
            }
        });
        final Label fontAdd = new Label("");
        appender.addRow(
                new Label("Increase font size by (req. restart):"),
                new HSlider(160, 0, 3, Config.fontadd) {
                    public void added() {
                        updateLabel();
                    }
                    public void changed() {
                        Utils.setprefi("fontadd", val);
                        Config.fontadd = val;
                        updateLabel();
                    }
                    private void updateLabel() {
                        fontAdd.settext(String.format("%d", val));
                    }
                },
                fontAdd
        );


        Button resetWndBtn = new Button(220, "Reset Windows (req. logout)") {
            @Override
            public void click() {
                try {
                    for (String key : Utils.prefs().keys()) {
                        if (key.endsWith("_c")) {
                            Utils.delpref(key);
                        }
                    }
                } catch (BackingStoreException e) {
                }
                Utils.delpref("mmapc");
                Utils.delpref("mmapwndsz");
                Utils.delpref("mmapsz");
                Utils.delpref("quickslotsc");
                Utils.delpref("chatsz");
                Utils.delpref("chatvis");
                Utils.delpref("menu-visible");
                Utils.delpref("fbelt_vertical");
                Utils.delpref("haven.study.position");
            }
        };
        uis.add(resetWndBtn, new Coord(620 / 2 - resetWndBtn.sz.x / 2 , 320));
        uis.add(new PButton(200, "Back", 27, main), new Coord(210, 360));
        uis.pack();
    }

    private void initQuality() {
        final WidgetVerticalAppender appender = new WidgetVerticalAppender(withScrollport(quality, new Coord(620, 350)));
        appender.setVerticalMargin(VERTICAL_MARGIN);
        appender.setHorizontalMargin(HORIZONTAL_MARGIN);
        appender.add(new CheckBox("Show item quality") {
            {
                a = Config.showquality;
            }

            public void set(boolean val) {
                Utils.setprefb("showquality", val);
                Config.showquality = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Round item quality to a whole number") {
            {
                a = Config.qualitywhole;
            }

            public void set(boolean val) {
                Utils.setprefb("qualitywhole", val);
                Config.qualitywhole = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Draw background for quality values") {
            {
                a = Config.qualitybg;
            }

            public void set(boolean val) {
                Utils.setprefb("qualitybg", val);
                Config.qualitybg = val;
                a = val;
            }
        });
        appender.addRow(
            new Label("Background transparency (req. restart):"),
            new HSlider(200, 0, 255, Config.qualitybgtransparency) {
                public void changed() {
                    Utils.setprefi("qualitybgtransparency", val);
                    Config.qualitybgtransparency = val;
                }
            });

        quality.add(new PButton(200, "Back", 27, main), new Coord(210, 360));
        quality.pack();
    }

    private void initFlowermenus() {
        final WidgetVerticalAppender appender = new WidgetVerticalAppender(withScrollport(flowermenus, new Coord(620, 350)));

        appender.setVerticalMargin(VERTICAL_MARGIN);
        appender.setHorizontalMargin(HORIZONTAL_MARGIN);

        appender.add(new CheckBox("Automatically pick all clustered mussels (auto 'Pick' needs to be enabled)") {
            {
                a = Config.autopickmussels;
            }

            public void set(boolean val) {
                Utils.setprefb("autopickmussels", val);
                Config.autopickmussels = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Automatically pick all clustered gray clay (auto 'Pick' needs to be enabled)") {
            {
                a = Config.autopickclay;
            }

            public void set(boolean val) {
                Utils.setprefb("autopickclay", val);
                Config.autopickclay = val;
                a = val;
            }
        });
        appender.add(new Label("Automatic selecton:"));

        CheckListbox flowerlist = new CheckListbox(140, 17) {
            @Override
            protected void itemclick(CheckListboxItem itm, int button) {
                super.itemclick(itm, button);
                Utils.setprefchklst("flowersel", Config.flowermenus);
            }
        };

        Utils.loadprefchklist("flowersel", Config.flowermenus);
        for (CheckListboxItem itm : Config.flowermenus.values())
            flowerlist.items.add(itm);
        flowermenus.add(flowerlist, new Coord(0, 70));

        flowermenus.add(new PButton(200, "Back", 27, main), new Coord(210, 360));
        flowermenus.pack();
    }
    private void initstudydesksettings() {
        int x = 0;
        int y = 0, my = 0;

//        appender.add(new CheckBox("Enable Study Desk Alerts") {
//            {
//                a = Config.studydeskalerts;
//            }
//
//            public void set(boolean val) {
//                Utils.setprefb("studydeskalerts", val);
//                Config.studydeskalerts = val;
//                a = val;
//            }
//        });
        studydesksettings.add(new Label("Choose curios to check your studydesk for:"),x, y);
        y += 15;
        final CurioList list = studydesksettings.add(new CurioList(),x,y);

        y += list.sz.y + 5;
        final TextEntry value = studydesksettings.add(new TextEntry(150, "") {
            @Override
            public void activate(String text) {
                list.add(text);
                settext("");
            }
        }, x, y);

        studydesksettings.add(new Button(45, "Add") {
            @Override
            public void click() {
                list.add(value.text);
                value.settext("");
            }
        }, x + 155, y - 2);

        my = Math.max(my, y);

        studydesksettings.add(new PButton(200, "Back", 27, main), 0, my + 35);
        studydesksettings.pack();
    }
    private void initkeybindsettings() {
        WidgetList<KeyBinder.ShortcutWidget> list = keybindsettings.add(new WidgetList<KeyBinder.ShortcutWidget>(new Coord(300, 24), 16){
            @Override
            public boolean mousedown(Coord c0, int button) {
                boolean result = super.mousedown(c0, button);
                KeyBinder.ShortcutWidget item = itemat(c0);
                if(item != null) {
                    c0 = c0.add(0, sb.val * itemsz.y);
                    item.mousedown(c0.sub(item.parentpos(this)), button);
                }
                return result;
            }

            @Override
            public Object tooltip(Coord c0, Widget prev) {
                KeyBinder.ShortcutWidget item = itemat(c0);
                if(item != null) {
                    c0 = c0.add(0, sb.val * itemsz.y);
                    return item.tooltip(c0, prev);
                }
                return super.tooltip(c, prev);
            }
        });
        list.canselect = false;
        KeyBinder.makeWidgets(()->{
            for(int i = 0; i< list.listitems();i++){
                list.listitem(i).update();
            }
            return null;
        }).forEach(list::additem);




        keybindsettings.pack();
        keybindsettings.add(new PButton(200, "Back", 27, main), new Coord(410, 360));
        keybindsettings.pack();
    }

    private void initchatsettings() {
        final WidgetVerticalAppender appender = new WidgetVerticalAppender(withScrollport(chatsettings, new Coord(620, 310)));

        appender.setVerticalMargin(VERTICAL_MARGIN);
        appender.setHorizontalMargin(HORIZONTAL_MARGIN);

        appender.addRow(new Label("Enter Village name for Chat Alert sound, and village chat relay."),
                new TextEntry(150, Config.chatalert) {
                    @Override
                    public boolean type(char c, KeyEvent ev) {
                        if (!parent.visible)
                            return false;

                        boolean ret = buf.key(ev);
                        if (text.length() > 0) {
                            Utils.setpref("chatalert", text);
                        }

                        return ret;
                    }
                }
        );
        appender.addRow(new Label("Enter Discord Channel for Alerts to be sent to."),
                new TextEntry(150, Config.AlertChannel) {
                    @Override
                    public boolean type(char c, KeyEvent ev) {
                        if (!parent.visible)
                            return false;

                        boolean ret = buf.key(ev);
                        if (text.length() > 0) {
                            Utils.setpref("AlertChannel", text);
                            Config.AlertChannel = text;
                        }

                        return ret;
                    }
                }
        );
        appender.add(new CheckBox("Enable village chat alert sounds") {
            {
                a = Config.chatsounds;
            }

            public void set(boolean val) {
                Utils.setprefb("chatsounds", val);
                Config.chatsounds = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Enable discord chat alert sounds") {
            {
                a = Config.discordsounds;
            }

            public void set(boolean val) {
                Utils.setprefb("discordsounds", val);
                Config.discordsounds = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Enable public realm chat alert sounds") {
            {
                a = Config.realmchatalerts;
            }

            public void set(boolean val) {
                Utils.setprefb("realmchatalerts", val);
                Config.realmchatalerts = val;
                a = val;
            }
        });
        appender.addRow(new Label("Enter Discord Bot Key"),
                new TextEntry(475, Config.discordbotkey) {
                    @Override
                    public boolean type(char c, KeyEvent ev) {
                        if (!parent.visible)
                            return false;

                        boolean ret = buf.key(ev);
                        if (text.length() > 0) {
                            Utils.setpref("discordbotkey", text);
                            Config.discordbotkey = text;
                        }

                        return ret;
                    }
                }
        );
        appender.add(new CheckBox("Connect to Discord on Login") {
            {
                a = Config.autoconnectdiscord;
            }

            public void set(boolean val) {
                Utils.setprefb("autoconnectdiscord", val);
                Config.autoconnectdiscord = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Log village chat to Discord - Warning, best used if only one person is using on an alt."){
            {
                a = Config.discordchat;
            }

            public void set(boolean val) {
                Utils.setprefb("discordchat", val);
                Config.discordchat = val;
                a = val;
            }
        });
        appender.addRow(new Label("Enter Discord channel name for village chat output."),
                new TextEntry(150, Config.discordchannel) {
                    @Override
                    public boolean type(char c, KeyEvent ev) {
                        if (!parent.visible)
                            return false;

                        boolean ret = buf.key(ev);
                        if (text.length() > 0) {
                            Utils.setpref("discordchannel", text);
                            Config.discordchannel = text;
                        }

                        return ret;
                    }
                }
        );
        appender.add(new CheckBox("Connection to ArdZone Discord on login."){
            {
                a = Config.autoconnectarddiscord;
            }

            public void set(boolean val) {
                Utils.setprefb("autoconnectarddiscord", val);
                Config.autoconnectarddiscord = val;
                a = val;
            }
        });
       /* appender.addRow(new Label("Enter Discord Channel ID"),
                new TextEntry(150, Config.discordchannel) {
                    @Override
                    public boolean type(char c, KeyEvent ev) {
                        if (!parent.visible)
                            return false;

                        boolean ret = buf.key(ev);
                        if (text.length() > 0) {
                            Utils.setpref("discordchannel", text);
                        }

                        return ret;
                    }
                }
        );*/
        chatsettings.add(new PButton(200, "Back", 27, main), new Coord(210, 360));
        chatsettings.pack();
    }
    
    private void initHideMenu() {
        final WidgetVerticalAppender appender = new WidgetVerticalAppender(withScrollport(hidesettings, new Coord(620, 350)));

        appender.setVerticalMargin(VERTICAL_MARGIN);
        appender.setHorizontalMargin(HORIZONTAL_MARGIN);
        
        appender.add(new Label("Toggle hide by pressing ctrl + h"));
        appender.add(new CheckBox("Hide Cave Moths - Standalone Option, doesn't require toggling Hide.") {
            {
                a = Config.hidemoths;
            }

            public void set(boolean val) {
                Utils.setprefb("hidemoths", val);
                Config.hidemoths = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Hide trees") {
            {
                a = Config.hideTrees;
            }

            public void set(boolean val) {
                Utils.setprefb("hideTrees", val);
                Config.hideTrees = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Hide Tar Kilns") {
            {
                a = Config.hideTarKilns;
            }

            public void set(boolean val) {
                Utils.setprefb("hideTarKilns", val);
                Config.hideTarKilns = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Hide Smelters") {
            {
                a = Config.hideSmelters;
            }

            public void set(boolean val) {
                Utils.setprefb("hideSmelters", val);
                Config.hideSmelters = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Hide crops") {
            {
                a = Config.hideCrops;
            }

            public void set(boolean val) {
                Utils.setprefb("hideCrops", val);
                Config.hideCrops = val;
                a = val;
            }
        });
        
        appender.add(new CheckBox("Hide walls") {
            {
                a = Config.hideWalls;
            }

            public void set(boolean val) {
                Utils.setprefb("hideWalls", val);
                Config.hideWalls = val;
                a = val;
            }
        });

        appender.add(new CheckBox("Hide Animals") {
            {
                a = Config.hideanimals;
            }

            public void set(boolean val) {
                Utils.setprefb("hideanimals", val);
                Config.hideanimals = val;
                a = val;
            }
        });
        
        appender.add(new CheckBox("Hide wagons") {
            {
                a = Config.hideWagons;
            }

            public void set(boolean val) {
                Utils.setprefb("hideWagons", val);
                Config.hideWagons = val;
                a = val;
            }
        });
        
        appender.add(new CheckBox("Hide houses (Also doors)") {
            {
                a = Config.hideHouses;
            }

            public void set(boolean val) {
                Utils.setprefb("hideHouses", val);
                Config.hideHouses = val;
                a = val;
            }
        });
        
        appender.add(new CheckBox("Hide bushes") {
            {
                a = Config.hideBushes;
            }

            public void set(boolean val) {
                Utils.setprefb("hideBushes", val);
                Config.hideBushes = val;
                a = val;
            }
        });
        
        appender.add(new CheckBox("Hide drying frames") {
            {
                a = Config.hideDFrames;
            }

            public void set(boolean val) {
                Utils.setprefb("hideDFrames", val);
                Config.hideDFrames = val;
                a = val;
            }
        });
        
        appender.add(new CheckBox("Hide dream catchers") {
            {
                a = Config.hideDCatchers;
            }

            public void set(boolean val) {
                Utils.setprefb("hideDCatchers", val);
                Config.hideDCatchers = val;
                a = val;
            }
        });

        appender.add(new CheckBox("Draw colored overlay for hidden objects. Hide will need to be toggled if this setting is changed.") {
            {
                a = Config.showoverlay;
            }
            public void set(boolean val) {
                Utils.setprefb("showoverlay", val);
                Config.showoverlay = val;
                a = val;
            }
        });
        
        appender.add(new Label("Red"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.hidered);
            }

            public void changed() {
            	int vol = val;
                Config.hidered = vol;
                Utils.setprefi("hidered", vol);
                GobHitbox.fillclrstate = new States.ColState(new Color(Config.hidered, Config.hidegreen, Config.hideblue, 255));
            }
        });
        
        appender.setVerticalMargin(0);
        appender.add(new Label("Green"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.hidegreen);
            }

            public void changed() {
            	int vol = val;
                Config.hidegreen = vol;
                Utils.setprefi("hidegreen", vol);
                GobHitbox.fillclrstate = new States.ColState(new Color(Config.hidered, Config.hidegreen, Config.hideblue, 255));
            }
        });
        
        appender.setVerticalMargin(0);
        appender.add(new Label("Blue"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(150, 0, 255, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.hideblue);
                GobHitbox.fillclrstate = new States.ColState(new Color(Config.hidered, Config.hidegreen, Config.hideblue, 255));
            }

            public void changed() {
            	int vol = val;
                Config.hideblue = vol;
                Utils.setprefi("hideblue", vol);
                GobHitbox.fillclrstate = new States.ColState(new Color(Config.hidered, Config.hidegreen, Config.hideblue, 255));
            }
        });
        hidesettings.add(new PButton(200, "Back", 27, main), new Coord(210, 360));
        hidesettings.pack();
    }

    private void initSoundAlarms() {
        final WidgetVerticalAppender appender = new WidgetVerticalAppender(withScrollport(soundalarms, new Coord(620, 350)));

        appender.setVerticalMargin(VERTICAL_MARGIN);
        appender.setHorizontalMargin(HORIZONTAL_MARGIN);
        appender.add(new CheckBox("Ping on ant dungeon key drops.") {
            {
                a = Config.dungeonkeyalert;
            }

            public void set(boolean val) {
                Utils.setprefb("dungeonkeyalert", val);
                Config.dungeonkeyalert = val;
                a = val;
            }
        });
        appender.setVerticalMargin(0);
        appender.addRow(new Label("Forageable Alarm"), makeAlarmDropdownForagable());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int)(Config.alarmonforagablesvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alarmonforagablesvol = vol;
                Utils.setprefd("alarmonforagablesvol", vol);
            }
        });
        appender.addRow(new Label("Unknown Player Alarm"), makeAlarmDropdownUnknown());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int)(Config.alarmunknownvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alarmunknownvol = vol;
                Utils.setprefd("alarmunknownvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.addRow(new Label("Ant /Bat Dungeon Alarm"),makeAlarmDropdownDungeon());
        appender.addRow(new Label("Beaver Dungeon Alarm"),makeAlarmDropdownBeaverDungeon());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int)(Config.alarmdungeonvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alarmdungeonvol = vol;
                Utils.setprefd("alarmdungeonvol", vol);
            }
        });

        appender.setVerticalMargin(0);
        appender.addRow(new Label("Nidbane Alarm"), makeAlarmDropdownNidbane());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int)(Config.alarmnidbanevol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alarmnidbanevol = vol;
                Utils.setprefd("alarmnidbanevol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.addRow(new Label("Red Player Alarm"),makeAlarmDropdownRed());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.alarmredvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alarmredvol = vol;
                Utils.setprefd("alarmredvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.addRow(new Label("Adder Alarm"), makeAlarmDropdownAdder());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.alarmaddervol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alarmaddervol = vol;
                Utils.setprefd("alarmaddervol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.addRow(new Label("Lynx Alarm"), makeAlarmDropdownLynx());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.alarmlynxvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alarmlynxvol = vol;
                Utils.setprefd("alarmlynxvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.addRow(new Label("Walrus Alarm"), makeAlarmDropdownWalrus());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.alarmwalrusvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alarmwalrusvol = vol;
                Utils.setprefd("alarmwalrusvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.addRow(new Label("Seal Alarm"), makeAlarmDropdownSeal());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.alarmsealvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alarmsealvol = vol;
                Utils.setprefd("alarmsealvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.addRow(new Label("Mammoth Alarm"), makeAlarmDropdownMammoth());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.alarmmammothvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alarmmammothvol = vol;
                Utils.setprefd("alarmmammothvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.addRow(new Label("Eagle Alarm"), makeAlarmDropdownEagle());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.alarmeaglevol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alarmeaglevol = vol;
                Utils.setprefd("alarmeaglevol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.add(new CheckBox("Alarm on new private/party chat") {
            {
                a = Config.chatalarm;
            }

            public void set(boolean val) {
                Utils.setprefb("chatalarm", val);
                Config.chatalarm = val;
                a = val;
            }
        });
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.chatalarmvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.chatalarmvol = vol;
                Utils.setprefd("chatalarmvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.addRow(new Label("Study Finish Alarm"), makeAlarmDropdownStudy());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.studyalarmvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.studyalarmvol = vol;
                Utils.setprefd("studyalarmvol", vol);
            }
        });
        appender.addRow(new Label("Alarm on Trolls"), makeAlarmDropdownTroll());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.alarmtrollvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alarmtrollvol = vol;
                Utils.setprefd("alarmtrollvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.addRow(new Label("Catapult/Ram Alarm"), makeAlarmDropdownSiege());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.alarmbramvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alarmbramvol = vol;
                Utils.setprefd("alarmbramvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.addRow(new Label("Wrecking Ball Alarm"), makeAlarmDropdownWBall());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.alarmwballvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alarmwballvol = vol;
                Utils.setprefd("alarmwballvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.addRow(new Label("Bear Alarm"),makeAlarmDropdownBear());
        appender.setVerticalMargin(5);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                this.val = (int)(Config.alarmbearsvol * 1000.0D);
            }

            public void changed() {
                double vol = (double)this.val / 1000.0D;
                Config.alarmbearsvol = vol;
                Utils.setprefd("alarmbearsvol", vol);
            }
        });
        appender.setVerticalMargin(0);
        appender.addRow(new Label("Local Resource Alarm"),makeAlarmDropdownSwag());
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.alarmlocresvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alarmlocresvol = vol;
                Utils.setprefd("alarmlocresvol", vol);
            }
        });

        soundalarms.add(new Label("Alarm on"), new Coord(470, 0));
        CheckListbox itemslist = new CheckListbox(145, 18) {
            @Override
            protected void itemclick(CheckListboxItem itm, int button) {
                super.itemclick(itm, button);
                Utils.setprefchklst("alarmitems", Config.alarmitems);
            }
        };
        for (CheckListboxItem itm : Config.alarmitems.values())
            itemslist.items.add(itm);
        soundalarms.add(itemslist, new Coord(470, 15));
        soundalarms.add(new HSlider(145, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.alarmonforagablesvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alarmonforagablesvol = vol;
                Utils.setprefd("alarmonforagablesvol", vol);
            }
        }, new Coord(470, 340));

        soundalarms.add(new PButton(200, "Back", 27, main), new Coord(210, 360));
        soundalarms.pack();
    }

    private static final List<Integer> caveindust = Arrays.asList(1, 2, 5, 10, 15, 30, 45, 60, 120);
    private Dropbox<Integer> makeCaveInDropdown() {
        List<String> values = caveindust.stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<Integer>(9, values) {
            {
                super.change(null);
            }
            @Override
            protected Integer listitem(int i) {
                return caveindust.get(i);
            }

            @Override
            protected int listitems() {
                return caveindust.size();
            }

            @Override
            protected void drawitem(GOut g, Integer item, int i) {
                g.text(item.toString(), Coord.z);
            }

            @Override
            public void change(Integer item) {
                super.change(item);
                Config.caveinduration = item;
                Utils.setprefi("caveinduration", item);
            }
        };
    }


    private Dropbox<Locale> langDropdown() {
        List<Locale> languages = enumerateLanguages();
        List<String> values = languages.stream().map(x -> x.getDisplayName()).collect(Collectors.toList());
        return new Dropbox<Locale>(10, values) {
            {
                super.change(new Locale(Resource.language));
            }

            @Override
            protected Locale listitem(int i) {
                return languages.get(i);
            }

            @Override
            protected int listitems() {
                return languages.size();
            }

            @Override
            protected void drawitem(GOut g, Locale item, int i) {
                g.text(item.getDisplayName(), Coord.z);
            }

            @Override
            public void change(Locale item) {
                super.change(item);
                Utils.setpref("language", item.toString());
            }
        };
    }

    @SuppressWarnings("unchecked")
    private Dropbox<String> makeFontsDropdown() {
        final List<String> fonts = Arrays.asList(GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames());
        return new Dropbox<String>(8, fonts) {
            {
                super.change(Config.font);
            }

            @Override
            protected String listitem(int i) {
                return fonts.get(i);
            }

            @Override
            protected int listitems() {
                return fonts.size();
            }

            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }

            @Override
            public void change(String item) {
                super.change(item);
                Config.font = item;
                Utils.setpref("font", item);
            }
        };
    }

    private List<Locale> enumerateLanguages() {
        Set<Locale> languages = new HashSet<>();
        languages.add(new Locale("en"));

        Enumeration<URL> en;
        try {
            en = this.getClass().getClassLoader().getResources("l10n");
            if (en.hasMoreElements()) {
                URL url = en.nextElement();
                JarURLConnection urlcon = (JarURLConnection) (url.openConnection());
                try (JarFile jar = urlcon.getJarFile()) {
                    Enumeration<JarEntry> entries = jar.entries();
                    while (entries.hasMoreElements()) {
                        String name = entries.nextElement().getName();
                        // we assume that if tooltip localization exists then the rest exist as well
                        // up to dev to make sure that it's true
                        if (name.startsWith("l10n/" + Resource.BUNDLE_TOOLTIP))
                            languages.add(new Locale(name.substring(13, 15)));
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return new ArrayList<Locale>(languages);
    }

    private static final Pair[] combatkeys = new Pair[]{
            new Pair<>("[1-5] and [shift + 1-5]", 0),
            new Pair<>("[1-5] and [F1-F5]", 1),
            new Pair<>("[F1-F10]", 2)
    };

    @SuppressWarnings("unchecked")
    private Dropbox<Pair<String, Integer>> combatkeysDropdown() {
        List<String> values = Arrays.stream(combatkeys).map(x -> x.a.toString()).collect(Collectors.toList());
        Dropbox<Pair<String, Integer>> modes = new Dropbox<Pair<String, Integer>>(combatkeys.length, values) {
            @Override
            protected Pair<String, Integer> listitem(int i) {
                return combatkeys[i];
            }

            @Override
            protected int listitems() {
                return combatkeys.length;
            }

            @Override
            protected void drawitem(GOut g, Pair<String, Integer> item, int i) {
                g.text(item.a, Coord.z);
            }

            @Override
            public void change(Pair<String, Integer> item) {
                super.change(item);
                Config.combatkeys = item.b;
                Utils.setprefi("combatkeys", item.b);
            }
        };
        modes.change(combatkeys[Config.combatkeys]);
        return modes;
    }

    private static final List<Integer> fontSize = Arrays.asList(10, 11, 12, 13, 14, 15, 16);

    private Dropbox<Integer> makeFontSizeChatDropdown() {
        List<String> values = fontSize.stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<Integer>(fontSize.size(), values) {
            {
                change(Config.fontsizechat);
            }

            @Override
            protected Integer listitem(int i) {
                return fontSize.get(i);
            }

            @Override
            protected int listitems() {
                return fontSize.size();
            }

            @Override
            protected void drawitem(GOut g, Integer item, int i) {
                g.text(item.toString(), Coord.z);
            }

            @Override
            public void change(Integer item) {
                super.change(item);
                Config.fontsizechat = item;
                Utils.setprefi("fontsizechat", item);
            }
        };
    }

    private static final List<String> statSize = Arrays.asList("1", "2", "5", "10", "25", "50", "100", "200", "500", "1000");
    private Dropbox<String> makeStatGainDropdown() {
        return new Dropbox<String>(statSize.size(), statSize) {
            {
                super.change(Integer.toString(Config.statgainsize));
            }
            @Override
            protected String listitem(int i) {
                return statSize.get(i);
            }
            @Override
            protected int listitems() {
                return statSize.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.statgainsize = Integer.parseInt(item);
                Utils.setpref("statgainsize", item);
            }
        };
    }

    private static final List<String> afkTime = Arrays.asList("0","5","10","15","20","25","30","45","60");
    private Dropbox<String> makeafkTimeDropdown() {
        return new Dropbox<String>(afkTime.size(), afkTime) {
            {
                super.change(Integer.toString(Config.afklogouttime));
            }
            @Override
            protected String listitem(int i) {
                return afkTime.get(i);
            }
            @Override
            protected int listitems() {
                return afkTime.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.afklogouttime = Integer.parseInt(item);
                Utils.setpref("afklogouttime", item);
            }
        };
    }

    static private Scrollport.Scrollcont withScrollport(Widget widget, Coord sz) {
        final Scrollport scroll = new Scrollport(sz);
        widget.add(scroll, new Coord(0, 0));
        return scroll.cont;
    }

    public OptWnd() {
        this(true);
    }

    public void setMapSettings() {
        final String charname = gameui().chrid;

        CheckListbox boulderlist = new CheckListbox(140, 18) {
            @Override
            protected void itemclick(CheckListboxItem itm, int button) {
                super.itemclick(itm, button);
                Utils.setprefchklst("boulderssel_" + charname, Config.boulders);
            }
        };
        for (CheckListboxItem itm : Config.boulders.values())
            boulderlist.items.add(itm);
        map.add(boulderlist, new Coord(10, 15));

        CheckListbox bushlist = new CheckListbox(140, 18) {
            @Override
            protected void itemclick(CheckListboxItem itm, int button) {
                super.itemclick(itm, button);
                Utils.setprefchklst("bushessel_" + charname, Config.bushes);
            }
        };
        for (CheckListboxItem itm : Config.bushes.values())
            bushlist.items.add(itm);
        map.add(bushlist, new Coord(165, 15));

        CheckListbox treelist = new CheckListbox(140, 18) {
            @Override
            protected void itemclick(CheckListboxItem itm, int button) {
                super.itemclick(itm, button);
                Utils.setprefchklst("treessel_" + charname, Config.trees);
            }
        };
        for (CheckListboxItem itm : Config.trees.values())
            treelist.items.add(itm);
        map.add(treelist, new Coord(320, 15));

        CheckListbox iconslist = new CheckListbox(140, 18) {
            @Override
            protected void itemclick(CheckListboxItem itm, int button) {
                super.itemclick(itm, button);
                Utils.setprefchklst("iconssel_" + charname, Config.icons);
            }
        };
        for (CheckListboxItem itm : Config.icons.values())
            iconslist.items.add(itm);
        map.add(iconslist, new Coord(475, 15));

        map.add(new CheckBox("Show road Endpoints") {
            {
                a = Config.showroadendpoint;
            }

            public void set(boolean val) {
                Utils.setprefb("showroadendpoint", val);
                Config.showroadendpoint = val;
                a = val;
            }
        },165,340);

        map.add(new CheckBox("Show road Midpoints") {
            {
                a = Config.showroadmidpoint;
            }

            public void set(boolean val) {
                Utils.setprefb("showroadmidpoint", val);
                Config.showroadmidpoint = val;
                a = val;
            }
        },320,340);

        map.add(new CheckBox("Hide ALL (yes ALL) Icons") {
            {
                a = Config.hideallicons;
            }

            public void set(boolean val) {
                Utils.setprefb("hideallicons", val);
                Config.hideallicons = val;
                a = val;
            }
        },475,340);
        map.add(new PButton(100,"Clear Boulders", 27,clearboulders), new Coord(15,355));
        map.add(new PButton(100,"Clear Bushes", 27,clearbushes), new Coord(170,355));
        map.add(new PButton(100,"Clear Trees", 27,cleartrees), new Coord(325,355));
        map.add(new PButton(100,"Clear Hides", 27,clearhides), new Coord(480,355));


        map.pack();
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
        if ((sender == this) && (msg == "close")) {
            hide();
            if(ui.gui != null)
            setfocus(ui.gui.invwnd);
        } else {
            super.wdgmsg(sender, msg, args);
        }
    }

    public void show() {
        chpanel(main);
        super.show();
    }
    private void showChangeLog() {
        Window log = gameui().ui.root.add(new Window(new Coord(50, 50), "Changelog"), new Coord(100, 50));
        log.justclose = true;
        Textlog txt = log.add(new Textlog(new Coord(450, 500)));
        txt.quote = false;
        int maxlines = txt.maxLines = 200;
        log.pack();
        try {
            InputStream in = LoginScreen.class.getResourceAsStream("/changelog.txt");
            BufferedReader br = new BufferedReader(new InputStreamReader(in, "UTF-8"));
            File f = Config.getFile("changelog.txt");
            FileOutputStream out = new FileOutputStream(f);
            String strLine;
            int count = 0;
            while((count < maxlines) && (strLine = br.readLine()) != null) {
                txt.append(strLine);
                out.write((strLine + Config.LINE_SEPARATOR).getBytes());
                count++;
            }
            br.close();
            out.close();
            in.close();
        } catch(IOException ignored) {
        }
        txt.setprog(0);
    }

    private Dropbox<String> makeAlarmDropdownUnknown() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmunknownplayer);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmunknownplayer = item;
                Utils.setpref("alarmunknownplayer", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmunknownvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownRed() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmredplayer);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmredplayer = item;
                Utils.setpref("alarmredplayer", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmredvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownForagable() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmforagable);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmforagable = item;
                Utils.setpref("alarmforagable", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmonforagablesvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownBear() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmbear);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmbear = item;
                Utils.setpref("alarmbear", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmbearsvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownLynx() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmlynx);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmlynx = item;
                Utils.setpref("alarmlynx", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmbearsvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownAdder() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmadder);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmadder = item;
                Utils.setpref("alarmadder", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmaddervol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownWalrus() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmwalrus);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmwalrus = item;
                Utils.setpref("alarmwalrus", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmbearsvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownSeal() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmseal);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmseal = item;
                Utils.setpref("alarmseal", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmbearsvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownTroll() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmtroll);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmtroll = item;
                Utils.setpref("alarmtroll", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmtrollvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownMammoth() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmmammoth);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmmammoth = item;
                Utils.setpref("alarmmammoth", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmbearsvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownEagle() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmeagle);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmeagle = item;
                Utils.setpref("alarmeagle", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmbearsvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownDoomed() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmdoomed);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmdoomed = item;
                Utils.setpref("alarmdoomed", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmbramvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownWBall() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmwball);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmwball = item;
                Utils.setpref("alarmwball", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmwballvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownSwag() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmswag);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmswag = item;
                Utils.setpref("alarmswag", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmlocresvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownEyeball() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmeyeball);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmeyeball = item;
                Utils.setpref("alarmeyeball", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmeyeballvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownNidbane() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmnidbane);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmnidbane = item;
                Utils.setpref("alarmnidbane", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmnidbanevol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownDungeon() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmdungeon);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmdungeon = item;
                Utils.setpref("alarmdungeon", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmdungeonvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownBeaverDungeon() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmbeaverdungeon);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmbeaverdungeon = item;
                Utils.setpref("alarmbeaverdungeon", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmdungeonvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownSiege() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmsiege);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmsiege = item;
                Utils.setpref("alarmsiege", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmbramvol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownStudy() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmstudy);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.alarmstudy = item;
                Utils.setpref("alarmstudy", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.studyalarmvol);
            }
        };
    }
    private Dropbox<String> makeDropdownCleave() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.cleavesfx);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.cleavesfx = item;
                Utils.setpref("cleavesfx", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.cleavesoundvol);
            }
        };
    }
    private Dropbox<String> makeDropdownCombat() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.attackedsfx);
            }
            @Override
            protected String listitem(int i) {
                return alarms.get(i);
            }
            @Override
            protected int listitems() {
                return alarms.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                Config.attackedsfx = item;
                Utils.setpref("attackedsfx", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.attackedvol);
            }
        };
    }
}
