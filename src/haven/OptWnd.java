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
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;
import haven.resutil.BPRadSprite;
import haven.sloth.gfx.HitboxMesh;

import java.awt.Color;
import java.awt.GraphicsEnvironment;
import java.awt.event.KeyEvent;
import java.io.*;
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;
import java.util.function.Consumer;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.prefs.BackingStoreException;
import java.util.stream.Collectors;

import haven.sloth.gob.Movable;

import static haven.DefSettings.*;


public class OptWnd extends Window {
    public static final int VERTICAL_MARGIN = 10;
    public static final int HORIZONTAL_MARGIN = 5;
    private static final Text.Foundry fonttest = new Text.Foundry(Text.sans, 10).aa(true);
    public static final int VERTICAL_AUDIO_MARGIN = 5;
    public final Panel main, video, audio, display, map, general, combat, control, uis,uip, quality, flowermenus, soundalarms, hidesettings, studydesksettings, keybindsettings, chatsettings, clearboulders, clearbushes, cleartrees, clearhides;
    public Panel current;
    public CheckBox discordcheckbox;

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
                appender.add(new CheckBox("Add flared lip to top of ridges to make them obvious. (Requires restart)") {
                    {
                        a = Config.obviousridges;
                    }

                    public void set(boolean val) {
                        Utils.setprefb("obviousridges", val);
                        Config.obviousridges = val;
                        a = val;
                    }
                });
                appender.add(new CheckBox("Disable Animations (Big Performance Boost, makes some animations look weird.)") {
                    {
                        a = Config.disableAllAnimations;
                    }

                    public void set(boolean val) {
                        Utils.setprefb("disableAllAnimations", val);
                        Config.disableAllAnimations = val;
                        a = val;
                    }
                });
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
                appender.add(new CheckBox("Show skybox (Potential Performance Impact)") {
                    {
                        a = Config.skybox;
                    }

                    public void set(boolean val) {
                        Utils.setprefb("skybox", val);
                        Config.skybox = val;
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

    private Widget ColorPreWithLabel(final String text, final IndirSetting<Color> cl, final Consumer<Color> cb) {
        final Widget container = new Widget();
        final Label lbl = new Label(text);
        final IndirColorPreview pre = new IndirColorPreview(new Coord(16, 16), cl, cb);
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
                        PBotUtils.sysMsg("Discord Disconnected",Color.white);
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
                        PBotUtils.sysMsg("Not currently connected.",Color.white);
                }
            }, new Coord(210, 150));
            main.add(new Button(200, "Join Village Discord") {
                public void click() {
                    if(!gameui().discordconnected) {
                        if (Resource.getLocString(Resource.BUNDLE_LABEL, Config.discordbotkey) != null) {
                            new Thread(new Discord(PBotAPI.gui, "normal")).start();
                            gameui().discordconnected = true;
                        }
                        else
                            PBotUtils.sysMsg("No Key Detected, if there is one in chat settings you might need to relog.",Color.white);
                    }else if(gameui().discordconnected)
                        PBotUtils.sysMsg("Already connected.",Color.white);
                }
            }, new Coord(210, 180));
            main.add(new Button(200, "Join Ingame Discord") {
                public void click() {
                    if(gameui().discordconnected)
                        PBotUtils.sysMsg("Already Connected.",Color.white);
                    else {
                        new Thread(new Discord(PBotAPI.gui, "ard")).start();
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
        appender.add(new Label("Alerted gobs sound volume"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.alertsvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.alertsvol = vol;
                Utils.setprefd("alertsvol", vol);
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
        appender.add(new Label("'Ding' sound volume"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.sfxdingvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.sfxdingvol = vol;
                Utils.setprefd("sfxdingvol", vol);
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
        appender.add(new Label("Door close sound volume"));
        appender.setVerticalMargin(VERTICAL_AUDIO_MARGIN);
        appender.add(new HSlider(200, 0, 1000, 0) {
            protected void attach(UI ui) {
                super.attach(ui);
                val = (int) (Config.sfxdoorvol * 1000);
            }

            public void changed() {
                double vol = val / 1000.0;
                Config.sfxdoorvol = vol;
                Utils.setprefd("sfxdoorvol", vol);
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
        appender.add(new CheckBox("Flatten Palisades/Bwalls") {
            {
                a = Config.flatwalls;
            }

            public void set(boolean val) {
                Utils.setprefb("flatwalls", val);
                Config.flatwalls = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Flatten Cave Walls") {
            {
                a = Config.flatcaves;
            }

            public void set(boolean val) {
                Utils.setprefb("flatcaves", val);
                Config.flatcaves = val;
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
        appender.add(new CheckBox("Display Avatar Equipment tooltips.") {
            {
                a = Config.avatooltips;
            }

            public void set(boolean val) {
                Utils.setprefb("avatooltips", val);
                Config.avatooltips = val;
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
        appender.add(new IndirCheckBox("Toggle halo pointers", SHOWHALO));
        appender.add(new IndirCheckBox("Toggle halo pointers on hearthing", SHOWHALOONHEARTH));
        appender.add(new CheckBox("Show objects health - Useful for mine supports/boats") {
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
        appender.add(new IndirCheckBox("Show Your Movement Path", SHOWPLAYERPATH));
        appender.add(ColorPreWithLabel("Your path color: ", PLAYERPATHCOL, val ->{
            GobPath.clrst = new States.ColState(val);
        }));
        appender.add(new IndirCheckBox("Show Other Player Paths - Kinned player's paths will be their kin color.", SHOWGOBPATH));
        appender.add(ColorPreWithLabel("Unknown player path color: ", GOBPATHCOL, val ->{
            Movable.unknowngobcol = new States.ColState(val);
        }));
        appender.add(new IndirCheckBox("Show Mob Paths", SHOWANIMALPATH));
        appender.add(ColorPreWithLabel("Animal path color: ", ANIMALPATHCOL, val ->{
            Movable.animalpathcol = new States.ColState(val);
        }));
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
        appender.add(new CheckBox("Show animal radius") {
            {
                a = Config.showanimalrad;
            }

            public void set(boolean val) {
                Utils.setprefb("showanimalrad", val);
                Config.showanimalrad = val;
                a = val;
            }
        });
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
        appender.add(ColorPreWithLabel("Beehive radius color: ", BEEHIVECOLOR, val ->{
            BPRadSprite.smatBeehive = new States.ColState(val);
            GameUI gui = gameui();
            if (gui != null) {
                if (gui.map != null) {
                    MapView.rovlbeehive = new Gob.Overlay(new BPRadSprite(151.0F, -10.0F, BPRadSprite.smatBeehive));
                    gui.map.refreshGobsAll();
                }
            }
        }));
        appender.add(ColorPreWithLabel("Trough radius color: ", TROUGHCOLOR, val ->{
            BPRadSprite.smatTrough = new States.ColState(val);
            GameUI gui = gameui();
            if (gui != null) {
                if (gui.map != null) {
                    MapView.rovltrough = new Gob.Overlay(new BPRadSprite(200.0F, -10.0F, BPRadSprite.smatTrough));
                    gui.map.refreshGobsAll();
                }
            }
        }));
        appender.add(ColorPreWithLabel("Dangerous animal radius color: ", ANIMALDANGERCOLOR, val ->{
            BPRadSprite.smatDanger = new States.ColState(val);
            GameUI gui = gameui();
            if (gui != null) {
                if (gui.map != null) {
                    Gob.animalradius = new Gob.Overlay(new BPRadSprite(100.0F, -10.0F, BPRadSprite.smatDanger));
                    Gob.doubleanimalradius = new Gob.Overlay(new BPRadSprite(200.0F, -20.0F, BPRadSprite.smatDanger));
                    gui.map.refreshGobsAll();
                }
            }
        }));
        appender.add(ColorPreWithLabel("Mine support radius color: ", SUPPORTDANGERCOLOR, val ->{
            BPRadSprite.smatSupports = new States.ColState(val);
            GameUI gui = gameui();
            if (gui != null) {
                if (gui.map != null) {
                    MapView.rovlsupport = new Gob.Overlay(new BPRadSprite(100.0F, 0, BPRadSprite.smatSupports));
                    MapView.rovlcolumn = new Gob.Overlay(new BPRadSprite(125.0F, 0, BPRadSprite.smatSupports));
                    MapView.rovlbeam = new Gob.Overlay(new BPRadSprite(150.0F, 0, BPRadSprite.smatSupports));
                    gui.map.refreshGobsAll();
                }
            }
        }));
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
        appender.add(new CheckBox("Highlight cupboards based on amount of contents. Requires restart.") {
            {
                a = Config.showcupboardstatus;
            }

            public void set(boolean val) {
                Utils.setprefb("showcupboardstatus", val);
                Config.showdframestatus = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Highlight sheds based on amount of contents. Requires restart.") {
            {
                a = Config.showshedstatus;
            }

            public void set(boolean val) {
                Utils.setprefb("showshedstatus", val);
                Config.showshedstatus = val;
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
        appender.add(ColorPreWithLabel("Cheese rack missing color: ", CHEESERACKMISSINGCOLOR, val ->{
            BPRadSprite.cRackMissing = new Material.Colors(CHEESERACKMISSINGCOLOR.get());
        }));
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
        appender.add(ColorPreWithLabel("Garden Pot Finished Color (Requires restart)", GARDENPOTDONECOLOR));
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
        Button OutputSettings = new Button(220, "Output Light Settings to System Tab") {
            @Override
            public void click() {
                PBotUtils.sysLogAppend("Ambient Red "+DefSettings.NVAMBIENTCOL.get().getRed() + " Green - "+DefSettings.NVAMBIENTCOL.get().getGreen()+ " Blue - "+NVAMBIENTCOL.get().getBlue(),"white");
                PBotUtils.sysLogAppend("Diffuse Red "+DefSettings.NVDIFFUSECOL.get().getRed() + " Green - "+DefSettings.NVDIFFUSECOL.get().getGreen()+ " Blue - "+NVDIFFUSECOL.get().getBlue(),"white");
                PBotUtils.sysLogAppend("Specular Red "+DefSettings.NVSPECCOC.get().getRed() + " Green - "+DefSettings.NVSPECCOC.get().getGreen()+ " Blue - "+NVSPECCOC.get().getBlue(),"white");
            }
        };
        appender.add(OutputSettings);
        appender.add(new Label("Ghandhi Lighting Presets"));
        Button Preset1 = new Button(220, "Friday Evening") {
            @Override
            public void click() {
                DefSettings.NVAMBIENTCOL.set(new Color(51,59,119));
                DefSettings.NVDIFFUSECOL.set(new Color(20,28,127));
                DefSettings.NVSPECCOC.set(new Color(167,117,103));
            }
        };
        appender.add(Preset1);
        Button Preset2 = new Button(220, "Thieving Night") {
            @Override
            public void click() {
                DefSettings.NVAMBIENTCOL.set(new Color(5,10,51));
                DefSettings.NVDIFFUSECOL.set(new Color(0,31,50));
                DefSettings.NVSPECCOC.set(new Color(138,64,255));
            }
        };
        appender.add(Preset2);
        Button Preset3 = new Button(220, "Hunting Dusk") {
            @Override
            public void click() {
                DefSettings.NVAMBIENTCOL.set(new Color(165,213,255));
                DefSettings.NVDIFFUSECOL.set(new Color(160,193,255));
                DefSettings.NVSPECCOC.set(new Color(138,64,255));
            }
        };
        appender.add(Preset3);
        Button Preset4 = new Button(220, "Sunny Morning") {
            @Override
            public void click() {
                DefSettings.NVAMBIENTCOL.set(new Color(211,180,72));
                DefSettings.NVDIFFUSECOL.set(new Color(255,178,169));
                DefSettings.NVSPECCOC.set(new Color(255,255,255));
            }
        };
        appender.add(Preset4);
        appender.add(new Label("Default Lighting"));
        Button Preset5 = new Button(220, "Amber Default") {
            @Override
            public void click() {
                DefSettings.NVAMBIENTCOL.set(new Color(200,200,200));
                DefSettings.NVDIFFUSECOL.set(new Color(200,200,200));
                DefSettings.NVSPECCOC.set(new Color(255,255,255));
            }
        };
        appender.add(Preset5);
        appender.add(new IndirCheckBox("Dark Mode (overrides custom global light)", DARKMODE));
        appender.add(ColorPreWithLabel("Ambient Color", NVAMBIENTCOL));
        appender.add(ColorPreWithLabel("Diffuse Color", NVDIFFUSECOL));
        appender.add(ColorPreWithLabel("Specular Color", NVSPECCOC));

    }

    private void initMap() {
        map.add(new Label("Show boulders:"), new Coord(10, 0));
        map.add(new Label("Show bushes:"), new Coord(165, 0));
        map.add(new Label("Show trees:"), new Coord(320, 0));
        map.add(new Label("Hide icons:"), new Coord(475, 0));

        map.add(new CheckBox("Draw party members/names") {
            {
                a = Config.mapdrawparty;
            }

            public void set(boolean val) {
                Utils.setprefb("mapdrawparty", val);
                Config.mapdrawparty = val;
                a = val;
            }
        },420,380);

        map.add(new CheckBox("Show names above questgivers.") {
            {
                a = Config.mapdrawquests;
            }

            public void set(boolean val) {
                Utils.setprefb("mapdrawquests", val);
                Config.mapdrawquests = val;
                a = val;
            }
        },0,380);

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
        appender.add(new CheckBox("Disable swimming automatically after 30 seconds.") {
            {
                a = Config.temporaryswimming;
            }

            public void set(boolean val) {
                Utils.setprefb("temporaryswimming", val);
                Config.temporaryswimming = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Auto hearth on unknown/red players") {
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
        appender.add(new CheckBox("Auto remove damaged tableware items") {
            {
                a = Config.savecutlery;
            }

            public void set(boolean val) {
                Utils.setprefb("savecutlery", val);
                Config.savecutlery = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Autodrink below threshold") {
            {
                a = Config.autodrink;
            }

            public void set(boolean val) {
                Utils.setprefb("autodrink", val);
                Config.autodrink = val;
                a = val;
            }
        });
        Label AutodrinkThreshold;
        AutodrinkThreshold = new Label("Autodrink Threshold: "+Config.autodrinkthreshold);
        appender.add(AutodrinkThreshold);
        appender.add(new HSlider(130, 0, 100, Config.autodrinkthreshold) {
            public void added() {
                updateLabel();
            }

            protected void attach(UI ui) {
                super.attach(ui);
                val = (Config.autodrinkthreshold);
            }

            public void changed() {
                Utils.setprefi("autodrinkthreshold", val);
                Config.autodrinkthreshold = val;
                updateLabel();
            }

            private void updateLabel() {
                AutodrinkThreshold.settext(String.format("Autodrink Threshold : %d Percent", val));
            }
        });
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
        appender.add(new CheckBox("Drop soil") {
            {
                a = Config.dropSoil;
            }

            public void set(boolean val) {
                Utils.setprefb("dropSoil", val);
                Config.dropSoil = val;
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
        appender.add(new CheckBox("Automatically drop Cattail Heads/Roots (Only keep fibres)") {
            {
                a = Config.DropCattails;
            }

            public void set(boolean val) {
                Utils.setprefb("DropCattails", val);
                Config.DropCattails = val;
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
        appender.add(new Label("Chat Exempt will force the fight session to have focus unless the chat box has focus."));
        appender.add(new CheckBox("Force Fight Session Focus - Chat Exempt") {
            {
                a = Config.forcefightfocus;
            }

            public void set(boolean val) {
                Utils.setprefb("forcefightfocus", val);
                Config.forcefightfocus = val;
                a = val;
            }
        });
        appender.add(new Label("Chat Included will force fight session to have focus at all times, this will prevent talking in combat."));
        appender.add(new CheckBox("Force Fight Session Focus - Chat Included") {
            {
                a = Config.forcefightfocusharsh;
            }

            public void set(boolean val) {
                Utils.setprefb("forcefightfocusharsh", val);
                Config.forcefightfocusharsh = val;
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
        appender.add(new CheckBox("Disable pick forage keybind (Q by Default) opening/closing gates.") {
            {
                a = Config.disablegatekeybind;
            }

            public void set(boolean val) {
                Utils.setprefb("disablegatekeybind", val);
                Config.disablegatekeybind = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Disable pick forage keybind (Q by Default) picking up/dropping carts.") {
            {
                a = Config.disablecartkeybind;
            }

            public void set(boolean val) {
                Utils.setprefb("disablecartkeybind", val);
                Config.disablecartkeybind = val;
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
        appender.add(new CheckBox("Disable menugrid magic hotkeys") {
            {
                a = Config.disablemagaicmenugrid;
            }

            public void set(boolean val) {
                Utils.setprefb("disablemagaicmenugrid", val);
                Config.disablemagaicmenugrid = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Show PBot Menugrid icon (Requires relog)") {
            {
                a = Config.showPBot;
            }

            public void set(boolean val) {
                Utils.setprefb("showPBot", val);
                Config.showPBot = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Show Old PBot Menugrid icon (Requires relog)") {
            {
                a = Config.showPBotOld;
            }

            public void set(boolean val) {
                Utils.setprefb("showPBotOld", val);
                Config.showPBotOld = val;
                a = val;
            }
        });
        appender.add(new CheckBox("Detailed Shift+Mouseover tooltips - Negative FPS Impact when holding shift.") {
            {
                a = Config.detailedresinfo;
            }

            public void set(boolean val) {
                Utils.setprefb("detailedresinfo", val);
                Config.detailedresinfo = val;
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
        appender.add(new IndirCheckBox("Amber flowermenus", AMBERMENU));
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
        appender.add(new CheckBox("Stack cupboard windows on top of eachother") {
            {
                a = Config.stackwindows;
            }

            public void set(boolean val) {
                Utils.setprefb("stackwindows", val);
                Config.stackwindows = val;
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
        appender.add(new CheckBox("Hide quests panel on login") {
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

        flowermenus.add(new Label("Autopick Clusters:"), new Coord(150,0));
        CheckListbox clusterlist = new CheckListbox(140, 17) {
            @Override
            protected void itemclick(CheckListboxItem itm, int button) {
                super.itemclick(itm, button);
                Utils.setprefchklst("clustersel", Config.autoclusters);
            }
        };
        Utils.loadprefchklist("clustersel", Config.autoclusters);
        clusterlist.items.addAll(Config.autoclusters.values());
        flowermenus.add(clusterlist, new Coord(150, 20));


        flowermenus.add(new Label("Automatic selecton:"), new Coord(0,0));
        CheckListbox flowerlist = new CheckListbox(140, 17) {
            @Override
            protected void itemclick(CheckListboxItem itm, int button) {
                super.itemclick(itm, button);
                Utils.setprefchklst("flowersel", Config.flowermenus);
            }
        };
        Utils.loadprefchklist("flowersel", Config.flowermenus);
        flowerlist.items.addAll(Config.flowermenus.values());
        flowermenus.add(flowerlist, new Coord(0, 20));

        flowermenus.add(new PButton(200, "Back", 27, main), new Coord(210, 360));
        flowermenus.pack();
    }
    private void initstudydesksettings() {
        int x = 0;
        int y = 0, my = 0;
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
                            Config.chatalert = text;
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
        discordcheckbox = new CheckBox("Log village chat to Discord - Warning, best used if only one person is using on an alt."){
            {
                a = Config.discordchat;
            }

            public void set(boolean val) {
                final String charname = gameui().chrid;
                Utils.setprefb("discordchat_"+charname, val);
                Config.discordchat = val;
                a = val;
            }
        };
        appender.add(discordcheckbox);
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
        
        appender.add(new Label("Toggle bulk hide by pressing the keybind you assign in Keybind Settings"));
        appender.add(new Label("These hides are for all objects of this type, to hide individual ones instead please utilize the alt + right click menu."));
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
        appender.add(new CheckBox("Hide boulders") {
            {
                a = Config.hideboulders;
            }

            public void set(boolean val) {
                Utils.setprefb("hideboulders", val);
                Config.hideboulders = val;
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
        appender.add(new CheckBox("Draw colored overlay for hidden objects. Hide will need to be toggled") {
            {
                a = Config.showoverlay;
            }
            public void set(boolean val) {
                Utils.setprefb("showoverlay", val);
                Config.showoverlay = val;
                a = val;
            }
        });
        appender.add(ColorPreWithLabel("Hidden/Hitbox color: ", HIDDENCOLOR, val ->{
            GobHitbox.fillclrstate = new States.ColState(val);
            HitboxMesh.updateColor(new States.ColState(val));
                    if(ui.sess != null) {
                        ui.sess.glob.oc.changeAllGobs();
                    }
    }));
        appender.add(new Button(200, "New Hidden System", false) {
            public void click() {
                GameUI gui = gameui();
                if(gui != null)
                    gui.toggleHidden();
            }
        });
        appender.add(new Button(200, "New Deleted System", false) {
            public void click() {
                GameUI gui = gameui();
                if(gui != null)
                    gui.toggleDeleted();
            }
        });
        appender.setVerticalMargin(0);
        hidesettings.add(new PButton(200, "Back", 27, main), new Coord(210, 360));
        hidesettings.pack();
    }

    private void initSoundAlarms() {
        final WidgetVerticalAppender appender = new WidgetVerticalAppender(withScrollport(soundalarms, new Coord(620, 350)));
        appender.add(new Label("Individual alarms are now set by alt+right clicking an object, or navigating to the alert menu and adding manually."));
        appender.add(new Label("The alert menu can be found by navigating through the bottom right menugrid using 'Game Windows'"));
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
        appender.add(new Button(200, "New Alerts System", false) {
            public void click() {
                GameUI gui = gameui();
                if(gui != null)
                    gui.toggleAlerted();
            }
        });
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

    private Dropbox<String> makeAlarmDropdownHorse() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmhorse);
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
                Config.alarmhorse = item;
                Utils.setpref("alarmhorse", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmhorsevol);
            }
        };
    }

    private Dropbox<String> makeAlarmDropdownMoose() {
        final List<String> alarms = Config.alarms.values().stream().map(x -> x.toString()).collect(Collectors.toList());
        return new Dropbox<String>(Config.alarms.size(), alarms) {
            {
                super.change(Config.alarmmoose);
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
                Config.alarmmoose = item;
                Utils.setpref("alarmmoose", item);
                if(!item.equals("None"))
                    Audio.play(Resource.local().loadwait(item),Config.alarmmoosevol);
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
