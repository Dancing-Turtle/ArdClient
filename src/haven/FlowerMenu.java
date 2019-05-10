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


import java.awt.*;
import java.util.ConcurrentModificationException;
import java.util.function.Consumer;

import com.jogamp.opengl.util.packrect.LevelSet;
import haven.DefSettings;
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;

import static haven.DefSettings.AMBERMENU;
import static haven.DefSettings.QUICKERMENU;
import static java.lang.Math.PI;
import haven.DefSettings;
import haven.sloth.gob.HeldBy;

public class FlowerMenu extends Widget {
    public static final Color pink = new Color(255, 0, 128);
    public static final Color ptc = Color.YELLOW;
    public static final Text.Foundry ptf = new Text.Foundry(Text.dfont, 12);
    public static final Coord customBoxPadding = new Coord(4, 4);
    public static final IBox pbox = Window.wbox;
    public static final Tex pbg = Window.bg;
    public static final int ph = 30, ppl = 8;
    private static final int HORSE_DELAY = 1;
    private static final int TIMEOUT = 5000;
    public Petal[] opts;
    private UI.Grab mg, kg;
    private static String nextAutoSel;
    private static long nextAutoSelTimeout;
    public static String lastSel;
    public Thread horsemounter;
    private final Consumer<Integer> callback;
    private boolean selected = false;
    private boolean ignoreAutoSetting;

    @RName("sm")
    public static class $_ implements Factory {
        public Widget create(UI ui, Object[] args) {
            String[] opts = new String[args.length];
            for (int i = 0; i < args.length; i++)
                opts[i] = (String) args[i];
            return (new FlowerMenu(opts));
        }
    }

    public class Petal extends Widget {
        public String name;
        private boolean h;
        private boolean clicked;
        public double ta, tr;
        private final static double rad = 75;
        public int num;
        public Text text;
        public double a = 1;

        public Petal(String name) {
            super(Coord.z);
            this.name = name;
            text = ptf.render(Resource.getLocString(Resource.BUNDLE_FLOWER, name), Color.YELLOW);
            resize(text.sz().x + 25, ph);
        }

        public void move(Coord c) {
            this.c = c.sub(sz.div(2));
        }

        public void move(double a, double r) {
            if (AMBERMENU.get()) {
                this.c = Coord.sc(a, r).sub(sz.div(2));
                // adjust horizontal position for potentially parallel petals to avoid overlap
                if (r == rad) {
                    for (Petal p : opts) {
                        if (this.c.x + sz.x >= p.c.x &&
                                (num == 7 && p.num == 1 || num == 6 && p.num == 2 || num == 5 && p.num == 3)) {
                            p.c.x = opts[0].c.x + opts[0].sz.x / 2 + 5;
                            this.c.x = p.c.x - sz.x - 5;
                            break;
                        }
                    }
                }
            } else
                move(Coord.sc(a, r));
        }

        public void draw(GOut g) {
            g.chcolor(new Color(DefSettings.WNDCOL.get().getRed(), DefSettings.WNDCOL.get().getGreen(), DefSettings.WNDCOL.get().getBlue(), (int) (255 * a)));
            g.image(pbg, new Coord(3, 3), new Coord(3, 3), sz.add(new Coord(-6, -6)));

            if (h) {
                g.chcolor(0, 0, 0, (int) (128 * a));
                g.frect(Coord.z, sz);
                g.chcolor(new Color(255, 255, 255, (int) (255 * a)));
            }
            // pbg is to short for wide petals
            if (pbg.sz().x < sz.x && AMBERMENU.get())
                g.image(pbg, new Coord(pbg.sz().x, 3), new Coord(3, 3), sz.add(new Coord(-6, -6)));
            pbox.draw(g, Coord.z, sz);
            g.image(text.tex(), sz.div(2).sub(text.sz().div(2)));
        }

        public boolean mousedown(Coord c, int button) {
            if(!selected)
               choose(this);
            selected = true;
            return (true);
        }

        @Override
        public void mousemove(Coord c) {
            h = c.isect(Coord.z, sz.sub(1, 1));
        }
    }

    private static double nxf(double a) {
        return (-1.8633 * a * a + 2.8633 * a);
    }

    public class Opening extends NormAnim {
        Opening() {
            super(0);
        }

        public void ntick(double s) {
            for (Petal p : opts) {
                p.move(p.ta + ((1 - s) * PI), p.rad * s);
                p.a = s;
                if (s == 1.0) {
                    CheckListboxItem itm = Config.flowermenus.get(p.name);
                    if (itm != null && itm.selected && !ui.modctrl && (!ignoreAutoSetting || p.name.equals("Peer into")) ||
                            p.name.equals(nextAutoSel) && System.currentTimeMillis() - nextAutoSelTimeout < 2000) {
                        nextAutoSel = null;
                        try {
                            if (p.name.equals("Cargo") && PBotAPI.gui.map.player() != null && PBotAPI.gui.map.player().getattr(HeldBy.class) != null) {
                                PBotAPI.gui.ui.root.wdgmsg("gk", 27);
                            }
                        }catch(Exception e){e.printStackTrace();}
                        choose(p);
                        try {
                            if (p.name.contains("Giddy") && Config.horseautorun) {
                                    horsemounter = new Thread(new FlowerMenu.horsemounter());
                                    horsemounter.start();
                            }
                        }catch(Exception e){e.printStackTrace();}
                        break;
                    }
                }
            }
        }
    }

    public class horsemounter implements Runnable {
        public void run() {
            try {
                GameUI gui = getGUI();
                int timeout = 0;
               while(gui.map.player() != null && gui.map.player().getattr(HeldBy.class) == null){
                   timeout++;
                   if(timeout > 1000) {//if we timeout, break horsemounter
                       return;
                   }
                   PBotUtils.sleep(10);
               }
               if(PBotAPI.gui.map.player() == null){
                   //player now null, exit.
                   return;
               }
                Speedget speedwdg = gui.speedget.get();
                if(speedwdg != null)
                    speedwdg.set(2);
            } catch (Exception e) { }
        }
    }

    public GameUI getGUI() {
        return HavenPanel.lui.root.findchild(GameUI.class);
    }

    public class Chosen extends NormAnim {
        Petal chosen;
        Chosen(Petal c) {
            super(0.75);
            chosen = c;
            selected = true;
        }

        public void ntick(double s) {
            if (AMBERMENU.get()) {
                for (Petal p : opts) {
                    if (p == chosen) {
                        if (s > 0.6) {
                            p.a = 1 - ((s - 0.6) / 0.4);
                        } else if (s < 0.3) {
                            p.move(p.ta, p.rad * (1 - (s / 0.3)));
                        }
                    } else {
                        p.destroy();
                      /*  if (s > 0.3)
                            p.a = 0;
                        else
                            p.a = 1 - (s / 0.3);*/
                    }
                }
            } else {
                double ival = 0.8;
                double off = ((1.0 - ival) / (opts.length - 1));
                for (int i = 0; i < opts.length; i++) {
                    Petal p = opts[i];
                    if (p == chosen) {
                        if (s > 0.6) {
                            p.a = 1 - ((s - 0.6) / 0.4);
                        } else if (s < 0.3) {
                            double a = nxf(s / 0.3);
                            p.move(p.ta, p.tr * (1 - a));
                        }
                    } else {
                        p.destroy();
                      /*  if (s > 0.3) {
                            p.a = 0;
                        } else {
                            double a = s / 0.3;
                            a = Utils.clip((a - (off * i)) * (1.0 / ival), 0, 1);
                            p.a = 1 - a;
                        }*/
                    }
                }
            }
                if (s == 1.0)
                    ui.destroy(FlowerMenu.this);
            }
        }


        public class Cancel extends NormAnim {
            Cancel() {
                super(0);
            }

            public void ntick(double s) {
                if (AMBERMENU.get()) {
                    for (Petal p : opts) {
                        p.move(p.ta + ((s) * PI), p.rad * (1 - s));
                        p.a = 1 - s;
                    }
                } else {
                    double ival = 0.8;
                    double off = (opts.length == 1) ? 0.0 : ((1.0 - ival) / (opts.length - 1));
                    for (int i = 0; i < opts.length; i++) {
                        Petal p = opts[i];
                        double a = Utils.clip((s - (off * i)) * (1.0 / ival), 0, 1);
                        double b = 1.0 - nxf(1.0 - a);
                        p.move(p.ta + (b * PI), p.tr * (1 - b));
                        p.a = 1 - a;
                    }
                }
                    if (s == 1.0)
                        ui.destroy(FlowerMenu.this);
                }
            }


        public class CustomPetal extends Petal {
            boolean h = false;

            private CustomPetal(String name) {
                super(name);
                sz = new Coord(text.sz().x + 35, 30);
            }

            @Override
            public void draw(GOut g) {
                g.chcolor(new Color(DefSettings.WNDCOL.get().getRed(), DefSettings.WNDCOL.get().getGreen(), DefSettings.WNDCOL.get().getBlue(), (int) (255 * a)));
                Coord bgc = new Coord();
                for (bgc.y = 0; bgc.y < sz.y; bgc.y += pbg.sz().y) {
                    for (bgc.x = 0; bgc.x < sz.x; bgc.x += pbg.sz().x)
                        g.image(pbg, bgc, Coord.z, sz);
                }
                g.chcolor();
                //
                if (h) {
                    g.chcolor(0, 0, 0, (int) (128 * a));
                    g.frect(Coord.z, sz);
                    g.chcolor(new Color(255, 255, 255, (int) (255 * a)));
                }
                FastText.print(g, new Coord(10, 7), Integer.toString((num + 1) % 10));
                g.image(text.tex(), sz.sub(8, 0).sub(text.sz()).div(2).add(8, 0));
                g.chcolor();
            }

            @Override
            public void move(double a, double r) { }

            @Override
            public void mousemove(Coord c) {
                h = c.isect(Coord.z, sz.sub(1, 1));
            }
        }

        private void organize(Petal[] opts) {
            if (AMBERMENU.get()) {
                for (int i = 0; i < opts.length; i++) {
                    double ta = PI / 2 - i * PI / 4;

                    // slightly adjust 45 degrees angles
                    if (ta == PI / 4 || ta == -3 * PI / 4)
                        ta -= 0.25;
                    if (ta == -PI / 4 || ta == -5 * PI / 4)
                        ta += 0.25;

                    opts[i].ta = ta;
                }
            } else {
                int width = 80;
                for (Petal petal : opts)
                    width = Math.max(width, petal.sz.x);
                Coord c = new Coord(customBoxPadding);
                for (Petal petal : opts) {
                    petal.c = new Coord(c);
                    petal.resize(width, petal.sz.y);
                    c.y += petal.sz.y - 1;
                }
            }
            pack();
            // clip to parent
            int x = Utils.clip(this.c.x, 0, parent.sz.x - sz.x);
            int y = Utils.clip(this.c.y, 0, parent.sz.y - sz.y);
            this.c = new Coord(x, y);
        }

        public FlowerMenu(final Consumer<Integer> callback, final String... options) {
            super(Coord.z);
            this.callback = callback;
            opts = new Petal[options.length];

            for (int i = 0; i < options.length; i++) {
                if (AMBERMENU.get()) {
                    add(opts[i] = new Petal(options[i]));
                }
                else {
                    add(opts[i] = new CustomPetal(options[i]));
                }
                    opts[i].num = i;
                    if (options[i].equals("Study") || options[i].equals("Turn"))    // eatable curios & spitroasting
                        ignoreAutoSetting = true;
            }
        }

        public FlowerMenu(String... options) {
            this(null, options);
        }



        protected void added() {
            if (c.equals(-1, -1))
                c = parent.ui.lcc;
            mg = ui.grabmouse(this);
            kg = ui.grabkeys(this);
            organize(opts);
            new Opening();
        }

        public boolean mousedown(Coord c, int button) {
            if(selected)
                return false;
            if (!anims.isEmpty())
                return (true);
            if (!super.mousedown(c, button))
                choose(null);
            return (true);
        }

        public void uimsg(String msg, Object... args) {
            switch (msg) {
                case "cancel":
                    new Cancel();
                    mg.remove();
                    kg.remove();
                    break;
                case "act":
                    new Chosen(opts[(Integer) args[0]]);
                    mg.remove();
                    kg.remove();
                    break;
            }
        }

        public void draw(GOut g) {
            super.draw(g, false);
        }

        public boolean keydown(java.awt.event.KeyEvent ev) {
            return (true);
        }

        public boolean type(char key, java.awt.event.KeyEvent ev) {
            if (Config.userazerty)
                key = Utils.azerty2qwerty(key);

            if ((key >= '0') && (key <= '9')) {
                int opt = (key == '0') ? 10 : (key - '1');
                if (opt < opts.length) {
                    choose(opts[opt]);
                    kg.remove();
                }
                return (true);
            } else if (key == 27) {
                choose(null);
                kg.remove();
                return (true);
            }
            return (false);
        }

        public void choose(Petal option) {
            if (callback == null) {
                selected = true;
                if (option == null) {
                    wdgmsg("cl", -1);
                    lastSel = null;
                } else {
                    wdgmsg("cl", option.num, ui.modflags());
                    lastSel = option.name;
                    MapView.pllastcc = null;
                }
            } else {
                callback.accept(option != null ? option.num : -1);
                ui.destroy(this);
            }
        }

        public static void setNextSelection(String name) {
            nextAutoSel = name;
            nextAutoSelTimeout = System.currentTimeMillis();
        }
    }


