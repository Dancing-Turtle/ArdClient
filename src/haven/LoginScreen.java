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

import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;

import java.awt.Color;
import java.awt.Desktop;
import java.awt.event.KeyEvent;
import java.io.*;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Scanner;

public class LoginScreen extends Widget {
	boolean serverStatus;
    Login cur;
    Text error;
    IButton btn;
    Button statusbtn;
    Button optbtn;
    private TextEntry user;
    OptWnd opts;
    static Text.Foundry textf, textfs, special;
    static Tex bg = Resource.loadtex("gfx/loginscr");
    Text progress = null;
    private Window log;

    static {
        textf = new Text.Foundry(Text.sans, 16).aa(true);
        textfs = new Text.Foundry(Text.sans, 14).aa(true);
        special = new Text.Foundry(Text.latin, 14).aa(true);
    }

    public LoginScreen() {
        super(bg.sz());
        setfocustab(true);
        add(new Img(bg), Coord.z);
        optbtn = adda(new Button(100, "Options"), sz.x-110, 40, 0, 1);
       // new UpdateChecker().start();
        add(new LoginList(200, 29), new Coord(10, 10));
        statusbtn = adda(new Button(200, "Initializing..."), sz.x-210, 80, 0, 1);
        StartUpdaterThread();
        GameUI.swimon = false;
        GameUI.trackon = false;
        GameUI.crimeon = false;
    }

    private void showChangeLog() {
        log = ui.root.add(new Window(new Coord(50, 50), "Changelog"), new Coord(100, 50));
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

    private static abstract class Login extends Widget {
        abstract Object[] data();

        abstract boolean enter();
    }

    private class Pwbox extends Login {
        TextEntry  pass;

        private Pwbox(String username, boolean save) {
            setfocustab(true);
            add(new Label("User name", textf), Coord.z);
            add(user = new TextEntry(150, username), new Coord(0, 20));
            add(new Label("Password", textf), new Coord(0, 50));
            add(pass = new TextEntry(150, ""), new Coord(0, 70));
            pass.pw = true;
            if (user.text.equals(""))
                setfocus(user);
            else
                setfocus(pass);
            resize(new Coord(150, 150));
            LoginScreen.this.add(this, new Coord(345, 310));
        }

        public void wdgmsg(Widget sender, String name, Object... args) {
        }

        Object[] data() {
            return (new Object[]{new AuthClient.NativeCred(user.text, pass.text), false});
        }

        boolean enter() {
            if (user.text.equals("")) {
                setfocus(user);
                return (false);
            } else if (pass.text.equals("")) {
                setfocus(pass);
                return (false);
            } else {
                return (true);
            }
        }

        public boolean globtype(char k, KeyEvent ev) {
            if ((k == 'r') && ((ev.getModifiersEx() & (KeyEvent.META_DOWN_MASK | KeyEvent.ALT_DOWN_MASK)) != 0)) {
                return (true);
            }
            return (false);
        }
    }

    private class Tokenbox extends Login {
        Text label;
        Button btn;

        private Tokenbox(String username) {
            label = textfs.render("Identity is saved for " + username, java.awt.Color.WHITE);
            add(btn = new Button(100, "Forget me"), new Coord(75, 30));
            resize(new Coord(250, 100));
            LoginScreen.this.add(this, new Coord(295, 330));
        }

        Object[] data() {
            return (new Object[0]);
        }

        boolean enter() {
            return (true);
        }

        public void wdgmsg(Widget sender, String name, Object... args) {
            if (sender == btn) {
                LoginScreen.this.wdgmsg("forget");
                return;
            }
            super.wdgmsg(sender, name, args);
        }

        public void draw(GOut g) {
            g.image(label.tex(), new Coord((sz.x / 2) - (label.sz().x / 2), 0));
            super.draw(g);
        }

        public boolean globtype(char k, KeyEvent ev) {
            if ((k == 'f') && ((ev.getModifiersEx() & (KeyEvent.META_DOWN_MASK | KeyEvent.ALT_DOWN_MASK)) != 0)) {
                LoginScreen.this.wdgmsg("forget");
                return (true);
            }
            return (false);
        }
    }

    public class LoginList extends Listbox<LoginData> {
        private final Tex xicon = Text.render("\u2716", Color.RED, special).tex();
        private int hover = -1;
        private final static int ITEM_HEIGHT = 20;
        private Coord lastMouseDown = Coord.z;

        public LoginList(int w, int h) {
            super(w, h, ITEM_HEIGHT);
        }

        @Override
        protected void drawbg(GOut g) {
            g.chcolor(0, 0, 0, 120);
            g.frect(Coord.z, sz);
            g.chcolor();
        }

        @Override
        protected void drawsel(GOut g) {
        }

        @Override
        protected LoginData listitem(int i) {
            synchronized (Config.logins) {
                return Config.logins.get(i);
            }
        }

        @Override
        protected int listitems() {
            synchronized (Config.logins) {
                return Config.logins.size();
            }
        }

        @Override
        public void mousemove(Coord c) {
            setHoverItem(c);
            super.mousemove(c);
        }

        @Override
        public boolean mousewheel(Coord c, int amount) {
            setHoverItem(c);
            return super.mousewheel(c, amount);
        }

        private void setHoverItem(Coord c) {
            if (c.x > 0 && c.x < sz.x && c.y > 0 && c.y < listitems() * ITEM_HEIGHT)
                hover = c.y / ITEM_HEIGHT + sb.val;
            else
                hover = -1;
        }

        @Override
        protected void drawitem(GOut g, LoginData item, int i) {
            if (hover == i) {
                g.chcolor(96, 96, 96, 255);
                g.frect(Coord.z, g.sz);
                g.chcolor();
            }
            Tex tex = Text.render(item.name, Color.WHITE, textfs).tex();
            int y = ITEM_HEIGHT / 2 - tex.sz().y / 2;
            g.image(tex, new Coord(5, y));
            g.image(xicon, new Coord(sz.x - 25, y));
        }

        @Override
        public boolean mousedown(Coord c, int button) {
            lastMouseDown = c;
            return super.mousedown(c, button);
        }

        @Override
        protected void itemclick(LoginData itm, int button) {
            if (button == 1) {
                if (lastMouseDown.x >= sz.x - 25 && lastMouseDown.x <= sz.x - 25 + 20) {
                    synchronized (Config.logins) {
                        Config.logins.remove(itm);
                        Config.saveLogins();
                    }
                } else if (c.x < sz.x - 35) {
                    parent.wdgmsg("forget");
                    parent.wdgmsg("login", new Object[]{new AuthClient.NativeCred(itm.name, itm.pass), false});
                    Context.accname = itm.name;
                }
                super.itemclick(itm, button);
            }
        }
    }

    private void mklogin() {
        synchronized (ui) {
            adda(btn = new IButton("gfx/hud/buttons/login", "u", "d", "o") {
                protected void depress() {
                    Audio.play(Button.lbtdown.stream());
                }

                protected void unpress() {
                    Audio.play(Button.lbtup.stream());
                }
            }, 419, 510, 0.5, 0.5);
            progress(null);
        }
    }

    private void error(String error) {
        synchronized (ui) {
            if (this.error != null)
                this.error = null;
            if (error != null)
                this.error = textf.render(error, java.awt.Color.RED);
        }
    }

    private void progress(String p) {
        synchronized (ui) {
            if (progress != null)
                progress = null;
            if (p != null)
                progress = textf.render(p, java.awt.Color.WHITE);
        }
    }

    private void clear() {
        if (cur != null) {
            ui.destroy(cur);
            cur = null;
            ui.destroy(btn);
            btn = null;
        }
        progress(null);
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (sender == btn) {
            if (cur.enter()) {
                Context.accname = user.text;
                super.wdgmsg("login", cur.data());
            }
            return;
        } else if (sender == optbtn) {
            if (opts == null) {
                opts = adda(new OptWnd(false) {
                    public void hide() {
                /* XXX */
                        reqdestroy();
                    }
                }, sz.div(2), 0.5, 0.5);
            } else {
                opts.reqdestroy();
                opts = null;
            }
            return;
        } else if (sender == opts) {
            opts.reqdestroy();
            opts = null;
        } else if(sender == statusbtn) {
        		Desktop desktop = Desktop.isDesktopSupported() ? Desktop.getDesktop() : null;
        		if(desktop != null && desktop.isSupported(Desktop.Action.BROWSE)) {
        			try {
                    desktop.browse(new URI("http://www.havenandhearth.com/portal/"));
					} catch (IOException | URISyntaxException e) {
						e.printStackTrace();
					}
        		}
        } else
        	super.wdgmsg(sender, msg, args);
    }

    public void cdestroy(Widget ch) {
        if (ch == opts) {
            opts = null;
        }
    }

    public void uimsg(String msg, Object... args) {
        synchronized (ui) {
            if (msg == "passwd") {
                clear();
                cur = new Pwbox((String) args[0], (Boolean) args[1]);
                mklogin();
            } else if (msg == "token") {
                clear();
                cur = new Tokenbox((String) args[0]);
                mklogin();
            } else if (msg == "error") {
                error((String) args[0]);
            } else if (msg == "prg") {
                error(null);
                clear();
                progress((String) args[0]);
            }
        }
    }

    public void presize() {
        c = parent.sz.div(2).sub(sz.div(2));
    }

    protected void added() {
        presize();
        parent.setfocus(this);
        if(Config.isUpdate){
            showChangeLog();
        }
    }

    public void draw(GOut g) {
        super.draw(g);
        if (error != null)
            g.image(error.tex(), new Coord(420 - (error.sz().x / 2), 450));
        if (progress != null)
            g.image(progress.tex(), new Coord(420 - (progress.sz().x / 2), 350));
    }

    public boolean type(char k, KeyEvent ev) {
        if (k == 10) {
            if ((cur != null) && cur.enter()) {
                Context.accname = user.text;
                wdgmsg("login", cur.data());
            }
            return (true);
        }
        return (super.type(k, ev));
    }
    
    private void StartUpdaterThread() {
        Thread statusupdaterthread = new Thread(new Runnable() {
            public void run() {
				try {

					URL url = new URL("http://www.havenandhearth.com/portal/index/status");
	        		while(true) {
						Scanner scan = new Scanner(url.openStream());
						while(scan.hasNextLine()) {
							String line = scan.nextLine();
							if(line.contains("h2")) {
								statusbtn.change(line.substring(line.indexOf("<h2>")+4, line.indexOf("</h2>")), Color.WHITE);
		        				}
	        				}
                        GameUI gui = PBotAPI.gui;
	        				if(gui != null) {
                                if (gui.ui.sess.alive()) {
                                   break;
                                }
                            }
						Thread.sleep(5000);
	        		}
				} catch(IOException e) {
					e.printStackTrace();
				} catch(InterruptedException e) {
					e.printStackTrace();
				} 
            }
        });
        statusupdaterthread.start();
}
}
