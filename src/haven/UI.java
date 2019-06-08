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
import java.lang.ref.WeakReference;
import java.util.*;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.InputEvent;
import java.util.List;


public class UI {
    public RootWidget root;
    public static int MOD_SHIFT = 1, MOD_CTRL = 2, MOD_META = 4, MOD_SUPER = 8;
    final private LinkedList<Grab> keygrab = new LinkedList<Grab>(), mousegrab = new LinkedList<Grab>();
    public Map<Integer, Widget> widgets = new TreeMap<Integer, Widget>();
    public Map<Widget, Integer> rwidgets = new HashMap<Widget, Integer>();
    Receiver rcvr;
    public Coord mc = Coord.z, lcc = Coord.z;
    public Session sess;
    public int questid;
    public boolean modshift, modctrl, modmeta, modsuper;
    public int keycode;
    public boolean readytodrop = false;
    public Object lasttip;
    double lastevent, lasttick;
    public Widget mouseon;
    public Console cons = new WidgetConsole();
    private Collection<AfterDraw> afterdraws = new LinkedList<AfterDraw>();
    public final ActAudio audio = new ActAudio();
    public int beltWndId = -1;
	public GameUI gui;
	public WeakReference<Widget> realmchat;
    public WeakReference<FightWnd> fightwnd;

    {
        lastevent = lasttick = Utils.rtime();
    }

    public interface Receiver {
        public void rcvmsg(int widget, String msg, Object... args);
    }

    public interface Runner {
        public Session run(UI ui) throws InterruptedException;
    }

    public interface AfterDraw {
        public void draw(GOut g);
    }

    private class WidgetConsole extends Console {
        {
            setcmd("q", (cons1, args) -> HackThread.tg().interrupt());
            setcmd("lo", (cons1, args) -> sess.close());
            setcmd("kbd", (cons1, args) -> {
                Config.zkey = args[1].toString().equals("z") ? KeyEvent.VK_Y : KeyEvent.VK_Z;
                Utils.setprefi("zkey", Config.zkey);
            });
        }

        private void findcmds(Map<String, Command> map, Widget wdg) {
            if (wdg instanceof Directory) {
                Map<String, Command> cmds = ((Directory) wdg).findcmds();
                synchronized (cmds) {
                    map.putAll(cmds);
                }
            }
            for (Widget ch = wdg.child; ch != null; ch = ch.next)
                findcmds(map, ch);
        }

        public Map<String, Command> findcmds() {
            Map<String, Command> ret = super.findcmds();
            findcmds(ret, root);
            return (ret);
        }
    }

    @SuppressWarnings("serial")
    public static class UIException extends RuntimeException {
        public String mname;
        public Object[] args;

        public UIException(String message, String mname, Object... args) {
            super(message);
            this.mname = mname;
            this.args = args;
        }
    }

    public UI(Coord sz, Session sess) {
        root = new RootWidget(this, sz);
        widgets.put(0, root);
        rwidgets.put(root, 0);
        this.sess = sess;
        if(sess != null)
            this.sess.glob.ui = new WeakReference<>(this);
    }

    public void setreceiver(Receiver rcvr) {
        this.rcvr = rcvr;
    }

    public void bind(Widget w, int id) {
        widgets.put(id, w);
        rwidgets.put(w, id);
        if(id == questid && CharWnd.abandonquest){
            w.wdgmsg("opt","rm");
            readytodrop = true;
            CharWnd.abandonquest = false;
        }
        w.binded();
    }

    public void drawafter(AfterDraw ad) {
        synchronized (afterdraws) {
            afterdraws.add(ad);
        }
    }

    public void tick() {
        double now = Utils.rtime();
        root.tick(now - lasttick);
        lasttick = now;
    }

    public void draw(GOut g) {
        root.draw(g);
        synchronized (afterdraws) {
            for (AfterDraw ad : afterdraws)
                ad.draw(g);
            afterdraws.clear();
        }
    }

    //ids go sequential, 2^16 limit judging by parent != 65535...
    //At 65535 wrap back around? Can we break the game by hitting that limit.............
    public int next_predicted_id = 2;
    public void newwidget(int id, String type, int parent, Object[] pargs, Object... cargs) throws InterruptedException {

     // System.out.println("Widget ID : "+id+" Type : "+type+" Parent : "+parent);
        if (Config.quickbelt && type.equals("wnd") && cargs[1].equals("Belt")) {
            // use custom belt window
            type = "alt-wnd-belt";
            beltWndId = id;
        } else if (type.equals("inv") && pargs[0].toString().equals("study")) {
            // use custom study inventory
            type = "inv-study";
        }

        Widget.Factory f = Widget.gettype2(type);
        synchronized(this) {
            if (parent == beltWndId)
                f = Widget.gettype2("inv-belt");

            Widget wdg = f.create(this, cargs);
            wdg.attach(this);
            if(type.equals("quest")) {
                if(CharWnd.abandonquest) {
                  questid = id;
                }
            }
            if (parent != 65535) {
                Widget pwdg = widgets.get(parent);
                if(pwdg == null)
                    throw(new UIException("Null parent widget " + parent + " for " + id, type, cargs));
                pwdg.addchild(wdg, pargs);

                if (pwdg instanceof Window) {
                    // here be horrors... FIXME
                    GameUI gui = null;
                    for (Widget w : rwidgets.keySet()) {
                        if (w instanceof GameUI) {
                            gui = (GameUI) w;
                            break;
                        }
                    }
                    processWindowContent(parent, gui, (Window) pwdg, wdg);
                }
            } else {
                if (wdg instanceof Window) {
                    // here be horrors... FIXME
                    GameUI gui = null;
                    for (Widget w : rwidgets.keySet()) {
                        if (w instanceof GameUI) {
                            gui = (GameUI) w;
                            break;
                        }
                    }
                    processWindowCreation(id, gui, (Window) wdg);
                }
            }
            bind(wdg, id);
            if(type.contains("rchan"))
                 realmchat = new WeakReference<>(wdg);
            if(type.contains("speedget"))
                gui.speedget = new WeakReference<>((Speedget)wdg);
            if(wdg instanceof FightWnd){
                fightwnd = new WeakReference<>((FightWnd)wdg);
            }
        }
        next_predicted_id = id + 1;
    }

    public void addwidget(int id, int parent, Object[] pargs) {
        synchronized(this) {
            Widget wdg = widgets.get(id);
            if(wdg == null)
                throw(new UIException("Null child widget " + id + " added to " + parent, null, pargs));
            Widget pwdg = widgets.get(parent);
            if(pwdg == null)
                throw(new UIException("Null parent widget " + parent + " for " + id, null, pargs));
            pwdg.addchild(wdg, pargs);
        }
    }

    private void processWindowContent(long wndid, GameUI gui, Window pwdg, Widget wdg) {
        String cap = pwdg.origcap;
        if (gui != null && gui.livestockwnd.pendingAnimal != null && gui.livestockwnd.pendingAnimal.wndid == wndid) {
            if (wdg instanceof TextEntry)
                gui.livestockwnd.applyName(wdg);
            else if (wdg instanceof Label)
                gui.livestockwnd.applyAttr(cap, wdg);
            else if (wdg instanceof Avaview)
                gui.livestockwnd.applyId(wdg);
        } else if (wdg instanceof ISBox && cap.equals("Stockpile")) {
            TextEntry entry = new TextEntry(40, "") {
                @Override
                public boolean keydown(KeyEvent e) {
                    return !(e.getKeyCode() >= KeyEvent.VK_F1 && e.getKeyCode() <= KeyEvent.VK_F12);
                }

                @Override
                public boolean type(char c, KeyEvent ev) {
                    if (c >= KeyEvent.VK_0 && c <= KeyEvent.VK_9 && buf.line.length() < 2 || c == '\b') {
                        return buf.key(ev);
                    } else if (c == '\n') {
                        try {
                            int count = Integer.parseInt(dtext());
                            for (int i = 0; i < count; i++)
                                wdg.wdgmsg("xfer");
                            return true;
                        } catch (NumberFormatException e) {
                        }
                    }
                    return false;
                }
            };
            Button btn = new Button(65, "Take") {
                @Override
                public void click() {
                    try {
                        String cs = entry.dtext();
                        int count = cs.isEmpty() ? 1 : Integer.parseInt(cs);
                        for (int i = 0; i < count; i++)
                            wdg.wdgmsg("xfer");
                    } catch (NumberFormatException e) {
                    }
                }
            };
            pwdg.add(btn, new Coord(0, wdg.sz.y + 5));
            pwdg.add(entry, new Coord(btn.sz.x + 5, wdg.sz.y + 5 + 2));
        }
    }

    private void processWindowCreation(long wdgid, GameUI gui, Window wdg) {
        String cap = wdg.origcap;
        if (cap.equals("Charter Stone") || cap.equals("Sublime Portico")) {
            // show secrets list only for already built chartes/porticos
            if (wdg.wsz.y >= 50) {
                wdg.add(new CharterList(150, 20), new Coord(0, 50));
                wdg.presize();
            }
        } else if (gui != null && gui.livestockwnd != null && gui.livestockwnd.getAnimalPanel(cap) != null) {
            gui.livestockwnd.initPendingAnimal(wdgid, cap);
        }
    }

    public abstract class Grab {
        public final Widget wdg;

        public Grab(Widget wdg) {
            this.wdg = wdg;
        }

        public abstract void remove();
    }

    public Grab grabmouse(Widget wdg) {
        if (wdg == null) throw (new NullPointerException());
        Grab g = new Grab(wdg) {
            public void remove() {
                mousegrab.remove(this);
            }
        };
        mousegrab.addFirst(g);
        return (g);
    }

    public Grab grabkeys(Widget wdg) {
        if (wdg == null) throw (new NullPointerException());
        Grab g = new Grab(wdg) {
            public void remove() {
                keygrab.remove(this);
            }
        };
        keygrab.addFirst(g);
        return (g);
    }

    private void removeid(Widget wdg) {
        wdg.removed();
        if (rwidgets.containsKey(wdg)) {
            int id = rwidgets.get(wdg);
            widgets.remove(id);
            rwidgets.remove(wdg);
        }
        for (Widget child = wdg.child; child != null; child = child.next)
            removeid(child);
    }

    public void destroy(Widget wdg) {
        for (Iterator<Grab> i = mousegrab.iterator(); i.hasNext(); ) {
            Grab g = i.next();
            if (g.wdg.hasparent(wdg))
                i.remove();
        }
        for (Iterator<Grab> i = keygrab.iterator(); i.hasNext(); ) {
            Grab g = i.next();
            if (g.wdg.hasparent(wdg))
                i.remove();
        }
        removeid(wdg);
        wdg.reqdestroy();
    }

    public void destroy(int id) {
        synchronized (this) {
            if (widgets.containsKey(id)) {
                Widget wdg = widgets.get(id);
                destroy(wdg);
            }
        }
    }

    /**
     * For scripting only
     */
    public void wdgmsg(final int id, final String msg, Object... args) {
        if(rcvr != null)
            rcvr.rcvmsg(id, msg, args);
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
        int id;
        synchronized(this) {
     //  try { for(Object obj:args) if(!sender.toString().contains("Camera")) System.out.println("Sender : " + sender + " msg = " + msg + " arg 1 : " + obj); }catch(ArrayIndexOutOfBoundsException q){}
            if (msg.endsWith("-identical"))
                return;

            if(!rwidgets.containsKey(sender)) {
                if(msg.equals("close")) {
                    sender.reqdestroy();
                    return;
                }
                else
                System.err.printf("Wdgmsg sender (%s) is not in rwidgets, message is %s\n", sender.getClass().getName(), msg);
                 //   System.out.println("Args:"+args[0]);
              // System.out.println("Sender is : "+sender);
                return;
            }
            id = rwidgets.get(sender);
        }
        if(rcvr != null)
            rcvr.rcvmsg(id, msg, args);
    }

    public void uimsg(int id, String msg, Object... args) {
        synchronized (this) {
            Widget wdg = widgets.get(id);
            if(realmchat != null){
                try{
                 if(realmchat.get() != null && id == realmchat.get().wdgid()) {
                     if (msg.contains("msg") && wdg.toString().contains("Realm")) {
                         ((ChatUI.EntryChannel) realmchat.get()).updurgency(1);
                         if (Config.realmchatalerts)
                             Audio.play(ChatUI.notifsfx);
                     }
                 }
            }catch(NullPointerException e){e.printStackTrace();}
            }
                if (wdg != null) {
             // try { for(Object obj:args) if(!wdg.toString().contains("CharWnd")) System.out.println("UI Wdg : " + wdg + " msg : "+msg+" id = " + id + " arg 1 : " + obj); }catch(ArrayIndexOutOfBoundsException qq){}
                wdg.uimsg(msg.intern(), args); }
                    else throw (new UIException("Uimsg to non-existent widget " + id, msg, args));
            }
    }

    private void setmods(InputEvent ev) {
        int mod = ev.getModifiersEx();
        Debug.kf1 = modshift = (mod & InputEvent.SHIFT_DOWN_MASK) != 0;
        Debug.kf2 = modctrl = (mod & InputEvent.CTRL_DOWN_MASK) != 0;
        Debug.kf3 = modmeta = (mod & (InputEvent.META_DOWN_MASK | InputEvent.ALT_DOWN_MASK)) != 0;
    /*
    Debug.kf4 = modsuper = (mod & InputEvent.SUPER_DOWN_MASK) != 0;
	*/
    }

    private Grab[] c(Collection<Grab> g) {
        return (g.toArray(new Grab[0]));
    }

    public void type(KeyEvent ev) {
        setmods(ev);
        for (Grab g : c(keygrab)) {
            //Make sure this wdg is visible the entire way up
            if (g.wdg.tvisible()) {
            if (g.wdg.type(ev.getKeyChar(), ev))
                return;
        }
        }
        if (!root.type(ev.getKeyChar(), ev))
            root.globtype(ev.getKeyChar(), ev);
    }

    public void keydown(KeyEvent ev) {
        setmods(ev);
        keycode = ev.getKeyCode();
        for (Grab g : c(keygrab)) {
            //Make sure this wdg is visible the entire way up
            if (g.wdg.tvisible()) {
            if (g.wdg.keydown(ev))
                return;
        }
        }
        if (!root.keydown(ev))
            root.globtype((char) 0, ev);
    }

    public void keyup(KeyEvent ev) {
        setmods(ev);
        keycode = -1;
        for (Grab g : c(keygrab)) {
            //Make sure this wdg is visible the entire way up
            if (g.wdg.tvisible()) {
            if (g.wdg.keyup(ev))
                return;
        }
        }
        root.keyup(ev);
    }

    private Coord wdgxlate(Coord c, Widget wdg) {
        return (c.sub(wdg.rootpos()));
    }

    public boolean dropthing(Widget w, Coord c, Object thing) {
        if (w instanceof DropTarget) {
            if (((DropTarget) w).dropthing(c, thing))
                return (true);
        }
        for (Widget wdg = w.lchild; wdg != null; wdg = wdg.prev) {
            Coord cc = w.xlate(wdg.c, true);
            if (c.isect(cc, wdg.sz)) {
                if (dropthing(wdg, c.add(cc.inv()), thing))
                    return (true);
            }
        }
        return (false);
    }
  //  public void mousedown(Coord c, int button){
   //     mousedown(new MouseEvent(panel, 0, 0, 0, c.x, c.y, 1, false, button), c, button);
   // }

    public void mousedown(MouseEvent ev, Coord c, int button) {
        setmods(ev);
        lcc = mc = c;
        for (Grab g : c(mousegrab)) {
            //Make sure this wdg is visible the entire way up
            if (g.wdg.tvisible()) {
            if (g.wdg.mousedown(wdgxlate(c, g.wdg), button))
                return;
        }
        }
        root.mousedown(c, button);
    }

    public void mouseup(MouseEvent ev, Coord c, int button) {
        setmods(ev);
        mc = c;
        for (Grab g : c(mousegrab)) {
            //Make sure this wdg is visible the entire way up
            if (g.wdg.tvisible()) {
            if (g.wdg.mouseup(wdgxlate(c, g.wdg), button))
                return;
        }
        }
        root.mouseup(c, button);
    }

    public void mousemove(MouseEvent ev, Coord c) {
        setmods(ev);
        mc = c;
        root.mousemove(c);
    }

    public void mousewheel(MouseEvent ev, Coord c, int amount) {
        setmods(ev);
        lcc = mc = c;
        for (Grab g : c(mousegrab)) {
            if (g.wdg.tvisible()) {
            if (g.wdg.mousewheel(wdgxlate(c, g.wdg), amount))
                return;
        }
        }
        root.mousewheel(c, amount);
    }

    public static int modflags(InputEvent ev) {
        int mod = ev.getModifiersEx();
        return((((mod & InputEvent.SHIFT_DOWN_MASK) != 0) ? MOD_SHIFT : 0) |
                (((mod & InputEvent.CTRL_DOWN_MASK) != 0)  ? MOD_CTRL : 0) |
                (((mod & (InputEvent.META_DOWN_MASK | InputEvent.ALT_DOWN_MASK)) != 0) ? MOD_META : 0)
                /* (((mod & InputEvent.SUPER_DOWN_MASK) != 0) ? MOD_SUPER : 0) */);
    }

    public int modflags() {
        return((modshift ? MOD_SHIFT : 0) |
                (modctrl  ? MOD_CTRL  : 0) |
                (modmeta  ? MOD_META  : 0) |
                (modsuper ? MOD_SUPER : 0));
    }


    public void destroy() {
        audio.clear();
        removeid(root);
    }
}
