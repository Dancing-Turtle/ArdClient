package haven.timers;


import java.awt.Color;
import java.util.List;

import haven.Audio;
import haven.Config;
import haven.Coord;
import haven.GameUI;
import haven.Glob;
import haven.Label;
import haven.Resource;
import haven.Text;
import haven.Widget;

public class TimerWdg extends Widget {
    private static final Resource timersfx = Resource.local().loadwait("sfx/timer");
    public final static int height = 31;
    private final static int txty = 8;
    public String name;
    public long start, duration, elapsed;
    public boolean active = false;
    private haven.Label lbltime, lblname;
    private haven.Button btnstart, btnstop, btnedit;
    private Label btndel;

    public TimerWdg(String name, long duration, long start) {
        this.name = name;
        this.duration = duration;

        sz = new Coord(420, height);
        lblname = new haven.Label(name, Text.num12boldFnd, Color.WHITE);
        add(lblname, new Coord(3, txty));
        lbltime = new haven.Label(timeFormat(duration), Text.num12boldFnd, Color.WHITE);

        add(lbltime, new Coord(190, txty));

        btnstart = new haven.Button(50, "Start") {
            @Override
            public void click() {
                start();
            }
        };
        btnstop = new haven.Button(50, "Stop") {
            @Override
            public void click() {
                stop();
            }
        };
        btnstop.hide();
        btndel = new Label("\u2718", Text.delfnd, Color.RED) {
            @Override
            public boolean mousedown(Coord c, int button) {
                delete();
                return true;
            }
        };
        btnedit = new haven.Button(50, "Edit") {
            @Override
            public void click() {
                edit();
            }
        };

        add(btnstart, new Coord(270, 3));
        add(btnstop, new Coord(270, 3));
        add(btnedit, new Coord(334, 3));
        add(btndel, new Coord(395, 6));

        if (start != 0)
            start(start);
    }

    public void updateRemaining() {
        lbltime.settext(timeFormat(duration - elapsed));
    }

    public void updateDuration() {
        lbltime.settext(timeFormat(duration));
    }


    public void updateName() {
        lblname.settext(name.length() > 21 ? name.substring(0, 20) : name);
    }

    private String timeFormat(long time) {
        long ts = time / 1000;
        return String.format("%02d:%02d.%02d", (int) (ts / 3600), (int) ((ts % 3600) / 60), (int) (ts % 60));
    }

    public void start() {
        start = (long)(ui.sess.glob.globtime() * 1000 / Glob.SERVER_TIME_RATIO);
        btnstart.hide();
        btnstop.show();
        active = true;
        Glob.timersThread.save();
    }

    public void start(long start) {
        this.start = start;
        btnstart.hide();
        btnstop.show();
        active = true;
    }

    public void delete() {
        active = false;
        Glob.timersThread.remove(this);
        reqdestroy();
        int y = this.c.y;
       List<TimerWdg> timers = Glob.timersThread.getall();
        for (TimerWdg timer : timers) {
            if (timer.c.y > y)
                timer.c.y -= height;
        }
        gameui().timerswnd.resize();
        Glob.timersThread.save();
    }

    public void stop() {
        active = false;
        btnstart.show();
        btnstop.hide();
        updateDuration();
    }

    public void done() {
        stop();
        GameUI gui = ((TimersWnd) parent.parent.parent).gui;
        gui.add(new TimerDoneWindow(name, this), new Coord(gui.sz.x / 2 - 150, gui.sz.y / 2 - 75));
        if (Config.timersalarm)
            Audio.play(timersfx, Config.timersalarmvol);
        Glob.timersThread.save();
    }

    public void edit() {
        GameUI gui = ((TimersWnd) parent.parent.parent).gui;
        gui.add(new TimerEditWnd("Edit Timer", gui, name, duration, this), new Coord(gui.sz.x / 2 - 200, gui.sz.y / 2 - 200));
    }

    private class TimerDoneWindow extends haven.Window {
        public TimerDoneWindow(String timername, TimerWdg timer) {
            super(new Coord(300, 130), "Hooray!","Hooray!");

            haven.Label lbltimer = new haven.Label(timername, Text.num12boldFnd);
            add(lbltimer, new Coord(300 / 2 - lbltimer.sz.x / 2, 20));

            haven.Label lblinf = new haven.Label("has finished running");
            add(lblinf, new Coord(300 / 2 - lblinf.sz.x / 2, 50));

            add(new haven.Button(60, "Close") {
                @Override
                public void click() {
                    parent.reqdestroy();
                }
            }, new Coord(150 / 2 - 60 / 2, 90));
            add(new haven.Button(60, "Close & Restart") {
                @Override
                public void click() {
                    timer.start();
                    parent.reqdestroy();
                }
            }, new Coord(200 - 60 / 2, 90));
        }

        @Override
        public void wdgmsg(Widget sender, String msg, Object... args) {
            if (sender == cbtn)
                reqdestroy();
            else
                super.wdgmsg(sender, msg, args);
        }

        @Override
        public boolean type(char key, java.awt.event.KeyEvent ev) {
            if (key == 27) {
                reqdestroy();
                return true;
            }
            return super.type(key, ev);
        }
    }
}
