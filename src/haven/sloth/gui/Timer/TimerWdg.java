package haven.sloth.gui.Timer;


import haven.*;
import haven.sloth.DefSettings;
import haven.sloth.io.TimerData;
import haven.sloth.util.ObservableListener;

import java.util.Collection;

public class TimerWdg extends Widget implements ObservableListener<TimerData.TimerInstance> {
    private static final Resource timersfx = Resource.local().loadwait("custom/sfx/timer");
    private final static int height = 31;
    private final static int txty = 8;
    private final static Coord timec = new Coord(190, txty);

    private class TimerInstWdg extends Widget {
        final TimerData.TimerInstance inst;
        private long elapsed = time.duration;
        private TimerInstWdg(final TimerData.TimerInstance inst) {
            this.inst = inst;
            add(new Button(50, "Cancel", this::cancel), new Coord(270, 3));
            pack();
	}

	@Override
	public void draw(GOut g) {
	    FastText.print(g, timec, timeFormat(elapsed));
            super.draw(g);
	}

	@Override
	public void tick(double dt) {
	    elapsed = (time.duration - ((long)ui.sess.glob.globtime() - inst.start))/3;
	    if(elapsed <= 0) {
	        ui.gui.add(new TimerDoneWindow(time.name), new Coord(50, 50));
		Audio.play(timersfx, DefSettings.TIMERVOLUME.get()/1000f);
		time.finish(inst);
	    }
            super.tick(dt);
	}

	private void cancel() {
            time.finish(inst);
	}
    }

    public TimerData.Timer time;
    private final int base_height;

    TimerWdg(TimerData.Timer t) {
	time = t;
	sz = new Coord(420, height);
	add(new Label(time.name), new Coord(3, txty));
	add(new Label(timeFormat(time.duration/3)), timec);
	int x = add(new Button(50, "Start", this::start), new Coord(270, 3)).sz.x + 270 + 5;
	add(new Button(20, "X", this::delete), new Coord(x, 3));
	pack();
	base_height = sz.y;
    }

    @Override
    protected void added() {
	time.listen(this);
    }

    private static String timeFormat(long ts) {
	return String.format("%02d:%02d.%02d", (int) (ts / 3600), (int) ((ts % 3600) / 60), (int) (ts % 60));
    }

    public void start() {
        time.makeInstance((long)ui.sess.glob.globtime());
    }

    public void delete() {
        TimerData.remTimer(time);
    }

    @Override
    public void init(Collection<TimerData.TimerInstance> base) {
	for(final TimerData.TimerInstance inst : base) {
	    add(new TimerInstWdg(inst));
	}
	pack();
	parent.pack();
    }

    @Override
    public void added(TimerData.TimerInstance item) {
	add(new TimerInstWdg(item));
	pack();
	parent.pack();
    }

    @Override
    public void remove(TimerData.TimerInstance item) {
	ui.destroy(find(item));
	pack();
	parent.pack();
    }

    public void pack() {
	int y = base_height;
	Widget next;

	for(Widget wdg = child; wdg != null; wdg = next) {
	    next = wdg.next;
	    if(wdg instanceof TimerInstWdg) {
		wdg.c = new Coord(wdg.c.x, y);
		y += wdg.sz.y;
	    }
	}
	super.pack();
    }

    public TimerInstWdg find(TimerData.TimerInstance t) {
	Widget next;

	for (Widget wdg = child; wdg != null; wdg = next) {
	    next = wdg.next;
	    if (wdg instanceof TimerInstWdg) {
		TimerInstWdg tw = (TimerInstWdg) wdg;
		if (tw.inst == t)
		    return tw;
	    }
	}

	return null;
    }

    @Override
    protected void removed() {
        time.unlisten(this);
    }

    private class TimerDoneWindow extends Window {
	private TimerDoneWindow(String timername) {
	    super(new Coord(300, 130), "Hooray!", "Hooray!");

	    Label lbltimer = new Label(timername);
	    add(lbltimer, new Coord(300 / 2 - lbltimer.sz.x / 2, 20));

	    Label lblinf = new Label("has finished running");
	    add(lblinf, new Coord(300 / 2 - lblinf.sz.x / 2, 50));

	    add(new Button(60, "Close") {
		@Override
		public void click() {
		    parent.reqdestroy();
		}
	    }, new Coord(300 / 2 - 60 / 2, 90));
	}

	public void close() {
	    ui.destroy(this);
	}
    }
}
