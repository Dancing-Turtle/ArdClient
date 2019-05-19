package haven.timers;


import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import haven.*;
import haven.res.ui.tt.q.qbuff.QBuff;
import org.json.JSONException;
import org.json.JSONObject;

public class TimersThread extends Thread {
    private List<TimerWdg> timers = new ArrayList<TimerWdg>();

    public TimersThread() {
        super("Timers Thread");
    }

    @Override
    public void run() {
        while (true) {
            synchronized (timers) {
                for (int i = 0; i < timers.size(); i++) {
                    TimerWdg timer = timers.get(i);
                    if (!timer.active || timer.ui == null)
                        continue;

                    Session sess = timer.ui.sess;
                    if (!sess.state.equals("") || sess.glob.serverEpoch == 0)
                        continue;

                    timer.elapsed = (long)(sess.glob.globtime() * 1000 / Glob.SERVER_TIME_RATIO) - timer.start;
                    timer.updateRemaining();

                    if (timer.elapsed >= timer.duration) {
                        timer.done();
                        i--;
                    }
                }
            }
            try {
                sleep(1000);
            } catch (InterruptedException e) {
            }
        }
    }

    public TimerWdg add(String name, long duration, long start) {
        synchronized (timers) {
            TimerWdg timer = new TimerWdg(name, duration, start);
            timers.add(timer);
            if(Config.timersort)
                sort(timers);
            return timer;
        }
    }

    public void remove(TimerWdg timer) {
        synchronized (timers) {
            timers.remove(timer);
        }
    }

    public List<TimerWdg> getall() {
        synchronized (timers) {
            return timers;
        }
    }


    public void save() {
        synchronized (timers) {
            JSONObject[] timersjson = new JSONObject[timers.size()];
            for (int i = 0; i < timers.size(); i++) {
                final TimerWdg timer = timers.get(i);
                timersjson[i] = new JSONObject()
                        .put("name", timer.name)
                        .put("duration", timer.duration)
                        .put("start", timer.active ? timer.start : 0);
            }
            Utils.setprefjsona("timers", timersjson);
        }
    }

    public void load() {
        JSONObject[] tstarr = Utils.getprefjsona("timers", null);
        if (tstarr == null)
            return;
        for (int i = 0; i < tstarr.length; i++) {
            JSONObject t = tstarr[i];
            long start = 0;
            try {
                start = t.getLong("start");
            } catch (JSONException e) {
            }
            add(t.getString("name"), t.getLong("duration"), start);
        }
    }

    public void sort (List <TimerWdg> items) {
        Collections.sort(items, (a, b) -> {
            Long aq = a.duration;
            Long bq = b.duration;
            if (aq == null || bq == null)
                return 0;
            else if (aq.doubleValue() == bq.doubleValue())
                return 0;
            else if (aq.doubleValue() > bq.doubleValue())
                return 1;
            else
                return -1;
        });
    }
}

