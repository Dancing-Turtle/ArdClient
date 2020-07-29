package haven.automation;


import haven.Coord;
import haven.GameUI;
import haven.WItem;
import haven.purus.pbot.PBotUtils;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

public class SplitLogs implements Runnable {
    private GameUI gui;
    private static final int TIMEOUT = 2000;
    int startsize = 0;

    public SplitLogs(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        List<WItem> blocks = new ArrayList<>();
        try {
            blocks.addAll(PBotUtils.getPlayerInvContentsPartial(gui.ui, "Block"));
            startsize = blocks.size();
        } catch (Exception q) {
        }
        if (blocks.size() == 0) {
            PBotUtils.sysMsg(gui.ui, "No blocks found.", Color.white);
            return;
        }
        int startID = gui.ui.next_predicted_id;
        int iteration = 0;
        int freespace = PBotUtils.freeSlotsInv(gui.maininv);
        for (WItem item : blocks) {
            item.item.wdgmsg("iact", Coord.z, -1);
            gui.ui.wdgmsg(startID + iteration, "cl", 0, 0);
            if (freespace >= 3)
                iteration = iteration + 6;
            else if (freespace == 2)
                iteration = iteration + 5;
            else if (freespace == 1)
                iteration = iteration + 4;
            else
                iteration = iteration + 3;
            freespace = freespace - 3;
        }
        PBotUtils.sysMsg(gui.ui, "Exiting Log Splitter", Color.white);
    }
}

