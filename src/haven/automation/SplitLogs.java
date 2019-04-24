package haven.automation;


import haven.*;
import haven.Window;
import haven.purus.pbot.PBotUtils;

import java.awt.*;
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
                blocks.addAll(PBotUtils.getPlayerInvContentsPartial("Block"));
                startsize = blocks.size();
            } catch (Exception q) {}
        if(blocks.size() == 0){
            PBotUtils.sysMsg("No blocks found.",Color.white);
            return;
        }
        for (WItem item : blocks) {
                FlowerMenu.setNextSelection("Split");
                System.out.println(PBotUtils.getPlayerInvContentsPartial("Block").size()+ " startsize : "+startsize);
                item.item.wdgmsg("iact", Coord.z, -1);
                int timeout = 0;
                while(PBotUtils.getPlayerInvContentsPartial("Block").size() == startsize) {
                    timeout++;
                    if(timeout > 200)
                        break;
                    PBotUtils.sleep(100);
                }
                startsize--;
        }
        PBotUtils.sysMsg("Exiting Log Splitter",Color.white);
    }
}

