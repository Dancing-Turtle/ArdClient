package haven.automation;


import haven.Coord;
import haven.FlowerMenu;
import haven.GameUI;
import haven.WItem;
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;

public class OysterOpener implements Runnable {
    private GameUI gui;
    int startsize = 0;
    public OysterOpener(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        List<WItem> oysters = new ArrayList<>();
        try {
            oysters.addAll(PBotUtils.getPlayerInvContentsPartial("Oyster"));
            startsize = oysters.size();
        } catch (Exception q) {}
        if(oysters.size() == 0){
            PBotUtils.sysMsg("No blocks found.",Color.white);
            return;
        }
        for (WItem item : oysters) {
            FlowerMenu.setNextSelection("Crack open");
            System.out.println(PBotUtils.getPlayerInvContentsPartial("Block").size()+ " startsize : "+startsize);
            item.item.wdgmsg("iact", Coord.z, -1);
            int timeout = 0;
            while(PBotUtils.getPlayerInvContentsPartial("Oyster").size() == startsize) {
                timeout++;
                if(timeout > 200)
                    break;
                PBotUtils.sleep(100);
            }
            startsize--;
        }
        PBotUtils.sysMsg("Exited OysterOpener",Color.white);
    }
}

