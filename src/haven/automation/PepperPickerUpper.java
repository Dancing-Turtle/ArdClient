package haven.automation;


import haven.*;
import haven.purus.BotUtils;

import java.awt.*;

import static haven.OCache.posres;

public class PepperPickerUpper implements Runnable {
    private GameUI gui;

    public PepperPickerUpper(GameUI gui) {
        this.gui = gui;
    }
    private static final int TIMEOUT = 150;
    private static final int DELAY = 8;
    int timeout = 0;
    @Override
    public void run() {

        Gob gob = BotUtils.findObjectByNames(1000, "gfx/terobjs/items/peppercorn");
        if(gob == null) {
            BotUtils.sysMsg("No Pepper on ground found!", Color.GREEN);
            return;
        }
        while(gob != null && BotUtils.getItemAtHand() == null) {
            BotUtils.doClick(gob, 3, 1);
            timeout = 0;
            while(BotUtils.findObjectById(gob.id) != null && BotUtils.getItemAtHand() == null) {
                if(timeout >= TIMEOUT){
                    BotUtils.sysMsg("Timed out",Color.white);
                    return;
                }
                timeout += DELAY;
                BotUtils.sleep(100);
            }
            gob = BotUtils.findObjectByNames(1000, "gfx/terobjs/items/peppercorn");
        }
        BotUtils.sysMsg("Done",Color.white);
    }
}
