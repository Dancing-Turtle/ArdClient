package haven.automation;


import haven.*;
import haven.purus.BotUtils;

import java.awt.*;

import static haven.OCache.posres;

public class ShooTargeted implements Runnable {
    private GameUI gui;

    public ShooTargeted(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        Gob animal = MapView.shooanimal;

        if (animal == null)
            return;

        FlowerMenu.setNextSelection("Shoo");
        gui.map.wdgmsg("click", animal.sc, animal.rc.floor(posres), 3, 0, 0, (int) animal.id, animal.rc.floor(posres), 0, -1);
    }
}
