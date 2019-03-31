package haven.automation;


import haven.*;
import haven.Utils;
import haven.purus.pbot.PBotUtils;

import static haven.OCache.posres;

public class ShooTargeted implements Runnable {
    private GameUI gui;
    private boolean inspect, slaughter;

    public ShooTargeted(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        Gob animal = MapView.shooanimal;
        inspect = Config.flowermenus.get("Inspect").selected;
        slaughter = Config.flowermenus.get("Slaughter").selected;
        for(CheckListboxItem itm :Config.flowermenus.values()) {
            if (itm.name.equals("Inspect") || itm.name.equals("Slaughter")) {
                itm.selected =false;
                Utils.setprefchklst("flowersel", Config.flowermenus);
            }
        }

        if (animal == null)
            return;
        FlowerMenu.setNextSelection("Shoo");
       gui.map.wdgmsg("click", animal.sc, animal.rc.floor(posres), 3, 0, 0, (int) animal.id, animal.rc.floor(posres), 0, -1);
        PBotUtils.sleep(300);
        for(CheckListboxItem itm :Config.flowermenus.values()) {
            if (itm.name.equals("Inspect")) {
                itm.selected =inspect;
                Utils.setprefchklst("flowersel", Config.flowermenus);
            }
            if (itm.name.equals("Slaughter")) {
                itm.selected =slaughter;
                Utils.setprefchklst("flowersel", Config.flowermenus);
            }
        }
    }
}
