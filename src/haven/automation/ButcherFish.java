package haven.automation;


import haven.FlowerMenu;
import haven.GameUI;
import haven.WItem;

public class ButcherFish implements Runnable, WItemDestroyCallback {
    private GameUI gui;
    private boolean fishdone;
    private static final int TIMEOUT = 2000;
    private static final int DELAY = 8;

    public ButcherFish(GameUI gui) {
        this.gui = gui;
    }

    @Override
    public void run() {
        WItem fish;
        while ((fish = Utils.findItemByPrefixInInv(gui.maininv, "gfx/invobjs/fish-")) != null) {
            fishdone = false;
            fish.registerDestroyCallback(this);

            FlowerMenu.setNextSelection("Butcher");
            gui.ui.lcc = fish.rootpos();
            fish.item.wdgmsg("iact", fish.c, 0);

            int timeout = 0;
            while (!fishdone) {
                timeout += DELAY;
                if (timeout >= TIMEOUT)
                    return;
                try {
                    Thread.sleep(DELAY);
                } catch (InterruptedException e) {
                    return;
                }
            }
        }
    }

    @Override
    public void notifyDestroy() {
        fishdone = true;
    }
}
