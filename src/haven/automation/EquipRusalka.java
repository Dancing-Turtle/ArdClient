package haven.automation;


import haven.*;

import java.util.ArrayList;
import java.util.List;

public class EquipRusalka implements Runnable {
    private GameUI gui;
    private static final int TIMEOUT = 2000;
    private static final Coord sqsz = new Coord(36, 33);

    public EquipRusalka(GameUI gui) {
        this.gui = gui;
    }
    String oldShoes = null;

    @Override
    public void run() {
        try {
            oldShoes = getShoes().item.getname();
            swap(getShoes());
            Inventory inv = gui.maininv;

            List<WItem> items = new ArrayList<>();
            for(Widget witm = inv.child; witm != null; witm = witm.next) {
                synchronized(witm) {
                    if(witm instanceof WItem) {
                        WItem witem = (WItem) witm;
                        items.add(witem);
                    }
                }
            }

            if (oldShoes.contains("Rusalka")){
                for(WItem item: items){
                    if (item.item.getname().contains("Boot")){
                        swap(item);
                        break;
                    }
                }
            } else {
                for(WItem item: items){
                    if (item.item.getname().contains("Rusalka")){
                        swap(item);
                        break;
                    }
                }
            }


        }catch(Exception e){
            e.printStackTrace();
        }

    }

    public void swap(WItem item){
        item.item.wdgmsg("transfer", Coord.z);
    }

    public WItem getShoes(){
        return gui.getequipory().quickslots[15];
    }
}



