package haven.automation;


import haven.*;
import haven.Button;
import haven.Window;
import haven.purus.BotUtils;
import haven.automation.GobSelectCallback;
import haven.purus.pbot.PBotAPI;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;

public class CountGobs implements Runnable, GobSelectCallback {

    public GameUI gui;

    public Gob gob;
    public Gob selection;
    public Boolean isknocked = false;

    public CountGobs(GameUI gui) {
        this.gui = gui;
    }

    private static final int TIMEOUT = 2000;

    public void run() {
        BotUtils.sysMsg("Alt + click your target", Color.WHITE);
        BotUtils.gui.map.registerGobSelect(this);
        while (selection == null) {
            BotUtils.sleep(10);
        }
        countgobs();
    }

    public void gobselect(Gob gob) {
        selection = gob;
       // if(gob.knocked)
           // isknocked = true;
        BotUtils.sysMsg("Selection is "+selection.getres().name,Color.white);
        gui.map.unregisterGobSelect();

    }

    public void countgobs() {
        List<Gob> allGobs = PBotAPI.getGobs();
        List<Gob> list = new ArrayList<>();
       // BotUtils.sysMsg("Counting",Color.white);
        for (int i = 0; i < allGobs.size(); i++) {
            try {
                Resource res = selection.getres();
                Resource res2 = allGobs.get(i).getres();
              //  BotUtils.sysLogAppend("Knocked status : "+selection.knocked,"white");
                if(selection.knocked == null && selection.getres().name.equals(allGobs.get(i).getres().name)){

                    list.add(allGobs.get(i));
                }
                else if (selection.getres().name.equals(allGobs.get(i).getres().name)){
                    if (allGobs.get(i).knocked && selection.knocked)
                        list.add(allGobs.get(i));
                     else if (!allGobs.get(i).knocked && !selection.knocked)
                        list.add(allGobs.get(i));
                }
            }catch(NullPointerException | Loading e){}

        }
        BotUtils.sysMsg("Total is - "+list.size(),Color.white);

    }

}



