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
    public Boolean isknocked;

    public CountGobs(GameUI gui) {
        this.gui = gui;
    }

    private static final int TIMEOUT = 2000;

    public void run() {


        BotUtils.sysMsg("Alt + click your target", Color.WHITE);
        gui.map.unregisterGobSelect();
        registerGobSelect();
        while (selection == null) {
            BotUtils.sleep(10);
        }
        if (selection == null){
            BotUtils.sysMsg("Selection is Null",Color.white);
        }
        countgobs();

    }

    private void registerGobSelect() {
        synchronized (GobSelectCallback.class) {
          //  BotUtils.sysMsg("Registering Gob",Color.white);
            BotUtils.gui.map.registerGobSelect(this);
        }
    }


    public void gobselect(Gob gob) {
        while (gob == null){
            BotUtils.sleep(10);
        }
        selection = gob;
        if(gob.knocked)
            isknocked = true;
        BotUtils.sysMsg("Selection is "+selection.getres().name,Color.white);
        //BotUtils.sysMsg(selection.getres().name, Color.white);
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
                if (selection.getres().name == allGobs.get(i).getres().name && gob.knocked == isknocked) {
                   // BotUtils.sysMsg("idk" + selection.getres().name, Color.white);
                    //BotUtils.sysMsg("idk2" + allGobs.get(i).getres().name, Color.white);
                    list.add(allGobs.get(i));
                    // animal2 = allGobs.get(i)
                    // ((LivestockManager.Panel)parent.parent.parent).delete(animal2);
                    // ((Scrollport.Scrollcont) parent).update();
                    // BotUtils.sysMsg("Working",Color.white);
                }
            }
                catch(NullPointerException | Loading e){ }
        }
        BotUtils.sysMsg("Total is - "+list.size(),Color.white);

    }

}



