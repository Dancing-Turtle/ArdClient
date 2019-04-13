package haven.automation;


import haven.*;
import haven.Button;
import haven.Window;
import haven.automation.GobSelectCallback;
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;

public class CountGobs implements Runnable, GobSelectCallback {

    public GameUI gui;

    public Gob gob;
    public Gob selection;
    private boolean isdead;

    public CountGobs(GameUI gui) {
        this.gui = gui;
    }

    private static final int TIMEOUT = 2000;

    public void run() {
        PBotUtils.sysMsg("Alt + click your target", Color.WHITE);
        PBotAPI.gui.map.registerGobSelect(this);
        while (selection == null) {
            PBotUtils.sleep(10);
        }
        isdead = selection.isDead();
        countgobs();
    }

    public void gobselect(Gob gob) {
        selection = gob;
        try {
            int i = selection.sdt();
            PBotUtils.sysMsg("Selection is " + selection.getres().name + " stage is " + i + " type is : " + selection.type+" gob id is "+selection.id, Color.white);
        }catch(Exception e){
            PBotUtils.sysMsg("Selection is " + selection.getres().name + " type is : " + selection.type+" gob id is "+selection.id, Color.white);
        }
        gui.map.unregisterGobSelect();

    }

    public void countgobs() {
        List<Gob> allGobs = PBotUtils.getGobs();
        List<Gob> list = new ArrayList<>();
       // BotUtils.sysMsg("Counting",Color.white);
        for (int i = 0; i < allGobs.size(); i++) {
            try {
                Resource res = selection.getres();
                Resource res2 = allGobs.get(i).getres();
                if (selection.getres().name.equals(allGobs.get(i).getres().name)){
                    if (allGobs.get(i).isDead() && selection.isDead())
                        list.add(allGobs.get(i));
                     else if (!allGobs.get(i).isDead() && !selection.isDead())
                        list.add(allGobs.get(i));
                }
            }catch(NullPointerException | Loading e){}

        }
       // int stage = selection.getattr(ResDrawable.class).spr.res.ver;
      //  String stage2 = selection.getattr(ResDrawable.class).spr.res.name;
      //  String stage3 = selection.getattr(ResDrawable.class).spr.res.indir().name;
      //  BotUtils.sysLogAppend("a : "+selection.a+" type: "+selection.type+" get stage : "+selection.getStage()+" V : "+selection.getv(),"white");
        PBotUtils.sysMsg("Total is - "+list.size(),Color.white);


    }

}



