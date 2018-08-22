package haven.automation;


import haven.Button;
import haven.*;
import haven.Window;
import haven.purus.BotUtils;
import haven.purus.pbot.PBotAPI;

import java.awt.*;
import java.awt.event.KeyEvent;

import static haven.OCache.posres;

public class CraftAllBot extends Window implements GobSelectCallback{
    private GameUI gui;
    private UI ui;
    public Widget craftall;
    private static final Text.Foundry infof = new Text.Foundry(Text.sans, 10).aa(true);
    private static final int TIMEOUT_ACT = 6000;
    public boolean terminate = false;
    private Button runbtn, stopbtn, clrbtn, cauldbtn;
    private static final int TIMEOUT = 2000;
    private static final int HAND_DELAY = 8;
    private Thread runner;
    public Gob barrel;
    public Gob cauldron;
    public Gob cistern;
    public Boolean usecauldron = false;
    public Window cwnd = null;




public CraftAllBot(GameUI gui){
    super(new Coord(160, 110), "CraftAllBot");

    clrbtn = new Button(140, "Clear Selection") {
        @Override
        public void click() {
            barrel = null;
            cauldron = null;
        }
    };
    add(clrbtn, new Coord(10, 40));


    runbtn = new Button(140, "Run") {
        @Override
        public void click() {
            this.hide();
            cbtn.hide();
            stopbtn.show();
            clrbtn.hide();
            terminate = false;
            runner = new Thread(new CraftAllBot.Runner(), "CraftAllBot");
            runner.start();
        }
    };
    add(runbtn, new Coord(10, 10));

   /* cauldbtn = new Button(140, "Cauldron") {
        @Override
        public void click() {
            terminate = false;
            if(usecauldron)
            usecauldron = false;
            else
                usecauldron = true;
            BotUtils.sysMsg("Cauldron usage is : "+usecauldron,Color.white);
        }
    };
    add(cauldbtn, new Coord(10, 70));*/

    stopbtn = new Button(140, "Stop") {
        @Override
        public void click() {
            if(runner != null)
                runner.interrupt();
            terminate = true;
            // TODO: terminate PF
            //terminate();
            this.hide();
            runbtn.show();
            cbtn.show();
            clrbtn.show();
        }
    };
    stopbtn.hide();
    add(stopbtn, new Coord(10, 10));
}

public class Runner implements Runnable {
    @Override
    public void run() {
        GameUI gui = gameui();


            Window crafting = gui.getwnd("Crafting");
            if(crafting == null){
                BotUtils.sysMsg("Craft Window not open, open and retry.",Color.white);
                stopbtn.click();
                return;
            }
            for (Widget a = crafting.lchild; a != null; a = a.prev) {
                for (Widget aa = a.lchild; aa != null; aa = aa.prev) {
                    if (aa instanceof Button) {
                        if (((Button) aa).text.text == "Craft All") {
                            craftall = aa;
                            ((Button) aa).click();
                            //  BotUtils.sysMsg("Craft All Found",Color.white);
                        }
                    }
                }
            }
            int iterations = 0;
            while (!terminate) {
                if(iterations > 50)
                    stopbtn.click();
                if (cauldron != null && !terminate) {
                    try {
                        // BotUtils.sysLogAppend("before cwnd check","white");
                         int cwndtimeout = 0;
                        while (cwnd == null) {
                           //  BotUtils.sysLogAppend("inside cwnd check","white");
                            cwnd = gui.getwnd("Cauldron");
                            cwndtimeout ++;
                            if (cwnd != null)
                                break;
                            if(cwndtimeout > 50000)
                                stopbtn.click();
                        }
                        cwndtimeout = 0;
                        //  BotUtils.sysLogAppend("after cwnd check","white");
                        VMeter vm = cwnd.getchild(VMeter.class);
                        IMeter vm2 = cwnd.getchild(IMeter.class);
                        if (vm.amount < 30 && !terminate) {
                            Coord retain = barrel.rc.floor(posres);
                            if (barrel.ols.size() == 0 && cistern != null) {
                                PBotAPI.liftGob(barrel);
                                BotUtils.sleep(1000);
                                gui.map.wdgmsg("click", cistern.sc, cistern.rc.floor(posres), 3, 0, 0, (int) cistern.id, cistern.rc.floor(posres), 0, -1);
                                BotUtils.sleep(3500);
                                gui.map.wdgmsg("click", cauldron.sc, cauldron.rc.floor(posres), 3, 0, 0, (int) cauldron.id, cauldron.rc.floor(posres), 0, -1);
                                BotUtils.sleep(1000);
                                gui.map.wdgmsg("click", Coord.z, retain, 3, 0);
                                BotUtils.sleep(1000);
                                gui.map.wdgmsg("click", cauldron.sc, cauldron.rc.floor(posres), 3, 0, 0, (int) cauldron.id, cauldron.rc.floor(posres), 0, -1);
                                BotUtils.sleep(1000);
                                ((Button) craftall).click();
                                BotUtils.sleep(1000);
                            }
                            else {
                                PBotAPI.liftGob(barrel);
                                BotUtils.sleep(1000);
                                gui.map.wdgmsg("click", cauldron.sc, cauldron.rc.floor(posres), 3, 0, 0, (int) cauldron.id, cauldron.rc.floor(posres), 0, -1);
                                BotUtils.sleep(1000);
                                gui.map.wdgmsg("click", Coord.z, retain, 3, 0);
                                BotUtils.sleep(1000);
                                gui.map.wdgmsg("click", cauldron.sc, cauldron.rc.floor(posres), 3, 0, 0, (int) cauldron.id, cauldron.rc.floor(posres), 0, -1);
                                BotUtils.sleep(1000);
                                ((Button) craftall).click();
                                BotUtils.sleep(1000);
                            }
                        }
                    } catch (NullPointerException i) {
                        runner.interrupt();
                        stopbtn.click();
                    }
                }
                IMeter.Meter stam = gui.getmeter("stam", 0);
                if (gui.prog == -1 && !terminate) {
                  //  BotUtils.sysLogAppend("inside drink","white");
                    Thread i = new Thread(new BeltDrink(gui), "BeltDrink");
                    i.start();
                    BotUtils.sleep(500);
                    while(gui.prog >= 0)
                        BotUtils.sleep(10);
                }
                while (gui.prog >= 0 && !terminate) {
                    iterations = 0;
                  //  BotUtils.sysLogAppend("sleeping during craft","white");
                    BotUtils.sleep(10);
                }
                if (stam.a > 50 && !terminate)
                    ((Button) craftall).click();
                else {
                    Thread i = new Thread(new BeltDrink(gui), "BeltDrink");
                    i.start();
                }
               // BotUtils.sysLogAppend("end","white");
                iterations ++;
              //  BotUtils.sysLogAppend("iterations : "+iterations,"white");
                cwnd = null;
            }
    }
}

    private void registerGobSelect() {
        synchronized (GobSelectCallback.class) {
            BotUtils.sysMsg("Registering Gob", Color.white);
            BotUtils.gui.map.registerGobSelect(this);
        }
    }

    public void gobselect(Gob gob) {
        Resource res = gob.getres();
        if (res != null) {
            if (res.name.equals("gfx/terobjs/barrel")) {
                BotUtils.sysMsg("Barrel selected!",Color.white);
                    barrel = gob;
                }
            if(res.name.equals("gfx/terobjs/cauldron")){
                BotUtils.sysMsg("Cauldron selected!",Color.white);
                cauldron = gob;
            }if(res.name.equals("gfx/terobjs/cistern") || res.name.equals("gfx/terobjs/well")){
                BotUtils.sysMsg("Water source selected!",Color.white);
                cistern = gob;
            }
        }
    }
    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (sender == cbtn)
            reqdestroy();
        else
            super.wdgmsg(sender, msg, args);
    }

    @Override
    public boolean type(char key, KeyEvent ev) {
        if (key == 27) {
            if (cbtn.visible)
                reqdestroy();
            return true;
        }
        return super.type(key, ev);
    }

    public void terminate() {
        terminate = true;
        if (runner != null)
            runner.interrupt();
       this.destroy();
    }

    public void liftGob(Gob gob) {
        //gui.wdgmsg("act", new Object[] { "Lift" });
        gui.act(0,Coord.z,gob,"Lift");
        PBotAPI.doClick(gob, 1, 0);
    }
}
