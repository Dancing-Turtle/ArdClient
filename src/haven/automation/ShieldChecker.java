package haven.automation;

import haven.Button;
import haven.*;
import haven.Label;
import haven.Window;
import net.dv8tion.jda.core.entities.TextChannel;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.util.*;
import java.util.List;

import static haven.OCache.posres;


public class ShieldChecker extends Window {
    private Thread runner;
    private final Label lblc, lblc2;
    private String curshield = "";
    private String curshield2 = "";
    private Button runbtn, stopbtn;
    private boolean terminate;
    ChatUI.Channel.Message sentmsg;
    private static final int SLEEP = 30 * 60 * 1000 * 2; // 60 min
    private static final int SLEEP5 = 1000*60*5; //5 min
    private boolean firstrun = true;
    private int iteration;
    private static final Text.Foundry infof = new Text.Foundry(Text.sans, 10).aa(true);

    public ShieldChecker() {
        super(new Coord(220, 180), "Shield Checker");

        final Label lbl = new Label("Click Run/Stop", infof);
        add(lbl, new Coord(0, 20));

        lblc = new Label("0", Text.num12boldFnd, Color.WHITE);
        add(lblc, new Coord(0, 70));
        lblc2 = new Label("0", Text.num12boldFnd, Color.WHITE);
        add(lblc2, new Coord(0, 85));

        runbtn = new Button(140, "Run") {
            @Override
            public void click() {
                this.hide();
                cbtn.hide();
                stopbtn.show();
                terminate = false;
             //   runner = new Thread(new Runner(), "Shield Checker");
              //  runner.start();
            }
        };
        add(runbtn, new Coord(0, 120));

        stopbtn = new Button(140, "Stop") {
            @Override
            public void click() {
                terminate = true;
                // TODO: terminate PF
                this.hide();
                runbtn.show();
                cbtn.show();
            }
        };
        add(stopbtn, new Coord(0,150));
        stopbtn.hide();
    }

  /* private class Runner implements Runnable {
        @Override
        public void run() {
            GameUI gui = gameui();
              while (!terminate) {
                  PBotAPI.doAct("inspect");
                  BotUtils.sleep(2000);
                  BotUtils.mapInteractLeftClick(0);
                  BotUtils.sleep(2000);
               //   BotUtils.sysMsg(ui.VillageShield, Color.white);
               //   lblc.settext("V Result : "+ui.VillageShield);
                //  lblc2.settext("P Result : "+ui.PrivateShield);
                  iteration++;
                  if(Discord.jdalogin!=null && !Config.AlertChannel.equals("Null")) {
                      for (TextChannel loop : haven.automation.Discord.channels) {
                          if (loop.getName().equals(Config.AlertChannel)) {
                              if(!ui.sess.alive()){
                                  loop.sendMessage("Shield Checker : I seem to have disconnected, disabling alerts. Please reconnect me at your earliest convenience.").queue();
                                  stopbtn.click();
                                  terminate = true;
                                  return;
                              }
                              else if (!firstrun) {
                                  if (curshield.equals(ui.VillageShield) && curshield2.equals(ui.PrivateShield) && iteration == 14) {
                                      iteration = 1;
                                      if(!curshield.equals("") && !curshield2.equals("")) //village and personal
                                      loop.sendMessage("Shield Checker: Village result was : " + ui.VillageShield+ " and personal result was : "+ui.PrivateShield).queue();
                                      else if(!curshield.equals("") && curshield2.equals("")) // village only
                                          loop.sendMessage("Shield Checker: Village result was : " + ui.VillageShield).queue();
                                      else if(curshield.equals("") && !curshield2.equals("")) //personal only
                                          loop.sendMessage("Shield Checker: Personal result was : " + ui.PrivateShield).queue();
                                  }
                                  else if(!curshield.equals(ui.VillageShield) || !curshield2.equals(ui.PrivateShield)) {
                                      if (!curshield.equals("") && !curshield2.equals("")) //village and personal
                                          loop.sendMessage("@everyone Shield result changed! Village result was : " + curshield + "is now : " + ui.VillageShield+" and personal result was : " + curshield2 + " is now : " + ui.PrivateShield).queue();
                                      else if (!curshield.equals("") && curshield2.equals("")) // village only
                                          loop.sendMessage("@everyone Shield result changed! Village result was : " + curshield + " is now  : " + ui.VillageShield).queue();
                                      else if (curshield.equals("") && !curshield2.equals("")) //personal only
                                          loop.sendMessage("@everyone Shield result changed! Personal result was : " + curshield2 + " is now : " + ui.PrivateShield).queue();
                                  }
                              } else {
                                  if(!ui.VillageShield.equals("") && !ui.PrivateShield.equals("")) //print both shields
                                  loop.sendMessage("First Run Shield Checker: Village result was : " + ui.VillageShield + " and Personal result was : "+ui.PrivateShield).queue();
                                  else if(!ui.VillageShield.equals("") && ui.PrivateShield.equals("")) // print only village shield
                                      loop.sendMessage("First Run Shield Checker: Village result was : " + ui.VillageShield).queue();
                                  else if(ui.VillageShield.equals("") && !ui.PrivateShield.equals("")) //print only personal shield
                                      loop.sendMessage("First Run Shield Checker: Personal result was : " + ui.PrivateShield).queue();
                                  firstrun = false;
                                  iteration = 2;
                              }
                          }
                      }
                  }
                  else
                      BotUtils.sysLogAppend("Checked shield but Discord is not connected, unable to send result.","white");
                  curshield = ui.VillageShield;
                  if(terminate)
                      return;
                  try{
                      Thread.sleep(SLEEP5);
                  }catch(InterruptedException q){}
              }
        }
        }*/
    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (sender == cbtn)
            reqdestroy();
        else if(msg.equals("terminate"))
        {
            for(TextChannel loop:haven.automation.Discord.channels)
                if (Config.AlertChannel.equals(loop.getName())) {
                    loop.sendMessage(gameui().getparent(GameUI.class).buddies.getCharName() + ": Detected stop command, disabling shield monitoring.").queue();
                    break;
                }
            terminate = true;
            reqdestroy();
        }
            super.wdgmsg(sender, msg, args);
    }

    }


