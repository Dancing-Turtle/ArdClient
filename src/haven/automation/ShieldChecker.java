package haven.automation;

import haven.Button;
import haven.*;
import haven.Label;
import haven.Window;
import haven.purus.BotUtils;
import haven.purus.pbot.PBotAPI;
import net.dv8tion.jda.core.entities.TextChannel;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.util.*;
import java.util.List;

import static haven.OCache.posres;


public class ShieldChecker extends Window {
    private Thread runner;
    private final Label lblc;
    private String curshield = "";
    private Button runbtn, stopbtn;
    private boolean terminate;
    ChatUI.Channel.Message sentmsg;
    private static final int SLEEP = 30 * 60 * 1000 * 2; // 60 min
    private boolean firstrun = true;
    private static final Text.Foundry infof = new Text.Foundry(Text.sans, 10).aa(true);

    public ShieldChecker() {
        super(new Coord(220, 180), "Shield Checker");

        final Label lbl = new Label("Click Run/Stop", infof);
        add(lbl, new Coord(0, 20));

        lblc = new Label("0", Text.num12boldFnd, Color.WHITE);
        add(lblc, new Coord(0, 70));

        runbtn = new Button(140, "Run") {
            @Override
            public void click() {
                this.hide();
                cbtn.hide();
                stopbtn.show();
                terminate = false;
                runner = new Thread(new Runner(), "Shield Checker");
                runner.start();
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

    private class Runner implements Runnable {
        @Override
        public void run() {
            GameUI gui = gameui();
              while (!terminate) {
                  PBotAPI.doAct("inspect");
                  BotUtils.sleep(1000);

                //  Gob player = BotUtils.player();
                //  gui.map.wdgmsg("click", player.sc, player.rc.floor(posres), 1, 0, 0, (int) player.id, player.rc.floor(posres), 1, 0);
                  BotUtils.mapInteractLeftClick(0);
                  BotUtils.sleep(2000);
               //   BotUtils.sysMsg(ui.VillageShield, Color.white);
                  lblc.settext("Result : "+ui.VillageShield);
                  if(Discord.jdalogin!=null && !Config.AlertChannel.equals("Null")) {
                      for (TextChannel loop : haven.automation.Discord.channels) {
                          if (loop.getName().equals(Config.AlertChannel)) {
                              if (!firstrun) {
                                  if (!curshield.equals(ui.VillageShield))
                                      loop.sendMessage("Shield Checker: Check result was : " + ui.VillageShield).queue();
                                  else
                                      loop.sendMessage("Shield Checker: everyone Check result changed! Is now : " + ui.VillageShield + " old value was : " + curshield).queue();
                              } else {
                                  loop.sendMessage("Shield Checker: Check result was : " + ui.VillageShield).queue();
                                  firstrun = false;
                              }
                          }
                      }
                  }
                  else
                      BotUtils.sysLogAppend("Checked shield but Discord is not connected, unable to send result.","white");
                  curshield = ui.VillageShield;
                  try{
                      Thread.sleep(SLEEP);
                  }catch(InterruptedException q){}
              }
        }
        }

    }


