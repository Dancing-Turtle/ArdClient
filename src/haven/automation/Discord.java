package haven.automation;

import haven.*;
import haven.purus.BotUtils;
import net.dv8tion.jda.core.AccountType;
import net.dv8tion.jda.core.JDA;
import net.dv8tion.jda.core.JDABuilder;
import net.dv8tion.jda.core.entities.Message;
import net.dv8tion.jda.core.entities.MessageChannel;
import net.dv8tion.jda.core.entities.User;
import net.dv8tion.jda.core.events.ReadyEvent;
import net.dv8tion.jda.core.events.message.MessageReceivedEvent;
import net.dv8tion.jda.core.hooks.ListenerAdapter;
import java.util.LinkedList;

import javax.security.auth.login.LoginException;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class Discord extends ListenerAdapter implements Runnable {

    public GameUI gui;

    public Discord(GameUI gui) {
        this.gui = gui;
    }

    public ChatUI.MultiChat Discord;
    public static int delay = 2000;
    public static String pendingmsg;
    public static boolean readytogo;
    public static MessageChannel channelfinal;
    public JDA jda;
    public MessageChannel channel;
    public String Camelot;
    public static JDA jdalogin;



   /* public void run() {
        System.out.println("Discord Loaded");
        BotUtils.sysLogAppend("Discord Loaded", "white");
        Thread t = new Thread(new Discord.Main());
        t.start();
    }*/

        public void run () {
            pendingmsg = null;
            try {
                JDABuilder builder = new JDABuilder(AccountType.BOT);
               // builder.setToken("NDUyMzM1MDYzNzAzNDg2NDc0.Dkvu1Q.In7-lxo-fwDSM5i13mRhtnlbFsU");
                builder.setToken(Resource.getLocString(Resource.BUNDLE_LABEL, Config.discordbotkey));
                builder.addEventListener(new Discord(gui));
                JDA jda = builder.buildBlocking();
            } catch (LoginException e) {
                e.printStackTrace();
                BotUtils.sysLogAppend(e.getMessage(),"white");
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            ActionListener checkMSG = new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    if (pendingmsg != null && readytogo) {
                         jdalogin.getTextChannelById(Resource.getLocString(Resource.BUNDLE_LABEL, Config.discordchannel)).sendMessage(pendingmsg).queue();
                         pendingmsg = null;
                    }
                }
            };
            new Timer(delay,checkMSG).start();
        }
        @Override
        public void onReady(ReadyEvent event) {
            System.out.println("Ready!");
            BotUtils.sysMsg("Discord Loaded",Color.white);
            readytogo = true;
            jdalogin = event.getJDA();
        }

        @Override
        public void onMessageReceived(MessageReceivedEvent event) {

            jda = event.getJDA();

            long responseNumber = event.getResponseNumber();

            User author = event.getAuthor();

            Message message = event.getMessage();

            channel = event.getChannel();

            String channelname = channel.getName();

            String msg = message.getContentDisplay();
            channelfinal = event.getChannel();

            System.out.println("Channelfinal = : "+channelfinal.getName());



            String[] SplitMessage = msg.split(" ");

            boolean bot = author.isBot();


            if (!author.isBot()) {

                    gui.Discord.append(channel.getName()+" - "+author.getName() + ":  " + msg, Color.green);
                    ChatUI.Channel.Message sendmsg = new ChatUI.Channel.SimpleMessage(channel.getName()+" - "+author.getName() + ": " + msg, Color.green, 0);
                    gui.chat.notify(gui.Discord, sendmsg);
                    gui.Discord.updurgency(1);
                }

            }
        }







