package haven.automation;

import haven.*;
import haven.purus.BotUtils;
import net.dv8tion.jda.core.AccountType;
import net.dv8tion.jda.core.JDA;
import net.dv8tion.jda.core.JDABuilder;
import net.dv8tion.jda.core.entities.*;
import net.dv8tion.jda.core.entities.Message;
import net.dv8tion.jda.core.events.ReadyEvent;
import net.dv8tion.jda.core.events.message.MessageReceivedEvent;
import net.dv8tion.jda.core.hooks.ListenerAdapter;
import net.dv8tion.jda.core.utils.MiscUtil;

import java.util.*;

import javax.security.auth.login.LoginException;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

public class Discord extends ListenerAdapter implements Runnable {

    public GameUI gui;

    public Discord(GameUI gui) {
        this.gui = gui;
    }

    public ChatUI.MultiChat Discord;
    public static int delay = 2000;
    public static boolean readytogo;
    public static MessageChannel channelfinal;
    public JDA jda;
    public MessageChannel channel;
    public static JDA jdalogin;
    public static List<TextChannel> channels, tempchannels;
    public int iw;
    public String LoadMSG;



   /* public void run() {
        System.out.println("Discord Loaded");
        BotUtils.sysLogAppend("Discord Loaded", "white");
        Thread t = new Thread(new Discord.Main());
        t.start();
    }*/

        public void run () {
           // pendingmsg = null;
            channels = new ArrayList<>();
            try {
                JDABuilder builder = new JDABuilder(AccountType.BOT);
                builder.setToken(Resource.getLocString(Resource.BUNDLE_LABEL, Config.discordbotkey));
                builder.addEventListener(new Discord(gui));
                JDA jda = builder.buildBlocking();
            } catch (LoginException e) {
                e.printStackTrace();
                BotUtils.sysLogAppend(e.getMessage(),"white");
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

           /* ActionListener checkMSG = new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    if (pendingmsg != null && readytogo) {
                         jdalogin.getTextChannelById(Resource.getLocString(Resource.BUNDLE_LABEL, Config.discordchannel)).sendMessage(pendingmsg).queue();
                         pendingmsg = null;
                    }
                }
            };
            new Timer(delay,checkMSG).start();*/
        }
        @Override
        public void onReady(ReadyEvent event) {
            System.out.println("Ready!");
            BotUtils.sysMsg("Discord Loaded",Color.white);
            readytogo = true;
            jdalogin = event.getJDA();
           tempchannels = jdalogin.getTextChannels();
            jdalogin.getPresence().setGame(Game.playing("Haven & Hearth"));
           for(TextChannel counter:tempchannels) {
               System.out.println("channel : " + counter.getName());
               if (counter.canTalk())
                   channels.add(counter);
           }
            //channels.sort.((object1, object2) -> object1.getName().compareTo(object2.getName()));
           channels.sort(Comparator.comparing(Channel::getName));

            for(TextChannel counter2:channels) {
                gui.chat.add(new ChatUI.DiscordChat(true, counter2.getName(), 2));
                long timestamp = System.currentTimeMillis(); // or any other epoch millis timestamp
                String discordTimestamp = Long.toUnsignedString(MiscUtil.getDiscordTimestamp(timestamp));
                MessageHistory history = MessageHistory.getHistoryBefore(counter2, discordTimestamp).complete();
                List<Message> msglist = history.getRetrievedHistory();
                for (Widget w = gui.chat.lchild; w != null; w = w.prev) {
                    if (w instanceof ChatUI.Channel) {
                        iw = ((ChatUI.Channel) w).iw();
                    }
                }
               // System.out.println("msglist size for : "+counter2.getName()+" is : "+msglist.size());
                try {
                    for (int i = 10; i >=0; i--) {
                        LoadMSG = null;
                        try {
                            List<MessageEmbed> embeds = msglist.get(i).getEmbeds();
                            List<Message.Attachment> attachs = msglist.get(i).getAttachments();
                            if (embeds.size() > 0)
                                LoadMSG = msglist.get(i).getAuthor().getName()+" : "+embeds.get(0).getUrl();
                            else
                                if(attachs.size() > 0)
                                LoadMSG = msglist.get(i).getAuthor().getName()+" : "+attachs.get(0).getUrl();
                            else
                                LoadMSG = msglist.get(i).getAuthor().getName()+" : "+msglist.get(i).getContentStripped();

                            if(embeds.size() >= 2 || attachs.size() >= 2)
                                LoadMSG = "**Discord Message not Loaded, multiple picture attachments.**";
                          //  System.out.println("Channel : "+counter2.getName()+" Msg : "+LoadMSG+" Embeds Size : "+embeds.size()+" Attachments size : "+attachs.size());
                        }catch(IndexOutOfBoundsException qq){System.out.println("Index out of bounds for : "+counter2.getName()+" msg number "+i);}


                        for (Widget w = gui.chat.lchild; w != null; w = w.prev) {
                            if (w instanceof ChatUI.DiscordChat) {
                                ChatUI.DiscordChat chat = (ChatUI.DiscordChat) w;
                                if (chat.name().equals(Resource.getLocString(Resource.BUNDLE_LABEL, counter2.getName()))) {
                                        ChatUI.Channel.Message sentmsg = new ChatUI.Channel.SimpleMessage(LoadMSG, Color.white, iw);
                                        chat.append(sentmsg);
                                        break;
                                    }
                                }
                            }
                        }
                }catch(IndexOutOfBoundsException q){}
            }

            for (Widget w = gui.chat.lchild; w != null; w = w.prev) {
                if (w instanceof ChatUI.MultiChat) {
                    ChatUI.MultiChat chat = (ChatUI.MultiChat) w;
                    if(Config.chatalert != null) {
                        if (chat.name().equals(Resource.getLocString(Resource.BUNDLE_LABEL, Config.chatalert))) {
                            chat.select();
                            chat.getparent(ChatUI.class).expand();
                            break;
                        }
                    }
                    else
                    if (chat.name().equals(Resource.getLocString(Resource.BUNDLE_LABEL, "Area Chat"))) {
                        chat.select();
                        chat.getparent(ChatUI.class).expand();
                        break;
                    }
                }
            }

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
            try {
                List<MessageEmbed> embeds = message.getEmbeds();
                List<Message.Attachment> attachs = message.getAttachments();
                if (embeds.size() > 0)
                    msg = author.getName()+" : "+embeds.get(0).getUrl();
                else
                    if(attachs.size() > 0 )
                        msg = author.getName()+" : "+attachs.get(0).getUrl();

                if(embeds.size() >= 2 || attachs.size() >= 2)
                    msg = "**Discord Message not Loaded, multiple picture attachments.**";
            }catch(IndexOutOfBoundsException qq){}

           // if (!author.isBot()) {
            for (Widget w = gui.chat.lchild; w != null; w = w.prev) {
                if (w instanceof ChatUI.Channel) {
                     iw = ((ChatUI.Channel) w).iw();
                }
            }
                for (Widget w = gui.chat.lchild; w != null; w = w.prev) {
                    if (w instanceof ChatUI.DiscordChat) {
                        ChatUI.DiscordChat chat = (ChatUI.DiscordChat) w;
                            if (chat.name().equals(Resource.getLocString(Resource.BUNDLE_LABEL, channelname))) {
                                if(!author.getName().contains("Haven")) {
                                    ChatUI.Channel.Message sentmsg = new ChatUI.Channel.SimpleMessage(author.getName() + ": " + msg, Color.green, iw);
                                    chat.append(sentmsg);
                                    gui.chat.notify(chat, sentmsg);
                                    chat.updurgency(1);
                                    break;
                                }else {
                                    ChatUI.Channel.Message sentmsg = new ChatUI.Channel.SimpleMessage(msg, Color.green, iw);
                                    chat.append(sentmsg);
                                    gui.chat.notify(chat, sentmsg);
                                    chat.updurgency(1);
                                    break;
                                }
                            }
                        }
                    }
              //  }

                   // gui.Discord.append(channel.getName()+" - "+author.getName() + ":  " + msg, Color.green);
                   // ChatUI.Channel.Message sendmsg = new ChatUI.Channel.SimpleMessage(channel.getName()+" - "+author.getName() + ": " + msg, Color.green, 0);
                   // gui.chat.notify(gui.Discord, sendmsg);
                  //  gui.Discord.updurgency(1);
                }
                public void kill(){

                }

            }









