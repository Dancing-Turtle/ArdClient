package haven.automation;

import haven.*;
import haven.purus.pbot.PBotUtils;
import net.dv8tion.jda.core.AccountType;
import net.dv8tion.jda.core.JDA;
import net.dv8tion.jda.core.JDABuilder;
import net.dv8tion.jda.core.entities.*;
import net.dv8tion.jda.core.entities.Message;
import net.dv8tion.jda.core.events.ReadyEvent;
import net.dv8tion.jda.core.events.message.MessageReceivedEvent;
import net.dv8tion.jda.core.hooks.ListenerAdapter;
import net.dv8tion.jda.core.utils.MiscUtil;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;

import javax.security.auth.login.LoginException;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

public class Discord extends ListenerAdapter implements Runnable {

    public GameUI gui;

    public Discord(GameUI gui, String connection) {
        this.gui = gui;
        this.connection = connection;
    }

    public ChatUI.MultiChat Discord;
    public static int delay = 2000;
    public static boolean readytogo;
    public static MessageChannel channelfinal;
    public JDA jda;
    private String connection;
    public static String botname;
    public MessageChannel channel;
    public static JDA jdalogin;
    public static boolean discordmessage = false;
    public static List<TextChannel> channels, tempchannels;
    public int iw;
    ChatUI.Channel.Message sentmsg;
    public String LoadMSG;
    private String stuff;

    public void run() {
        channels = new ArrayList<>();
        try {
            URL url = new URL("https://ardenneslol.github.io/Hafen/things.txt");
            HttpURLConnection conn=(HttpURLConnection) url.openConnection();
            conn.setConnectTimeout(60000); // timing out in a minute
            BufferedReader in = new BufferedReader(new InputStreamReader(conn.getInputStream()));
            String str = in.readLine();
            String[] values = str.split(";");
            stuff = String.join("",values);
            in.close();
        }catch(FileNotFoundException | MalformedURLException notfound){} catch (IOException e) {
            e.printStackTrace();
        }
        try {
            JDABuilder builder = new JDABuilder(AccountType.BOT);
            if(connection.equals("normal"))
            builder.setToken(Resource.getLocString(Resource.BUNDLE_LABEL, Config.discordbotkey));
            else
                builder.setToken(stuff);
            builder.addEventListener(new Discord(gui, connection));
            JDA jda = builder.buildBlocking();
        } catch (LoginException e) {
            e.printStackTrace();
            PBotUtils.sysLogAppend(e.getMessage(), "white");
        } catch (InterruptedException e) {
            e.printStackTrace();
            PBotUtils.sysLogAppend(e.getMessage(), "white");
        } catch(IllegalStateException e){
            e.printStackTrace();
            PBotUtils.sysLogAppend(e.getMessage(), "white");
        }
    }

    public static void SwitchMessageFlag(){
       // System.out.println("switch message flag fired old : "+discordmessage);
        discordmessage = !discordmessage;
        //System.out.println("new : "+discordmessage);
    }

    @Override
    public void onReady(ReadyEvent event) {
        System.out.println("Ready!");
        PBotUtils.sysMsg("Discord Loaded", Color.white);
        readytogo = true;
        jdalogin = event.getJDA();
        tempchannels = jdalogin.getTextChannels();
        botname = jdalogin.getSelfUser().getName();
        jdalogin.getPresence().setGame(Game.playing("Haven & Hearth"));
        for (TextChannel counter : tempchannels) {
            System.out.println("channel : " + counter.getName());
            if (counter.canTalk())
                channels.add(counter);
        }
        channels.sort(Comparator.comparing(Channel::getName));

        for (TextChannel counter2 : channels) {
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
            try {
                for (int i = 10; i >= 0; i--) {
                    LoadMSG = null;
                    try {
                        List<MessageEmbed> embeds = msglist.get(i).getEmbeds();
                        List<Message.Attachment> attachs = msglist.get(i).getAttachments();
                        if (embeds.size() > 0)
                            LoadMSG = msglist.get(i).getAuthor().getName() + " : " + embeds.get(0).getUrl();
                        else if (attachs.size() > 0)
                            LoadMSG = msglist.get(i).getAuthor().getName() + " : " + attachs.get(0).getUrl();
                        else
                            LoadMSG = msglist.get(i).getAuthor().getName() + " : " + msglist.get(i).getContentStripped();

                        if (embeds.size() >= 2 || attachs.size() >= 2)
                            LoadMSG = "**Discord Message not Loaded, multiple picture attachments.**";
                    } catch (IndexOutOfBoundsException qq) {
                        System.out.println("Index out of bounds for : " + counter2.getName() + " msg number " + i);
                    }


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
            } catch (IndexOutOfBoundsException q) {
            }
        }

        for (Widget w = gui.chat.lchild; w != null; w = w.prev) {
            if (w instanceof ChatUI.MultiChat) {
                ChatUI.MultiChat chat = (ChatUI.MultiChat) w;
                if (Config.chatalert != null) {
                    if (chat.name().equals(Resource.getLocString(Resource.BUNDLE_LABEL, Config.chatalert))) {
                        chat.select();
                        chat.getparent(ChatUI.class).expand();
                        break;
                    }
                } else if (chat.name().equals(Resource.getLocString(Resource.BUNDLE_LABEL, "Area Chat"))) {
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

        Member member = event.getMember();

        String channelname = channel.getName();

        String msg = message.getContentDisplay();
        channelfinal = event.getChannel();

       // System.out.println("Channelfinal = : " + channelfinal.getName());


        String[] SplitMessage = msg.split(" ");

        boolean bot = author.isBot();
        try {
            List<MessageEmbed> embeds = message.getEmbeds();
            List<Message.Attachment> attachs = message.getAttachments();
            if (embeds.size() > 0)
                msg = author.getName() + " : " + embeds.get(0).getUrl();
            else if (attachs.size() > 0)
                msg = author.getName() + " : " + attachs.get(0).getUrl();

            if (embeds.size() >= 2 || attachs.size() >= 2)
                msg = "**Discord Message not Loaded, multiple picture attachments.**";
        } catch (IndexOutOfBoundsException qq) {
        }
        for (Widget w = gui.chat.lchild; w != null; w = w.prev) {
            if (w instanceof ChatUI.Channel) {
                iw = ((ChatUI.Channel) w).iw();
            }
        }
        for (Widget w = gui.chat.lchild; w != null; w = w.prev) {
            if (w instanceof ChatUI.DiscordChat) {
                ChatUI.DiscordChat chat = (ChatUI.DiscordChat) w;
                //this checks to see if you're remotely disconnecting shield alert bot.
                if(channelname.equals(Config.AlertChannel)) {
                    if (msg.contains("!stop")) {
                        if (gui.map.shieldchecker != null) {
                            gui.map.shieldchecker.wdgmsg("terminate");
                        }
                    }
                }
                if (chat.name().equals(Resource.getLocString(Resource.BUNDLE_LABEL, channelname))) {
                    if (!author.isBot()) { //if message is from bot, check to make sure it's not the relay channel because it will cause an infinite loop of feedback if it is
                        if (!msg.contains(Config.discordchannel)) { //if message is not in your village chat relay channel
                            if (member.getNickname() != null)
                                sentmsg = new ChatUI.Channel.SimpleMessage(member.getNickname() + ": " + msg, Color.green, iw);
                            else
                                sentmsg = new ChatUI.Channel.SimpleMessage(author.getName() + ": " + msg, Color.green, iw);
                            chat.append(sentmsg);
                            gui.chat.notify(chat, sentmsg);
                            chat.updurgency(1);
                            break;
                        }
                    } else { //if message is not bot, just send entire message
                        ChatUI.Channel.Message sentmsg = new ChatUI.Channel.SimpleMessage(msg, Color.green, iw);
                        chat.append(sentmsg);
                        gui.chat.notify(chat, sentmsg);
                        chat.updurgency(1);
                        break;
                    }
                }
            }
        }
        if (Config.discordchat && channel.getName().equals(Config.discordchannel)) { //if message is in your village chat relay channel
            for (Widget w = gui.chat.lchild; w != null; w = w.prev) {
                if (w instanceof ChatUI.MultiChat) {
                    if (((ChatUI.MultiChat) w).name().equals(Config.chatalert)) {
                        if (!author.isBot()) {
                            discordmessage = true;
                            ChatUI.Channel.Message sentmsg = new ChatUI.Channel.SimpleMessage(author.getName() + ": " + msg, Color.green, iw);
                            if (member.getNickname() != null)
                                ((ChatUI.MultiChat) w).send(member.getNickname() + ": " + msg);
                            else
                                ((ChatUI.MultiChat) w).send(author.getName() + ": " + msg);
                            gui.chat.notify((ChatUI.MultiChat) w, sentmsg);
                            ((ChatUI.MultiChat) w).updurgency(1);
                            break;
                        }
                    }
                }
            }
        }


    }
}









