package haven.purus.pbot;

import haven.Config;
import haven.automation.DiscordBot;
import integrations.mapv4.MappingClient;
import net.dv8tion.jda.core.EmbedBuilder;
import net.dv8tion.jda.core.entities.Member;
import net.dv8tion.jda.core.entities.TextChannel;
import net.dv8tion.jda.core.hooks.ListenerAdapter;

import java.awt.*;
import java.util.List;

public class PBotDiscord extends ListenerAdapter {
    static List<TextChannel> channels;

    public static void setChannelByName(String name) {
        channels = DiscordBot.getJda().getTextChannelsByName(name, true);
    }

    public static void sendMessage(String text) {
        initalize();
        if (channels != null) {
            for (TextChannel ch : channels) {
                ch.sendMessage(text).queue();
            }
        }
    }

    public static void initalize() {
        setChannelByName(Config.discordchannel);
    }

    public static void at(String contains){
        initalize();
        for (TextChannel ch : channels) {
            for(Member member : ch.getMembers()){
                if(member.getUser().getName().contains(contains)){
                    ch.sendMessage("<@"+member.getUser().getId()+">").queue();
                }
            }
        }
    }

    //Must use Caps for name, doesn't require #0000
    public static String getAt(String name){
        initalize();
        String at = "";
        for (TextChannel ch : channels) {
            for(Member member : ch.getMembers()){
                if(member.getUser().getName().equals(name)){
                    at = "<@"+member.getUser().getId()+">";
                }
            }
        }
        return at;
    }

    public static String getUserID(String name){
        initalize();
        String at = "";
        for (TextChannel ch : channels) {
            for(Member member : ch.getMembers()){
                if(member.getUser().getName().equals(name)){
                     at = member.getUser().getId();
                }
            }
        }
        return at;
    }

    public static void embedMessage(String title, String urlThumb, String urlPicture, String text, String color) {
        initalize();
        EmbedBuilder embed = new EmbedBuilder();
        if (title != null) {
            embed.setTitle(title);
        }
        if (urlThumb != null) {
            embed.setThumbnail(urlThumb);
        }
        if (urlPicture != null) {
            embed.setImage(urlPicture);
        }
        if (text != null) {
            embed.setDescription(text);
        }
        if (color != null) {
            switch (color) {
                case "red":
                    embed.setColor(Color.red);
                    break;
                case "blue":
                    embed.setColor(Color.blue);
                    break;
                case "green":
                    embed.setColor(Color.green);
                    break;
                case "black":
                    embed.setColor(Color.black);
                    break;
                case "white":
                    embed.setColor(Color.blue);
                    break;
                case "yellow":
                    embed.setColor(Color.yellow);
                    break;
                default:
                    embed.setColor(Color.black);
            }
        }
        for (TextChannel ch : channels) {
            ch.sendMessage(embed.build()).queue();
        }
    }

    public static void embedMessage(String title, String urlPicture, String text) {
        initalize();
        EmbedBuilder embed = new EmbedBuilder();
        if (title != null) {
            embed.setTitle(title);
        }
        if (urlPicture != null) {
            embed.setImage(urlPicture);
        }
        if (text != null) {
            embed.setDescription(text);
        }
        for (TextChannel ch : channels) {
            ch.sendMessage(embed.build()).queue();
        }
    }

    public static void mapAlert(String string){
        try {
            String base = MappingClient.getInstance().endpoint;
            String mod = base.split("/")[2];
            String output = String.format("@everyone " + string + " at: " + " http://" + mod
                    + "/map/#/grid/2/%d/%d/6", MappingClient.getInstance().lastMapRef.gc.x, MappingClient.getInstance().lastMapRef.gc.y);
            PBotDiscord.sendMessage(output);
        } catch (Exception e){
            e.printStackTrace();
        }
    }

    public static void mapAlert(String name, String string){
        try {
            String base = MappingClient.getInstance().endpoint;
            String mod = base.split("/")[2];
            String output = String.format(getAt(name) + " " + string + " at: " + " http://" + mod
                    + "/map/#/grid/2/%d/%d/6", MappingClient.getInstance().lastMapRef.gc.x, MappingClient.getInstance().lastMapRef.gc.y);
            PBotDiscord.sendMessage(output);
        } catch (Exception e){
            e.printStackTrace();
        }
    }

}
