package haven.purus.pbot;

import haven.Config;
import haven.automation.DiscordBot;
import integrations.mapv4.MappingClient;
import net.dv8tion.jda.core.EmbedBuilder;
import net.dv8tion.jda.core.entities.Member;
import net.dv8tion.jda.core.entities.Role;
import net.dv8tion.jda.core.entities.TextChannel;
import net.dv8tion.jda.core.hooks.ListenerAdapter;

import java.awt.Color;
import java.util.List;

/**
 * @author Vareckeska
 */
public class PBotDiscord extends ListenerAdapter {
    static List<TextChannel> channels;

    /***
     * Sets up JDA and retrieves discord channels.
     * @param name
     */
    public static void setChannelByName(String name) {
        channels = DiscordBot.getJda().getTextChannelsByName(name, true);
    }

    /***
     * Ensures JDA is set up and there is a connection and sets channel.
     */
    public static void initalize() {
        setChannelByName(Config.discordchannel);
    }

    /***
     * Sends a message.
     * @param text
     */
    public static void sendMessage(String text) {
        initalize();
        if (channels != null) {
            for (TextChannel ch : channels) {
                ch.sendMessage(text).queue();
            }
        }
    }

    /***
     * Alerts a user with @name.
     * @param name
     */
    public static void alertUser(String name) {
        initalize();
        for (TextChannel ch : channels) {
            for (Member member : ch.getMembers()) {
                if (member.getUser().getName().contains(name)) {
                    ch.sendMessage("<@" + member.getUser().getId() + ">").queue();
                }
            }
        }
    }

    /***
     * Gets the ID string to alert a user.
     * Must use Caps for name, doesn't require #0000 after username.
     * @param name
     * @return returns Discord alert string.
     */
    public static String getAlertString(String name) {
        initalize();
        String at = "";
        for (TextChannel ch : channels) {
            for (Member member : ch.getMembers()) {
                if (member.getUser().getName().equals(name)) {
                    at = "<@" + member.getUser().getId() + ">";

                }
            }
        }
        return at;
    }

    /***
     * Messages a single role.
     * @param id
     * @param message
     */
    public static void messageRole(String id, String message) {
        initalize();
        PBotDiscord.sendMessage("<@&" + id + ">" + " " + message);
    }

    /***
     * Gets the role id of a single player.
     * @param nameplayer
     * @param namerole
     * @return Returns role ID.
     */
    public static String getRoleID(String nameplayer, String namerole) {
        initalize();
        String at = "";
        for (TextChannel ch : channels) {
            for (Member member : ch.getMembers()) {
                if (member.getUser().getName().equals(nameplayer)) {
                    for (Role r : ch.getJDA().getRolesByName(namerole, true)) {
                        at = r.getId();
                    }
                }
            }
        }
        return at;
    }

    /***
     * Gets the ID of a user.
     * @param name
     * @return Returns user ID.
     */
    public static String getUserID(String name) {
        initalize();
        String at = "";
        for (TextChannel ch : channels) {
            for (Member member : ch.getMembers()) {
                if (member.getUser().getName().equals(name)) {
                    at = member.getUser().getId();
                }
            }
        }
        return at;
    }

    /***
     * Sends an embed message to users.
     * Each individual component can be null.
     */
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

    /***
     * Embed message with less arguements.
     */
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

    /***
     * Alerts everyone in discord channel with map location and item found.
     * @param string Specific item to alert the users to.
     */
    public static void mapAlertEveryone(String string) {
        try {
            String base = MappingClient.getInstance().endpoint;
            String mod = base.split("/")[2];
            String output = String.format("@everyone " + string + " at: " + " http://" + mod
                    + "/map/#/grid/2/%d/%d/6", MappingClient.getInstance().lastMapRef.gc.x, MappingClient.getInstance().lastMapRef.gc.y);
            PBotDiscord.sendMessage(output);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /***
     * Alerts discord user with map location and item found.
     * @param name The name of the user to be messaged.
     * @param string Specific item to alert the users to.
     */
    public static void mapAlert(String name, String string) {
        try {
            String base = MappingClient.getInstance().endpoint;
            String mod = base.split("/")[2];
            String output = String.format(getAlertString(name) + " " + string + " at: " + " http://" + mod
                    + "/map/#/grid/%d/%d/%d/6", MappingClient.getInstance().lastMapRef.mapID, MappingClient.getInstance().lastMapRef.gc.x, MappingClient.getInstance().lastMapRef.gc.y);
            PBotDiscord.sendMessage(output);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /***
     * Alerts discord role with map location and item found.
     * @param id ID of the user to alert.
     * @param string Specific item to alert the users to.
     */
    public static void mapAlertRole(String id, String string) {
        try {
            String base = MappingClient.getInstance().endpoint;
            String mod = base.split("/")[2];
            String output = String.format(string + " at: " + " http://" + mod
                    + "/map/#/grid/%d/%d/%d/6", MappingClient.getInstance().lastMapRef.mapID, MappingClient.getInstance().lastMapRef.gc.x, MappingClient.getInstance().lastMapRef.gc.y);
            PBotDiscord.messageRole(id, output);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}
