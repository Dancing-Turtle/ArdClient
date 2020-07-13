package haven.automation;

import haven.Config;
import net.dv8tion.jda.core.AccountType;
import net.dv8tion.jda.core.JDA;
import net.dv8tion.jda.core.JDABuilder;
import net.dv8tion.jda.core.OnlineStatus;
import net.dv8tion.jda.core.entities.Game;

public class DiscordBot {
    static JDA jda;

    public static JDA getJda(){
        if(jda == null){
            try {
                jda = new JDABuilder(AccountType.BOT).setToken(Config.discordtoken).buildAsync();
                jda.getPresence().setStatus(OnlineStatus.ONLINE);
                jda.getPresence().setGame(Game.playing("Revived Bot"));
            } catch (Exception e){
                System.out.println("Invalid Token");
            }
            return jda;
        } else {
            return jda;
        }
    }
}









