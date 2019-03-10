package haven;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class QuestListItem implements Comparable<QuestListItem>{
    public String name;
    public int status;
    public int parentid;
    public Coord2d coord = null;
    public String questGiver = "";

    public QuestListItem(String name, int status, int parentid) {
        this.name = name;
        this.status = status;
        this.parentid = parentid;
        Pattern p = Pattern.compile("[A-Z].*?([A-Z].*?)\\b.*?");
        Matcher m = p.matcher(name);
        if (m.find()) {
            this.questGiver = m.group(1);
        }

    }

    public int compareTo(QuestListItem o) {
        return this.name.compareTo(o.name);
    }
}
