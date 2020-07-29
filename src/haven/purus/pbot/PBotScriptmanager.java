package haven.purus.pbot;

import haven.UI;

import java.io.File;
import java.util.HashMap;
import java.util.UUID;

public class PBotScriptmanager {

    public static HashMap<String, PBotScript> scripts = new HashMap<String, PBotScript>();
    public UI ui;

    public static void startScript(UI ui, File scriptFile) {
        String id = UUID.randomUUID().toString();
        PBotScript script = new PBotScript(ui, scriptFile, id);
        scripts.put(id, script);
        script.start();
    }

    public static PBotScript getScript(String id) {
        return scripts.get(id);
    }
}
