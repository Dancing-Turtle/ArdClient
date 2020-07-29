package haven.purus.pbot;

import haven.UI;

import java.io.File;
import java.util.HashMap;
import java.util.UUID;

public class PBotScriptmanagerOld {

    static HashMap<String, PBotScriptOld> scripts = new HashMap<String, PBotScriptOld>();

    public static void startScript(UI ui, File scriptFile) {
        String id = UUID.randomUUID().toString();
        PBotScriptOld script = new PBotScriptOld(ui, scriptFile, id);
        scripts.put(id, script);
        script.start();
    }

    public static PBotScriptOld getScript(String id) {
        return scripts.get(id);
    }
}
