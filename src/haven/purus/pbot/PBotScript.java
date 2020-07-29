package haven.purus.pbot;

import haven.UI;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;

import java.awt.Color;
import java.io.File;

public class PBotScript extends Thread {

    public Context context;
    private File scriptFile;
    private String name, id;
    public UI ui;

    public PBotScript(UI ui, File scriptFile, String id) {
        this.scriptFile = scriptFile;
        this.name = scriptFile.getName();
        this.id = id;
        this.ui = ui;
    }

    public void run() {
        PBotUtils.sysMsg(ui, "Starting script: " + name, Color.ORANGE);
        context = Context.newBuilder("js").allowAllAccess(true).build();
        try {
            context.eval("js", "const ScriptID = '" + id + "';");
            context.eval(Source.newBuilder("js", scriptFile).build());

        } catch (Exception e) {
            PBotError.handleException(ui, e);
        }
    }
}
