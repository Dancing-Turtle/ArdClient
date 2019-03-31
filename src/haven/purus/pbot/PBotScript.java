package haven.purus.pbot;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;

import java.awt.*;
import java.io.File;

public class PBotScript extends Thread {

	public Context context;
	private File scriptFile;
	private String name, id;

	public PBotScript(File scriptFile, String id) {
		this.scriptFile = scriptFile;
		this.name = scriptFile.getName();
		this.id = id;
	}
	
	public void run() {
		PBotUtils.sysMsg("Starting script: " + name, Color.ORANGE);
		context = Context.newBuilder("js").allowAllAccess(true).build();
		try {
			context.eval("js", "const ScriptID = '" + id + "';");
			context.eval(Source.newBuilder("js", scriptFile).build());

		} catch(Exception e) {
			PBotError.handleException(e);
		}
	}
}
