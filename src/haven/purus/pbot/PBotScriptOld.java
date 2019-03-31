package haven.purus.pbot;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import java.awt.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;

public class PBotScriptOld extends Thread {
	private File scriptFile;
	private String name, id;
	private ScriptEngine engine;

	public PBotScriptOld(File scriptFile, String id) {
		this.scriptFile = scriptFile;
		this.name = scriptFile.getName();
		this.id = id;
	}

	public void run() {
		PBotUtils.sysMsg("Starting script: " + name, Color.white);
		ScriptEngine engine = new ScriptEngineManager().getEngineByName("nashorn");
		this.engine = engine;
		try {
			engine.eval("var scriptID = \"" + id+"\";");
			engine.eval(new FileReader(scriptFile));
		} catch (FileNotFoundException | ScriptException e) {
			e.printStackTrace();
		}
	}

	public ScriptEngine getEngine() {
		return engine;
	}
}
