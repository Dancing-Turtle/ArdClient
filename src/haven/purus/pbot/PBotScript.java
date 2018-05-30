package haven.purus.pbot;

import java.awt.Color;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

public class PBotScript extends Thread {
	
	private File scriptFile;
	private String name, id;
	private ScriptEngine engine;

	public PBotScript(File scriptFile, String id) {
		this.scriptFile = scriptFile;
		this.name = scriptFile.getName();
		this.id = id;
	}
	
	public void run() {
		PBotAPI.sysMsg("Starting script: " + name, Color.ORANGE);
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
