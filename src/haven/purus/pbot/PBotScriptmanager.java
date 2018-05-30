package haven.purus.pbot;

import java.io.File;
import java.util.HashMap;
import java.util.UUID;

import javax.script.ScriptEngine;

public class PBotScriptmanager {
	
	static HashMap<String, PBotScript> scripts = new HashMap<String, PBotScript>();
	
	public static void startScript(File scriptFile) {
		String id = UUID.randomUUID().toString();
		PBotScript script = new PBotScript(scriptFile, id);
		scripts.put(id, script);
		script.start();
	}
	
	public static PBotScript getScript(String id) {
		return scripts.get(id);
	}
}
