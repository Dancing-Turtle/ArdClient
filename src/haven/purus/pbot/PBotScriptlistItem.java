package haven.purus.pbot;

import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import haven.Tex;
import haven.TexI;
import haven.Text;

public class PBotScriptlistItem {

	private String name;
	private File scriptFile;
	private Tex iconTex, nameTex;
	
	public PBotScriptlistItem(String name, File scriptFile) {
		this.name = name;
		this.scriptFile = scriptFile;
		
		File icon = new File("scripts/"+scriptFile.getName().substring(0, scriptFile.getName().lastIndexOf("."))+".png");
		if(icon.exists()) {
			try {
				this.iconTex = new TexI(ImageIO.read(icon));
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
		this.nameTex = Text.render(name.substring(0, name.length()-5)).tex();
	}
	
	public void runScript() {
		PBotScriptmanager.startScript(scriptFile);
	}
	
	public Tex getIconTex() {
		return this.iconTex;
	}
	
	public Tex getNameTex() {
		return this.nameTex;
	}
	
	public String getName() {
		return this.name;
	}

}
