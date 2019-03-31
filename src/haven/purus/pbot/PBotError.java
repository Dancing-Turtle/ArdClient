package haven.purus.pbot;

import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;

public class PBotError {

	// Writes error stacktrace to disk and notifies the user
	public static void handleException(Exception e) {
		try {
			File output = new File("PBotError_" + System.currentTimeMillis() + ".txt");
			output.createNewFile();
			PBotUtils.sysMsg("PBot error occurred!! Writing it into a file called: " + output.getPath(), Color.RED);
			PrintWriter pw = new PrintWriter(Files.newBufferedWriter(output.toPath(), StandardOpenOption.WRITE));
			e.printStackTrace(pw);
			pw.flush();
			e.printStackTrace();
		} catch(IOException e1) {
			e1.printStackTrace();
		}
	}
}
