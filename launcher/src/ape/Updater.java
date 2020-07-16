package ape;

import org.apache.commons.io.FileUtils;
import org.apache.commons.codec.digest.DigestUtils;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;

import java.security.*;

public class Updater {
	public static void main(String args[]) {
		if (args.length == 0) {
			Logging.log("args: update hash");
		} else {
			if (args[0].equals("update")) {
				Updater upd = new Updater(args[1]);
				try {
					upd.update();
				} catch (Exception e) {
					Logging.error(e, false);
				}
				upd.run(args);
			} else if (args[0].equals("hash")) {
				Hasher hash = new Hasher(args[1], args[2]);
				hash.run();
			}
		}
	}

	String baseurl;
	MessageDigest md;

	public Updater(String url_updfile) {
		baseurl = url_updfile;
		new File("tmp/").mkdirs();
	}

	public String getHash(String f) throws Exception {
		Logging.log("Generating hash for " + f);
		if(new File(f).exists())
			return DigestUtils.sha256Hex(FileUtils.readFileToByteArray(new File(f)));
		else
			return "";
	}

	public String[] checkVer() throws Exception {
		Logging.log("Comparing version files");
		String[] files = FileIO.loadIniFile("tmp/ver");
		String[] old = FileIO.loadIniFile("ver");
		ArrayList<String> download = new ArrayList<String>(files.length);
		String[] kv;

		//check if already updated
		//if(files[0].equals(old[0]))
		//    return null;

		//find different files
		for(int i = 1;i < files.length;++i) {
			kv = files[i].split("=");
			Logging.log("Comparing file " + kv[0]);
			if(!kv[1].equals(getHash(kv[0])))
				download.add(baseurl+kv[0]);
		}

		return download.toArray(new String[0]);
	}

	public void update() throws Exception {
		md = MessageDigest.getInstance("SHA-256");
		URL upd_url = new URL(baseurl+"ver");
		//download update file
		Logging.log("Downloading new version file");
		FileUtils.copyURLToFile(upd_url, new File("tmp/ver"));
		String[] dls = checkVer();

		//download files missing/mismatch on hashes
		if(dls != null) {
			for(String dl : dls) {
				String fn = dl.substring(baseurl.length());
				Logging.log("Downloading " + dl + " -> " + fn);
				try {
					FileUtils.copyURLToFile(new URL(dl.replaceAll("\\\\", "/")), new File(fn));
				} catch(Exception e) { Logging.error(e, false); }
			}
		}

		Logging.log("Moving over new version file");
		FileUtils.forceDelete(new File("ver"));
		FileUtils.moveFile(new File("tmp/ver"), new File("ver"));
	}

	public void run(String cargs[]) {
		Logging.log("Starting client");
		ArrayList<String> args = new ArrayList<String>();
		args.add("java");
		for(int i = 2;i < cargs.length;++i)
			args.add(cargs[i]);
		ProcessBuilder pb = new ProcessBuilder(args);
		pb.directory(new File("").getAbsoluteFile());
		try {
			pb.redirectOutput(ProcessBuilder.Redirect.INHERIT);
			pb.redirectError(ProcessBuilder.Redirect.INHERIT);
			pb.start().waitFor();
		} catch(Exception e) {
			Logging.error(e, false);
		}
	}
}