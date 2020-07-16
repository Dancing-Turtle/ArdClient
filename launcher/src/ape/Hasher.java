package ape;

import org.apache.commons.io.FileUtils;
import org.apache.commons.codec.digest.DigestUtils;

import java.io.File;
import java.util.*;

import java.security.*;

public class Hasher {
	private class Filter implements org.apache.commons.io.filefilter.IOFileFilter {
		public boolean accept(File f) {
			if(!f.getName().equals("ver") &&
					!f.getName().equals("hafen-updater.jar"))
				return true;
			else
				return false;
		}

		public boolean accept(File dir, String name) {
			return true;
		}
	}

	final File dir;
	final String vers;
	MessageDigest md;
	public Hasher(String basedir, String ver) {
		dir = new File(basedir);
		vers = ver;
		try {
			md = MessageDigest.getInstance("SHA-256");
		} catch(Exception e){e.printStackTrace(); }
	}

	public String getHash(File f) throws Exception {
		System.out.println("Get hash for " + f.getAbsolutePath());
		return DigestUtils.sha256Hex(FileUtils.readFileToByteArray(f));
	}

	public void run() {
		Filter filter = new Filter();
		Iterator<File>
				itr = FileUtils.iterateFiles(dir, filter, filter);
		File f ;
		StringBuilder sb = new StringBuilder();
		sb.append(vers);
		sb.append('\n');

		try {
			while(itr.hasNext()) {
				f = itr.next();
				String fn = f.getAbsolutePath().substring(dir.getAbsolutePath().length()+1);
				sb.append(fn.replaceAll("\\\\", "/"));
				sb.append('=');
				sb.append(getHash(f));
				sb.append('\n');
			}

			FileUtils.writeStringToFile(new File("ver"), sb.toString(), "UTF-8");
		} catch(Exception e) { e.printStackTrace(); }
	}
}