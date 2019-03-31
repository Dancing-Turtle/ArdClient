package haven.sloth.util;

import haven.Tex;
import haven.TexI;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.HashMap;
import java.util.Map;

public class Images {
    private static Map<String, Tex> cache = new HashMap<>();

    public static Tex load(String itm) {
	String path = "data/" + itm + ".png";
	if (cache.get(itm) != null)
	    return cache.get(itm);
	try {
	    Tex i = new TexI(ImageIO.read(new File(path)));
	    cache.put(itm, i);
	    return i;
	} catch (Exception e) {
	    e.printStackTrace();
	    System.out.println(path);
	}
	return null;
    }

    public static BufferedImage loadimg(String itm) {
	String path = "data/" + itm + ".png";
	try {
	    return ImageIO.read(new File(path));
	} catch (Exception e) {
	    e.printStackTrace();
	    System.out.println(path);
	}
	return null;
    }
}
