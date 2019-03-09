package haven;

import haven.TextureAtlas;

import java.awt.image.BufferedImage;
import static haven.DefSettings.HUDTHEME;

/**
 * Shortcuts for getting theme'd resource files
 */
public class Theme {
    private static final TextureAtlas atlas;
    private static final String fmt = "custom/hud/%s/%s";
    static {
        atlas = new TextureAtlas("Theme", new Coord(512, 512));
    }

    public static TextureAtlas.Img timg(final String res) {
        final String fres = String.format(fmt, HUDTHEME.get(), res);
        if(atlas.contains(fres)) {
            return atlas.get(fres);
	} else {
            atlas.add(fres, Resource.remote().loadwait(fres));
            return atlas.get(fres);
        }
    }

    public static TextureAtlas.Img timg(final String res, final int id) {
	final String fres = String.format(fmt, HUDTHEME.get(), res);
	final String rid = fres+"-"+id;
	if(atlas.contains(rid)) {
	    return atlas.get(rid);
	} else {
	    atlas.add(rid, Resource.remote().loadwait(fres), id);
	    return atlas.get(rid);
	}
    }

    public static Tex tex(final String res) {
	return Resource.loadtex(String.format(fmt, HUDTHEME.get(), res));
    }

    public static Tex tex(final String res, final int id) {
	return Resource.loadtex(String.format(fmt, HUDTHEME.get(), res), id);
    }

    public static BufferedImage img(final String res) {
	return Resource.loadimg(String.format(fmt, HUDTHEME.get(), res));
    }

    public static BufferedImage img(final String res, final int id) {
	return Resource.loadimg(String.format(fmt, HUDTHEME.get(), res), id);
    }

    public static Resource res(final String res) {
        return Resource.local().loadwait(String.format(fmt, HUDTHEME.get(), res));
    }

    public static String fullres(final String res) {
        return String.format(fmt, HUDTHEME.get(), res);
    }
}
