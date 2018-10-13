package haven;

import java.awt.image.BufferedImage;
import java.util.*;
import java.util.function.Function;

public class CharacterInfo {

    public final Constipation constipation = new Constipation();

    public static class Constipation {
	public final List<Data> els = new ArrayList<Data>();

	public void update(Indir<Resource> t, double a) {
	    prev:
	    {
		for (Iterator<Data> i = els.iterator(); i.hasNext(); ) {
		    Data el = i.next();
		    if(el.res != t)
			continue;
		    if(a == 1.0)
			i.remove();
		    else
			el.update(a);
		    break prev;
		}
		Data e = new Data(t, a);
		els.add(e);
	    }
	}

	public Data get(int i) {
		return els.size() > i ? els.get(i) : null;
	}

	public static class Data {
	    private final Map<Class, BufferedImage> renders = new HashMap<>();
	    public final Indir<Resource> res;
	    public double value;

	    public Data(Indir<Resource> res, double value) {
		this.res = res;
		this.value = value;
	    }

	    public void update(double a) {
		value = a;
		renders.clear();
	    }

	    private BufferedImage render(Class type, Function<Data, BufferedImage> renderer) {
		if(!renders.containsKey(type)) {
		    renders.put(type, renderer.apply(this));
		}
		return renders.get(type);
	    }
	}

	private final Map<Class, Function<Data, BufferedImage>> renderers = new HashMap<>();

	public void addRenderer(Class type, Function<Data, BufferedImage> renderer) {
	    renderers.put(type, renderer);
	}

	public boolean hasRenderer(Class type) {
	    return renderers.containsKey(type);
	}

	public BufferedImage render(Class type, Data data) {
	    try {
		return renderers.containsKey(type) ? data.render(type, renderers.get(type)) : null;
	    } catch (Loading ignored) {}
	    return null;
	}
    }
}
