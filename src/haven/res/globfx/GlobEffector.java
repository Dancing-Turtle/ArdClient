package haven.res.globfx;

import haven.*;
import haven.res.lib.globfx.Datum;
import haven.res.lib.globfx.Effect;

import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.*;

public class GlobEffector extends Drawable {
    /* Keep weak references to the glob-effectors themselves, or
     * GlobEffector.glob (and GlobEffector.gob.glob) will keep the
     * globs alive through the strong value references forever. */
    static Map<Glob, Reference<haven.res.lib.globfx.GlobEffector>> cur = new WeakHashMap<Glob, Reference<haven.res.lib.globfx.GlobEffector>>();
    public final Glob glob;
    Collection<Gob> holder = null;
    Map<Effect, Effect> effects = new HashMap<Effect, Effect>();
    Map<Datum, Datum> data = new HashMap<Datum, Datum>();
    
    private GlobEffector(Gob gob) {
    	super(gob);
    	this.glob = gob.glob;
    }
    
    public void setup(RenderList rl) {
    	synchronized(effects) {
    		for(Effect spr : effects.values())
    			rl.add(spr, null);
    	}
    }

    public Object staticp() {
    	return(null);
    }
    
    public void ctick(int idt) {
    	float dt = idt * 0.001f;
    	synchronized(effects) {
    		for(Iterator<Effect> i = effects.values().iterator(); i.hasNext();) {
    			Effect spr = i.next();
    			if(spr.tick(dt))
    				i.remove();
    		}
    		for(Iterator<Datum> i = data.values().iterator(); i.hasNext();) {
    			Datum d = i.next();
    			if(d.tick(dt))
    				i.remove();
    		}
    	}
    	synchronized(cur) {
    		synchronized(effects) {
    			if((effects.size() == 0) && (data.size() == 0)) {
    				glob.oc.lrem(holder);
    				cur.remove(glob);
    			}
    		}
    	}
    }
    
    public Resource getres() {
    	return(null);
    }
    
    private <T> T create(Class<T> fx) {
    	Resource res = Resource.classres(fx);
    	try {
    		try {
    			Constructor<T> cons = fx.getConstructor(Sprite.Owner.class, Resource.class);
    			return(cons.newInstance(gob, res));
    		} catch(NoSuchMethodException e) {}
    		throw(new RuntimeException("No valid constructor found for global effect " + fx));
    	} catch(InstantiationException e) {
    		throw(new RuntimeException(e));
    	} catch(IllegalAccessException e) {
    		throw(new RuntimeException(e));
    	} catch(InvocationTargetException e) {
    		if(e.getCause() instanceof RuntimeException)
    			throw((RuntimeException)e.getCause());
    		throw(new RuntimeException(e));
    	}
    }

    @SuppressWarnings("unchecked")
    public <T extends Effect> T get(T fx) {
    	synchronized(effects) {
    		T ret = (T)effects.get(fx);
    		if(ret == null)
    			effects.put(ret = fx, fx);
    		return(ret);
    	}
    }
    
    @SuppressWarnings("unchecked")
    public <T extends Datum> T getdata(T fx) {
    	synchronized(data) {
    		T ret = (T)data.get(fx);
    		if(ret == null)
    			data.put(ret = fx, fx);
    		return(ret);
    	}
    }
    
    private static haven.res.lib.globfx.GlobEffector get(Glob glob) {
    	synchronized(cur) {
    		Reference<haven.res.lib.globfx.GlobEffector> ref = cur.get(glob);
    		if(ref == null) {
    			Gob hgob = new Gob(glob, Coord2d.z) {
    				public Coord3f getc() {
    					return(Coord3f.o);
    				}
    			};
    			haven.res.lib.globfx.GlobEffector ne = new haven.res.lib.globfx.GlobEffector(hgob);
    			hgob.setattr(ne);
    			glob.oc.ladd(ne.holder = Collections.singleton(hgob));
    			cur.put(glob, ref = new WeakReference<haven.res.lib.globfx.GlobEffector>(ne));
    		}
    		return(ref.get());
    	}
    }

    public static <T extends Effect> T get(Glob glob, T fx) {
    	return(get(glob).get(fx));
    }

    public static <T extends Datum> T getdata(Glob glob, T fx) {
    	return(get(glob).getdata(fx));
    }
  }