package haven.res.lib.globfx;

import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.WeakHashMap;

import haven.Coord2d;
import haven.Coord3f;
import haven.Drawable;
import haven.GLState;
import haven.Glob;
import haven.Gob;
import haven.RenderList;
import haven.Resource;
import haven.Sprite.Owner;

public class GlobEffector extends Drawable {
    static Map<Glob, Reference<GlobEffector>> cur = new WeakHashMap<>();
    public final Glob glob;
    Collection<Gob> holder = null;
    Map<Effect, Effect> effects = new HashMap<>();
    Map<Datum, Datum> data = new HashMap<>();

    private GlobEffector(Gob var1) {
        super(var1);
        this.glob = var1.glob;
    }

    public void setup(RenderList var1) {
        synchronized(this.effects) {
            Iterator var3 = this.effects.values().iterator();

            while(var3.hasNext()) {
                Effect var4 = (Effect)var3.next();
                var1.add(var4, (GLState)null);
            }

        }
    }

    public void ctick(int var1) {
        synchronized(this.effects) {
            Iterator var3 = this.effects.values().iterator();

            while(var3.hasNext()) {
                Effect var4 = (Effect)var3.next();
                if(var4.tick((float)var1)) {
                    var3.remove();
                }
            }

            var3 = this.data.values().iterator();

            while(true) {
                if(!var3.hasNext()) {
                    break;
                }

                Datum var12 = (Datum)var3.next();
                if(var12.tick((float)var1)) {
                    var3.remove();
                }
            }
        }

        synchronized(cur) {
            synchronized(this.effects) {
                if(this.effects.size() == 0 && this.data.size() == 0) {
                    this.glob.oc.lrem(this.holder);
                    cur.remove(this.glob);
                }
            }
        }
    }

    public Resource getres() {
        return null;
    }

    private <T> T create(Class<T> var1) {
        Resource var2 = Resource.classres(var1);

        try {
            try {
                Constructor<T> var3 = var1.getConstructor(Owner.class, Resource.class);
                return var3.newInstance(this.gob, var2);
            } catch (NoSuchMethodException var4) {
                throw new RuntimeException("No valid constructor found for global effect " + var1);
            }
        } catch (InstantiationException var5) {
            throw new RuntimeException(var5);
        } catch (IllegalAccessException var6) {
            throw new RuntimeException(var6);
        } catch (InvocationTargetException var7) {
            if(var7.getCause() instanceof RuntimeException) {
                throw (RuntimeException)var7.getCause();
            } else {
                throw new RuntimeException(var7);
            }
        }
    }

    @SuppressWarnings("unchecked")
    public <T extends Effect> T get(T var1) {
        synchronized(this.effects) {
            Effect var3 = this.effects.get(var1);
            if(var3 == null) {
                var3 = var1;
                this.effects.put(var1, var1);
            }

            return (T)var3;
        }
    }

    @SuppressWarnings("unchecked")
    public <T extends Datum> T getdata(T var1) {
        synchronized(this.data) {
            Datum var3 = this.data.get(var1);
            if(var3 == null) {
                var3 = var1;
                this.data.put(var1, var1);
            }

            return (T)var3;
        }
    }

    private static GlobEffector get(Glob var0) {
        synchronized(cur) {
            Reference<GlobEffector> var2 = cur.get(var0);
            if(var2 == null) {
                GlobEffectorObj var3 = new GlobEffectorObj(var0, Coord2d.z);
                GlobEffector var4 = new GlobEffector(var3);
                var3.setattr(var4);
                var0.oc.ladd(var4.holder = Collections.singleton(var3));
                cur.put(var0, var2 = new WeakReference<>(var4));
            }

            return (GlobEffector)((Reference)var2).get();
        }
    }

    public static <T extends Effect> T get(Glob var0, T var1) {
        return get(var0).get(var1);
    }

    public static <T extends Datum> T getdata(Glob var0, T var1) {
        return get(var0).getdata(var1);
    }

    final static class GlobEffectorObj extends Gob {
        GlobEffectorObj(Glob var1, Coord2d var2) {
            super(var1, var2);
        }

        public Coord3f getc() {
            return Coord3f.o;
        }
    }

}
