package haven.res.lib.tree;

import haven.*;
import haven.res.lib.leaves.FallingLeaves;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Random;

public class Tree extends Sprite {
	private final Location scale;
	private final Location rot;
	public final float fscale;
	public final Rendered[][] parts;
	public int stage, sel;
	public LeafSpec leaves = null;
	private Location.Chain loc = null;

	public static Location mkscale(float x, float y, float z) {
		return(new Location(new Matrix4f(x, 0, 0, 0,
			0, y, 0, 0,
			0, 0, z, 0,
			0, 0, 0, 1)));
	}

	public static Location mkscale(float s) {
		return(mkscale(s, s, s));
	}

	public static Collection<Pair<Integer, Rendered>> lsparts(Resource res, int matsel) {
		Collection<Pair<Integer, Rendered>> rl = new ArrayList<>(16);
		for(FastMesh.MeshRes mr : res.layers(FastMesh.MeshRes.class)) {
			int id = (mr.id < 0) ? 0 : mr.id;
			int dmat = (id & 0xf0) >> 4;
			if((dmat == 0) || (matsel < 0)) {
				if(mr.mat != null)
					rl.add(new Pair<>(id, mr.mat.get().apply(mr.m)));
			} else {
				Material.Res mat = res.layer(Material.Res.class, (dmat * 4) + matsel);
				if(mat != null)
					rl.add(new Pair<>(id, mat.get().apply(mr.m)));
			}
		}
		for(RenderLink.Res lr : res.layers(RenderLink.Res.class)) {
			rl.add(new Pair<>(lr.id, lr.l.make()));
		}
		return(rl);
	}

	@SuppressWarnings("unchecked")
	public static Rendered[][] mkparts(Resource res, int matsel, int fl) {
		Collection<Rendered> rl[] = new Collection[3];
		for(int i = 0; i < rl.length; rl[i++] = new ArrayList<>());
			int pmask = (1 << rl.length) - 1;
		for(Pair<Integer, Rendered> part : lsparts(res, matsel)) {
			if(((1 << (part.a & 0xf)) & fl) != 0)
				continue;
			int dmesh = (part.a & 0xf00) >> 8;
			if((dmesh != 0) && (matsel >= 0) && ((dmesh & (1 << matsel)) == 0))
				continue;
			int sel = ((part.a & 0xf000) >> 12) & pmask;
			for(int i = 0; i < rl.length; i++)
				if((sel == 0) || ((sel & (1 << i)) != 0))
					rl[i].add(part.b);
			}
			Rendered[][] ret = new Rendered[rl.length][];
			for(int i = 0; i < ret.length; i++)
				ret[i] = rl[i].toArray(new Rendered[0]);
			return(ret);
		}

		public static Random randoom(Gob owner) {
			Random rnd;
			try {
				Coord tc = owner.rc.floor(MCache.tilesz);
				MCache.Grid grid = owner.glob.map.getgridt(tc);
				tc = tc.sub(grid.ul);
				rnd = new Random(grid.id);
				rnd.setSeed(rnd.nextLong() ^ tc.x);
				rnd.setSeed(rnd.nextLong() ^ tc.y);
			} catch (Loading e) {
				rnd = new Random(0);
			}
			return(rnd);
		}

		public static Location rndrot(Random rnd) {
			double aa = rnd.nextDouble() * Math.PI * 2;
			double ra = rnd.nextGaussian() * Math.PI / 64;
			Coord3f axis = new Coord3f((float)Math.sin(aa), (float)Math.cos(aa), 0);
			return(Location.rot(axis, (float)ra));
		}

		public static Location rndrot(Owner owner) {
			if(owner instanceof Gob)
				return(rndrot(randoom((Gob)owner)));
			return(null);
		}

		public Tree(Owner owner, Resource res, float scale, int s, int fl) {
			super(owner, res);
			rot = rndrot(owner);
			this.fscale = scale;
			this.scale = (scale == 1.0f) ? null : mkscale(scale);
			parts = mkparts(res, s, fl);
			sel = s;
		}

		private static final haven.res.lib.tree.Factory deffac = new haven.res.lib.tree.Factory();
		public static Tree mksprite(Owner owner, Resource res, Message sdt) {
			/* XXX: Remove me */
			return(deffac.create(owner, res, sdt));
		}

		public boolean setup(RenderList r) {
			if(rot != null)
				r.prepc(rot);
			if(scale != null) {
				r.prepc(scale);
				r.prepc(States.normalize);
			}
			for(Rendered p : parts[stage])
				r.add(p, null);
			this.loc = r.cstate().get(PView.loc);
			return(false);
		}

		private Random lrand;
		private double lrate;
		public boolean tick(int idt) {
			leaves: if(leaves != null) {
				double dt = idt * 0.001;
				if(lrand == null) {
					lrand = new Random();
					if(lrand.nextInt(2) == 0) {
						leaves = null;
						break leaves;
					}
					Random rrand = lrand;
					if(owner instanceof Gob)
						rrand = randoom((Gob)owner);
					lrate = 0.05 + (Math.pow(rrand.nextDouble(), 0.75) * 0.95);
				}
				if(fscale < 0.75)
					return(false);
				try {
					if((loc != null) && (lrand.nextDouble() > Math.pow(lrate, dt))) {
						FallingLeaves fx = FallingLeaves.get(((Gob)owner).glob);
						Material mat = leaves.mat[lrand.nextInt(leaves.mat.length)];
						fx.addleaf(new StdLeaf(fx, fx.onevertex(loc, leaves.mesh), mat));
					}
				} catch(Loading e) {}
			}
			return(false);
		}
	}