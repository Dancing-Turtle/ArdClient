package haven.res.lib.tree;

import haven.*;

import java.util.ArrayList;
import java.util.Collection;

public class Factory implements Sprite.Factory {
	public LeafSpec leaves = null;

	public Tree create(Sprite.Owner owner, Resource res, Message sdt) {
		int s = -1, fl = 0;
		if(!sdt.eom()) {
			int m = sdt.uint8();
			if((m & 0xf0) != 0)
				s = ((m & 0xf0) >> 4) - 1;
			fl = (m & 0x0f) << 1;
		}
		float scale = sdt.eom() ? 1 : (sdt.uint8() / 100.0f);
		Tree ret = new Tree(owner, res, scale, s, fl);
		if(s == 2)
			ret.leaves = this.leaves;
		return(ret);
	}

	public void dropleaves(int matid, Indir<Resource> imats) {
		Resource mats = Loading.waitfor(imats);
		LeafSpec leaves = new LeafSpec();
		for(FastMesh.MeshRes mr : Resource.classres(this.getClass()).layers(FastMesh.MeshRes.class)) {
			if(mr.mat.id == matid)
				leaves.mesh = mr.m;
		}
		if(leaves.mesh == null)
			throw(new RuntimeException("No leaf-dropping mesh"));
		Collection<Material> m = new ArrayList<>();
		for(Material.Res mr : mats.layers(Material.Res.class))
			m.add(mr.get());
		if(m.isEmpty())
			throw(new RuntimeException("No leaf materials"));
		leaves.mat = m.toArray(new Material[0]);
		this.leaves = leaves;
	}
}