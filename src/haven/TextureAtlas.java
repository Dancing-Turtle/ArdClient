package haven;

import com.google.common.flogger.FluentLogger;


import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

public class TextureAtlas {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public class Img implements Comparable<Img>{
	public final BufferedImage back;
	public final Coord sz;
	public Coord
		ul;
	public Coord3f
		tul, tbr;


	private Img(final BufferedImage buf) {
	    back = buf;
	    sz = Utils.imgsz(buf);
	}

	public void update(final Coord c) {
	    ul = c;
	}

	private void finalize(final Coord tsz) {
	    tul = new Coord3f(ul.x /(float)tsz.x, ul.y / (float)tsz.y, 0);
	    tbr = new Coord3f((ul.x+sz.x)/(float)tsz.x,
		    (ul.y+sz.y)/(float)tsz.y,
		    0);
	}

	public int compareTo(Img o) {
	    return Integer.compare(Math.max(sz.x, sz.y), Math.max(o.sz.x, o.sz.y));
	}

	public Tex tex() {
	    return TextureAtlas.this.tex();
	}
    }

    private class Node {
	public final Coord c;
	public Coord sz;
	public boolean used;
	private Node
		right, down;

	private Node(final Coord c, final Coord sz) {
	    this.c = c;
	    this.sz = sz;
	    used = false;
	    right = down = null;
	}
    }

    private HashMap<String, Img> itms = new HashMap<>();
    private BufferedImage back;
    private Tex tex;
    private final int mar = 1;
    private Coord sz;
    private boolean dirty, vdirty;
    private final String name;
    private Node root;

    public TextureAtlas(final String name, final Coord defsz) {
	this.name = name;
	sz = defsz;
	root = new Node(Coord.z, sz);
	back = TexI.mkbuf(sz);
	tex = null;
	dirty = false;

	/* TODO: redo with dynamic.sqlite tables
	String kv[];
	for(String s : FileIO.loadIniFile("data/"+name+"-preload.ini")) {
	    kv = s.split("=");
	    respreload.add(s);
	    add(kv[0],
		    Resource.remote().loadwait(kv[0],-1).layer(Resource.imgc).img);
	}*/
    }

    public TextureAtlas(final String name) {
        this(name, new Coord(1024, 1024));
    }

    private void savepreload() {
	//TODO: redo with dynamic.sqlite tables
	//String save = "";
	//for(String s : respreload)
	//    save = save + s + "\n";
	//FileIO.saveFile("data/"+name+"-preload.ini", save.getBytes());
    }

    private Node find(final Node n, final Coord sz) {
	if(n.right == null) {
	    if(n.sz.x >= sz.x &&
		    n.sz.y >= sz.y) {
		return n;
	    } else {
		return null;
	    }
	} else {
	    Node r = find(n.right, sz);
	    if(r != null)
		return r;
	    return find(n.down, sz);
	}
    }

    private void split(final Node n, final Coord sz) {
	n.right = new Node(n.c.add(sz.x,0),
		new Coord(n.sz.x-sz.x, sz.y));
	n.down = new Node(n.c.add(0,sz.y),
		new Coord(n.sz.x, n.sz.y - sz.y));
    }

    private Node grow(final Node root, final Coord sz) {
	Node ret;
	if(root.sz.y >= (root.sz.x + sz.x)) {
	    //grow right
	    ret = new Node(Coord.z, root.sz.add(sz.x, 0));
	    ret.sz.x = Tex.nextp2(ret.sz.x);
	    ret.down = root;
	    ret.right = new Node(new Coord(root.sz.x,0), new Coord(ret.sz.x-root.sz.x, ret.sz.y));
	} else {
	    //grow down
	    ret = new Node(Coord.z, root.sz.add(0, sz.y));
	    ret.sz.y = Tex.nextp2(ret.sz.y);
	    ret.down = new Node(new Coord(0, root.sz.y), new Coord(ret.sz.x, ret.sz.y-root.sz.y));
	    ret.right = root;
	}
	return ret;
    }

    private void repack() {
	List<Img> itms = new ArrayList<>(this.itms.values());
	Collections.sort(itms);
	sz = new Coord(0,0);
	for(Img i : itms) {
	    sz.x = Math.max(sz.x, i.sz.x+mar);
	    sz.y = Math.max(sz.y, i.sz.y+mar);
	}
	Node root = new Node(Coord.z, sz);
	Node f;
	Coord isz;
	int i = itms.size();

	//find locs
	while( --i >= 0 ) {
	    Img it = itms.get(i);
	    isz = it.sz.add(mar, mar);
	    f = find(root, isz);
	    if(f != null) {
		it.update(f.c);
		split(f, isz);
	    } else {
		f = grow(root, isz);
		root = f;
		f = find(root, isz);
		if(f != null) {
		    it.update(f.c);
		    split(f, isz);
		}
	    }
	}

	//render
	sz = new Coord(Tex.nextp2(root.sz.x), Tex.nextp2(root.sz.y));
	this.back = TexI.mkbuf(sz);
	final java.awt.Graphics g = back.getGraphics();
	for(Img it : itms) {
	    it.finalize(sz);
	    g.drawImage(it.back, it.ul.x, it.ul.y, null);
	}
	this.root = root;
    }

    public void add(final String name, final BufferedImage img) {
	Img it = new Img(img);
	if(itms.get(name) == null) {
	    itms.put(name, it);

	    Node f = find(root, it.sz.add(mar,mar));
	    if(f != null) {
		final java.awt.Graphics g = back.getGraphics();
		it.update(f.c);
		it.finalize(root.sz);
		g.drawImage(it.back, it.ul.x, it.ul.y, null);
		split(f, it.sz.add(mar, mar));
	    } else {
		vdirty = true;
	    }
	    dirty = true;
	}
    }

    public void add(final String name, final Resource res) {
	if(itms.get(name) == null) {
	    add(name, res.layer(Resource.imgc).img);
	    savepreload();
	}
    }

    public void add(final String name, final Resource res, final int id) {
	if(itms.get(name) == null) {
	    add(name, res.layer(Resource.imgc, id).img);
	    savepreload();
	}
    }

    public boolean contains(final String name) {
        return itms.containsKey(name);
    }

    public Img get(final String name) {
	if(itms.get(name) != null) {
	    return itms.get(name);
	}

	logger.atSevere().log("Failed to find %s in texture atlas", name);
	return null;
    }

    public Tex tex() {
	if(dirty || vdirty || tex == null) {
	    if(tex != null)
		tex.dispose();
	    if(vdirty) {
		repack();
	    }
	    if(Config.dumpcode) {
		Debug.dumpimage(back, "data/dump/atlas-" + this.name + ".png");
	    }
	    tex = new TexI(back);
	    dirty = vdirty =false;
	}
	return tex;
    }
}
