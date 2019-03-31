package haven.sloth.gui;

import haven.*;
import haven.Button;
import haven.Window;

import java.awt.*;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

public class MapMarkerWnd extends Window {
    private final static List<String> types = Arrays.asList("Placed", "Natural", "Custom", "Linked");
    private final static Predicate<MapFile.Marker> pmarkers = (m -> m instanceof MapFile.PMarker);
    private final static Predicate<MapFile.Marker> smarkers = (m -> m instanceof MapFile.SMarker);
    private final static Predicate<MapFile.Marker> slmarkers = (m -> m instanceof MapFile.SlothMarker && !(m instanceof MapFile.LinkedMarker));
    private final static Predicate<MapFile.Marker> lmarkers = (m -> m instanceof MapFile.LinkedMarker);
    private final static Comparator<MapFile.Marker> namecmp = Comparator.comparing(MapFile.Marker::name);
    private Predicate<MapFile.Marker> mflt;
    private List<MapFile.Marker> markers = Collections.emptyList();
    private int markerseq = -1;
    private Comparator<MapFile.Marker> mcmp = namecmp;
    private final MapWnd map;
    public final MarkerList list;
    private TextEntry namesel;
    private BuddyWnd.GroupSelector colsel;
    private Button mremove;
    private Dropbox<String> typesel;
    private Dropbox<MapFile.Marker> linker;

    public MapMarkerWnd(final MapWnd map) {
        super(Coord.z, "Markers", "Markers");
    	this.map = map;
    	mflt = pmarkers;
	list = add(new MarkerList(200, 20));
	resize(list.sz.add(0, 120));
	typesel = add(new Dropbox<String>(200, types.size(), 20) {
	    { sel = types.get(0); }
	    @Override
	    protected int listitems() {
		return types.size();
	    }

	    @Override
	    protected String listitem(int i) {
		return types.get(i);
	    }

	    @Override
	    protected void drawitem(GOut g, String item, int i) {
		FastText.aprint(g, new Coord(5, sz.y / 2), 0.0, 0.5, item);
	    }

	    @Override
	    public void change(String item) {
		super.change(item);
	    	switch (item) {
		    case "Placed":
			mflt = pmarkers;
			markerseq = -1;
		        break;
		    case "Natural":
			mflt = smarkers;
			markerseq = -1;
			break;
		    case "Custom":
			mflt = slmarkers;
			markerseq = -1;
			break;
		    case "Linked":
		        mflt = lmarkers;
		        markerseq = -1;
		        break;
		}
		list.display(0);
	    }
	}, list.c.add(0, list.sz.y + 10));
	pack();
    }

    @Override
    public void close() {
	hide();
    }

    @Override
    public void tick(double dt) {
	super.tick(dt);
	if(visible && (markerseq != map.view.file.markerseq)) {
	    if(map.view.file.lock.readLock().tryLock()) {
		try {
		    this.markers = map.view.file.markers.stream().filter(mflt).sorted(mcmp).collect(java.util.stream.Collectors.toList());
		    list.display();
		} finally {
		    map.view.file.lock.readLock().unlock();
		}
	    }
	}
    }

    public static final Color every = new Color(255, 255, 255, 16), other = new Color(255, 255, 255, 32), found = new Color(255, 255, 0, 32);
    public class MarkerList extends Searchbox<MapFile.Marker> {
	private final Text.Foundry fnd = CharWnd.attrf;

	public MapFile.Marker listitem(int idx) {return(markers.get(idx));}
	public int listitems() {return(markers.size());}
	public boolean searchmatch(int idx, String txt) {
	    return markers.get(idx).nm.toLowerCase().contains(txt.toLowerCase());
	}

	private MarkerList(int w, int n) {
	    super(w, n, 20);
	}

	private Function<String, Text> names = new CachedFunction<>(500, fnd::render);
	protected void drawbg(GOut g) {}
	public void drawitem(GOut g, MapFile.Marker mark, int idx) {
	    if(soughtitem(idx)) {
		g.chcolor(found);
		g.frect(Coord.z, g.sz);
	    }
	    g.chcolor(((idx % 2) == 0)?every:other);
	    g.frect(Coord.z, g.sz);
	    if(mark instanceof MapFile.PMarker)
		g.chcolor(((MapFile.PMarker)mark).color);
	    else if(mark instanceof MapFile.SlothMarker)
	        g.chcolor(((MapFile.SlothMarker) mark).color);
	    else
		g.chcolor();
	    if(!(mark instanceof MapFile.LinkedMarker))
	    	g.aimage(names.apply(mark.nm).tex(), new Coord(5, itemh / 2), 0, 0.5);
	    else
	        g.aimage(names.apply(String.format("[%d ⟶ %d] %s", ((MapFile.LinkedMarker) mark).id, ((MapFile.LinkedMarker) mark).lid, mark.nm)).tex(),
			new Coord(5, itemh/2), 0, 0.5);
	}

	public void change(MapFile.Marker mark) {
	    change2(mark);
	    if(mark != null)
		map.view.center(new MapFileWidget.SpecLocator(mark.seg, mark.tc));
	}

	public void change2(MapFile.Marker mark) {
	    this.sel = mark;

	    if(namesel != null) {
		ui.destroy(namesel);
		namesel = null;
		if(colsel != null) {
		    ui.destroy(colsel);
		    colsel = null;
		    if(mremove != null) {
			ui.destroy(mremove);
			mremove = null;
		    }
		    if(linker != null) {
		        ui.destroy(linker);
		        linker = null;
		    }
		}
		MapMarkerWnd.this.pack();
	    }

	    if(mark != null) {
	        if(mark instanceof MapFile.PMarker) {
		    typesel.sel = types.get(0);
		    mflt = pmarkers;
		    markerseq = -1;
		} else if(mark instanceof MapFile.SMarker){
		    typesel.sel = types.get(1);
		    mflt = smarkers;
		    markerseq = -1;
		} else if(mark instanceof MapFile.LinkedMarker) {
		    typesel.sel = types.get(3);
		    mflt = lmarkers;
		    markerseq = -1;
		} else {
		    typesel.sel = types.get(2);
		    mflt = slmarkers;
		    markerseq = -1;
		}

		if(namesel == null) {
		    namesel = MapMarkerWnd.this.add(new TextEntry(200, "") {
			{dshow = true;}
			public void activate(String text) {
			    mark.nm = text;
			    map.view.file.update(mark);
			    commit();
			    change2(null);
			}
		    }, new Coord(0, MapMarkerWnd.this.csz.y));
		}
		namesel.settext(mark.nm);
		namesel.buf.point = mark.nm.length();
		namesel.commit();
		if(mark instanceof MapFile.PMarker) {
		    MapFile.PMarker pm = (MapFile.PMarker)mark;
		    colsel = MapMarkerWnd.this.add(new BuddyWnd.GroupSelector(0) {
			public void changed(int group) {
			    this.group = group;
			    pm.color = BuddyWnd.gc[group];
			    map.view.file.update(mark);
			}
		    }, namesel.c.add(0, namesel.sz.y + 10));
		    if((colsel.group = Utils.index(BuddyWnd.gc, pm.color)) < 0)
			colsel.group = 0;
		    mremove = MapMarkerWnd.this.add(new Button(200, "Remove", false) {
			public void click() {
			    map.view.file.remove(mark);
			    change2(null);
			}
		    }, colsel.c.add(0, colsel.sz.y + 10));
		} else if(mark instanceof MapFile.SlothMarker) {
		    MapFile.SlothMarker pm = (MapFile.SlothMarker)mark;
		    colsel = MapMarkerWnd.this.add(new BuddyWnd.GroupSelector(0) {
			public void changed(int group) {
			    this.group = group;
			    pm.color = BuddyWnd.gc[group];
			    map.view.file.update(mark);
			}
		    }, namesel.c.add(0, namesel.sz.y + 10));
		    if((colsel.group = Utils.index(BuddyWnd.gc, pm.color)) < 0)
			colsel.group = 0;
		    if(mark instanceof MapFile.LinkedMarker) {
			linker = MapMarkerWnd.this.add(new Dropbox<MapFile.Marker>(200, 5, 20) {
			    private List<MapFile.Marker> lst;
			    {
				if(map.view.file.lock.readLock().tryLock()) {
				    try {
					lst = map.view.file.markers.stream()
						.filter(m -> m != mark && m instanceof MapFile.LinkedMarker &&
								MapFile.canLink(((MapFile.LinkedMarker) mark).type, ((MapFile.LinkedMarker) m).type))
						.sorted(mcmp).collect(java.util.stream.Collectors.toList());
					list.display();
				    } finally {
					map.view.file.lock.readLock().unlock();
				    }
				}
				if(((MapFile.LinkedMarker) mark).lid != -1)
				    sel = map.view.file.lmarkers.get(((MapFile.LinkedMarker) mark).lid);
			    }

			    @Override
			    public void change(MapFile.Marker item) {
				super.change(item);
				final MapFile.LinkedMarker link = (MapFile.LinkedMarker) item;
				if (((MapFile.LinkedMarker) mark).lid != -1) {
				    map.view.file.lmarkers.get(((MapFile.LinkedMarker) mark).lid).lid = -1;
				}
				((MapFile.LinkedMarker) mark).lid = link.id;
				link.lid = ((MapFile.LinkedMarker) mark).id;
				map.view.file.update(mark);
				map.view.file.update(link);
			    }

			    @Override
			    protected MapFile.Marker listitem(int i) {
				return lst.get(i);
			    }

			    @Override
			    protected int listitems() {
				return lst.size();
			    }

			    @Override
			    protected void drawitem(GOut g, MapFile.Marker item, int i) {
				FastText.aprintf(g, new Coord(5, itemh/2), 0.0, 0.5, "[%d] %s", ((MapFile.LinkedMarker) item).id, item.nm);
			    }
			}, colsel.c.add(0, colsel.sz.y + 10));
		    }
		}
		MapMarkerWnd.this.pack();
	    }
	    MapMarkerWnd.this.show();
	}
    }
}
