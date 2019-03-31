package haven.purus.pbot;

import haven.*;

import java.awt.event.KeyEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class PBotScriptlist extends Window {
	
	private TextEntry search;
	private ScriptList list;

	public PBotScriptlist() {
		super(Coord.z, "PBot New Scripts", "PBot New Scripts");
		
		search = new TextEntry(210, "") {
            @Override
            public boolean type(char c, KeyEvent ev) {
                if (!parent.visible)
                    return false;

                boolean ret = buf.key(ev);
                list.changeFilter(text);
                list.sb.val = 0;
                return true;
            }
		};
		add(search, new Coord(10, 5));
		
		list = new ScriptList(210, 10);
		add(list, new Coord(10, 35));
		pack();
	}
	
	@Override
	public boolean show(boolean show) {
		if(show) {
			search.settext("");
			list.refreshItemList();
		}
		return super.show(show);
	}
	
	private static class ScriptList extends Listbox<PBotScriptlistItem> {
		private static final Coord nameoff = new Coord(32, 5);
		
		private String filter = "";
		List<PBotScriptlistItem> itemList, filteredItemList;
		
		public ScriptList(int w, int h) {
			super(w, h, 24);
		}
		
		public void changeFilter(String filter) {
			filteredItemList = itemList.stream()
				.filter(item -> item.getName().toLowerCase().contains(filter.toLowerCase()))
				.collect(Collectors.toList());
		}

		@Override
		protected PBotScriptlistItem listitem(int i) {
			return filteredItemList.get(i);
		}

		@Override
		protected int listitems() {
			return filteredItemList.size();
		}

		@Override
		protected void drawitem(GOut g, PBotScriptlistItem item, int i) {
			try {
				g.image(item.getIconTex(), Coord.z);
				g.image(item.getNameTex(), nameoff);
			} catch (Loading e) {
			}
		}
		
		@Override
		public boolean mousedown(Coord c, int button) {
			PBotScriptlistItem item = itemat(c);
			if(item != null)
				item.runScript();
			return super.mousedown(c, button);
		}
		
		@Override
		protected void drawbg(GOut g) {
			g.chcolor(0, 0, 0, 120);
			g.frect(Coord.z, sz);
			g.chcolor();
		}
		
		public void refreshItemList() {
			itemList = new ArrayList<PBotScriptlistItem>();
			File scriptDirectory = new File("scripts/");
			if(!scriptDirectory.exists())
				scriptDirectory.mkdir();
			for(File f:scriptDirectory.listFiles()) {
				if(f.getName().endsWith(".PBot")) {
					itemList.add(new PBotScriptlistItem(f.getName(), f));
				}
			}
			itemList = itemList.stream()
				.sorted(Comparator.comparing(PBotScriptlistItem::getName))
				.collect(Collectors.toList());
			filteredItemList = itemList;
		}
	}
}
