package haven.purus.pbot;

import haven.Coord;
import haven.GOut;
import haven.GameUI;
import haven.Listbox;
import haven.Loading;
import haven.TextEntry;
import haven.UI;
import haven.Widget;

import java.awt.event.KeyEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class PBotScriptlist extends GameUI.Hidewnd {

    private TextEntry search;
    private ScriptList list;

    boolean dragging;
    PBotScriptlistItem draggedItem;

    public PBotScriptlist() {
        super(new Coord(228, 280), "PBot New Scripts");

        search = new TextEntry(210, "") {
            @Override
            public boolean keydown(KeyEvent ev) {
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
    }

    @Override
    public void draw(GOut g) {
        super.draw(g);
        ui.drawafter(new UI.AfterDraw() {
            public void draw(GOut g) {
                if (dragging && draggedItem != null) {
                    g.image(draggedItem.getIconTex(), ui.mc.add(new Coord(32, 32).div(2).inv()), new Coord(32, 32));
                }
            }
        });
    }

    @Override
    public boolean show(boolean show) {
        if (show) {
            search.settext("");
            list.refreshItemList();
        }
        return super.show(show);
    }

    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (msg.equals("dragstart")) {
            dragging = true;
            draggedItem = (PBotScriptlistItem) args[0];
        } else if (msg.equals("dragstop")) {
            dragging = false;
        } else {
            super.wdgmsg(sender, msg, args);
        }
    }

    private static class ScriptList extends Listbox<PBotScriptlistItem> {
        private static final Coord nameoff = new Coord(32, 5);

        private String filter = "";
        List<PBotScriptlistItem> itemList, filteredItemList;
        private UI.Grab grab;
        PBotScriptlistItem mdItem;

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
            if (i < 0 || i >= filteredItemList.size())
                return null;
            return filteredItemList.get(i);
        }

        @Override
        protected int listitems() {
            return filteredItemList.size();
        }

        @Override
        protected void drawitem(GOut g, PBotScriptlistItem item, int i) {
            try {
                g.image(item.getIconTex(), Coord.z, new Coord(24, 24));
                g.image(item.getNameTex(), nameoff);
            } catch (Loading e) {
            }
        }

        @Override
        public boolean mousedown(Coord c, int button) {
            mdItem = itemat(c);
            if (button == 1 && mdItem != null) {
                grab = ui.grabmouse(this);
                wdgmsg("dragstart", mdItem);
            }
            return true;
        }

        @Override
        public boolean mouseup(Coord c, int button) {
            PBotScriptlistItem item = itemat(c);
            if (item != null && itemat(c) == mdItem)
                item.runScript();
            if (mdItem != null && grab != null) {
                ui.dropthing(ui.root, ui.mc, mdItem);
            }
            if (button == 1 && grab != null) {
                grab.remove();
                grab = null;
                wdgmsg("dragstop");
            }
            mdItem = null;
            return true;
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
            if (!scriptDirectory.exists())
                scriptDirectory.mkdir();
            for (File f : scriptDirectory.listFiles()) {
                if (f.getName().endsWith(".PBot")) {
                    itemList.add(new PBotScriptlistItem(ui, f.getName(), f));
                }
            }
            itemList = itemList.stream()
                    .sorted(Comparator.comparing(PBotScriptlistItem::getName))
                    .collect(Collectors.toList());
            filteredItemList = itemList;
        }
    }
}
