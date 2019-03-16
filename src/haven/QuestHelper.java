package haven;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.util.*;
import java.util.List;

//Ported from mamber, adjusted for public use,  credits to Matias

public class QuestHelper extends Window{
    public boolean active = false;
    public QuestHelper.QuestList questList;

    public QuestHelper() {
        super(Coord.z, "Quest Helper","Quest Helper");
        new Coord(20, 55);
        this.add(new QuestHelper.PButton(80, "Refresh", this.questList), new Coord(100, 20));
        this.questList = new QuestHelper.QuestList(270, 13, this);
        this.add(this.questList, new Coord(10, 55));
        makeHidable();
        pack();
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (sender == this.cbtn) {
            this.hide();
            this.disable();
        } else {
            super.wdgmsg(sender, msg, args);
        }

    }

    public boolean type(char key, KeyEvent ev) {
        if (key == 27) {
            this.hide();
            this.disable();
            return true;
        } else {
            return super.type(key, ev);
        }
    }

    public void refresh() {
        if (this.active) {
            this.questList.quests.clear();
            this.questList.refresh = true;
            this.questList.quests.sort(this.questList.comp);
        }
        presize();
    }

    public void close() {
        hide();
        this.disable();
    }

    public void addConds(List<CharWnd.Quest.Condition> ncond, int id) {
        if (this.active) {
            boolean alltrue = true;

            for(int i = 0; i < ncond.size(); ++i) {
                QuestListItem qitem = new QuestListItem((ncond.get(i)).desc, (ncond.get(i)).done, id);
                if (alltrue && i == ncond.size() - 1) {
                    qitem = new QuestListItem("★ " + (ncond.get(i)).desc, 2, id);
                } else if ((ncond.get(i)).done == 1) {
                    qitem = new QuestListItem("✓ " + (ncond.get(i)).desc, (ncond.get(i)).done, id);
                } else {
                    alltrue = false;
                }

                boolean dontadd = false;
                Iterator var7 = this.questList.quests.iterator();

                while(var7.hasNext()) {
                    QuestListItem item = (QuestListItem)var7.next();
                    if (qitem.name.equals(item.name) && qitem.parentid == item.parentid) {
                        dontadd = true;
                    }
                }

                if (!dontadd) {
                    this.questList.quests.add(qitem);
                }
            }

            this.questList.quests.sort(this.questList.comp);
        }
    }

    private void disable() {
        this.active = false;
    }

    private static class QuestList extends Listbox<QuestListItem> {
        private static final Coord nameoff = new Coord(0, 5);
        public List<QuestListItem> quests = new ArrayList(50);
        public boolean refresh = true;
        private long lastUpdateTime = System.currentTimeMillis();
        private final Comparator<QuestListItem> comp = (a, b) -> { return a.name.compareTo(b.name);};
        private QuestHelper questHelper;

        public QuestList(int w, int h, QuestHelper questHelper) {
            super(w, h, 24);
            this.questHelper = questHelper;
        }

        public void tick(double dt) {
            GameUI gui = this.gameui();
            if (gui != null && gui.menu != null) {
                if (this.questHelper.active) {
                    long timesincelastupdate = System.currentTimeMillis() - this.lastUpdateTime;
                    if (timesincelastupdate < 1000L) {
                        this.refresh = false;
                    }

                    if (this.ui != null && this.refresh) {
                        this.refresh = false;
                        this.lastUpdateTime = System.currentTimeMillis();
                        this.quests.clear();

                        try {
                            Iterator var6 = this.gameui().chrwdg.cqst.quests.iterator();

                            while(var6.hasNext()) {
                                CharWnd.Quest quest = (CharWnd.Quest)var6.next();
                                if (quest.id != this.gameui().chrwdg.credos.pqid) {
                                    this.gameui().chrwdg.wdgmsg("qsel", new Object[]{quest.id});
                                }
                            }
                        } catch (NullPointerException var9) {
                            var9.printStackTrace();
                        } catch (Loading var10) {
                            this.refresh = true;
                            System.out.println("loading...");
                        }

                        Collections.sort(this.quests);
                        if (this.quests.size() > 0) {
                            this.change2(this.quests.get(0));
                        }
                    }
                }

            }
        }

        protected QuestListItem listitem(int idx) {
            return this.quests.get(idx);
        }

        protected int listitems() {
            return this.quests.size();
        }

        protected void drawbg(GOut g) {
            g.chcolor(0, 0, 0, 120);
            g.frect(Coord.z, this.sz);
            g.chcolor();
        }

        protected void drawitem(GOut g, QuestListItem item, int idx) {
            try {
                if (item.status == 2) {
                    g.chcolor(new Color(0, 255, 0));
                } else if (item.status == 1) {
                    g.chcolor(new Color(0, 255, 255));
                } else {
                    g.chcolor(new Color(255, 255, 255));
                }

                    g.text(item.name, nameoff);


                g.chcolor();
            } catch (Loading var5) {}

        }

        public void change(QuestListItem item) {
            if (item != null) {
                super.change(item);
                this.gameui().chrwdg.wdgmsg("qsel", item.parentid);
            }

        }
    }

    private class PButton extends Button {
        public final QuestHelper.QuestList tgt;

        public PButton(int w, String title, QuestHelper.QuestList tgt) {
            super(w, title);
            this.tgt = tgt;
        }

        public void click() {
            QuestHelper.this.refresh();
        }
    }
}
