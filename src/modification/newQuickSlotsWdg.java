package modification;

import haven.Config;
import haven.Coord;
import haven.DTarget;
import haven.Equipory;
import haven.GItem;
import haven.GOut;
import haven.GSprite;
import haven.Indir;
import haven.ItemInfo;
import haven.Loading;
import haven.Resource;
import haven.RichText;
import haven.Tex;
import haven.TexI;
import haven.Text;
import haven.UI;
import haven.Utils;
import haven.WItem;
import haven.Widget;
import haven.res.ui.tt.Wear;
import haven.res.ui.tt.q.qbuff.QBuff;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.Iterator;
import java.util.List;

public class newQuickSlotsWdg extends Widget implements DTarget {
    private static final Tex sbg = configuration.imageToTex("modification/gfx/hud/slots.png"); //Resource.loadtex("gfx/hud/slots");
    public static final int slots = 4;
    public static final Item[] items = new Item[4];
    private static final Coord ssz = new Coord(33, 33);
    private static final Coord spz = new Coord(4, 33);
    public static final Coord leftCoord;
    public static final Coord rightCoord;
    public static final Coord beltCoord;
    public static final Coord capeCoord;
    private ItemTip shorttip = null;
    private ItemTip longtip = null;
    private List ttinfo = null;
    public GItem item;
    private double hoverstart;
    private UI.Grab dragging;
    private Coord dc;
    private static final Color qualitybg;

    public newQuickSlotsWdg() {
        super(new Coord(ssz.x * 4 + spz.x * 3, ssz.y));
    }

    public static BufferedImage shorttip(List info) {
        return ItemInfo.shorttip(info);
    }

    public static BufferedImage longtip(GItem item, List info) {
        BufferedImage img = ItemInfo.longtip(info);
        Resource.Pagina pg = (item.res.get()).layer(Resource.pagina);
        if (pg != null) {
            img = ItemInfo.catimgs(0, img, RichText.render("\n" + pg.text, 200).img);
        }

        return img;
    }

    public BufferedImage longtip(List info) {
        return longtip(this.item, info);
    }

    public void draw(GOut g) {
        Equipory e = gameui().getequipory();
        if (e != null) {
            g.image(sbg, Coord.z);
            new Item(4, 14);
            new Item(3, 5);
            new Item(2, 7);
            new Item(1, 6);
            Item[] var3 = items;
            int var4 = var3.length;

            for (int var5 = 0; var5 < var4; ++var5) {
                Item item = var3[var5];
                create(g, item.wItem, item.coord, item.slot);
            }
        }

    }

    private void create(GOut g, WItem wItem, Coord coord, int slot) {
        if (wItem != null) {
            drawitem(g.reclipl(coord, g.sz), wItem);
            drawamountbar(g, wItem.item, (4 - slot) * (ssz.x + spz.x));
            drawwearbar(g, wItem.item, (4 - slot) * (ssz.x + spz.x));
            if (Config.showquality) {
                drawQuality(g, wItem, slot);
            }
        }

    }

    private void drawitem(GOut g, WItem witem) {
        GItem item = witem.item;
        GSprite spr = item.spr();
        if (spr != null) {
            g.defstate();
            witem.drawmain(g, spr);
            g.defstate();
        } else {
            g.image((WItem.missing.layer(Resource.imgc)).tex(), Coord.z, ssz);
        }

    }

    private void drawQuality(GOut g, WItem witem, int slot) {
        QBuff quality = witem.item.quality();
        if (Config.showquality && quality != null && quality.qtex != null) {
            Coord btm = new Coord((slot - 1) * (ssz.x + spz.x), this.sz.y - 15);
            Tex t = Config.qualitywhole ? quality.qwtex : quality.qtex;
            if (Config.qualitybg) {
                g.chcolor(qualitybg);
                g.frect(btm, t.sz().add(1, -1));
                g.chcolor();
            }

            g.image(t, btm);
        }

    }

    public void drawamountbar(GOut g, GItem item, int offset) {
        if (item.spr() != null) {
            try {
                Iterator var4 = item.info().iterator();

                while (var4.hasNext()) {
                    ItemInfo info = (ItemInfo) var4.next();
                    if (info instanceof ItemInfo.Contents) {
                        ItemInfo.Contents imtcnt = (ItemInfo.Contents) info;
                        if (imtcnt.content > 0.0D) {
                            if (item.getname().contains("Bucket")) {
                                double capacity = imtcnt.isseeds ? 1000.0D : 10.0D;
                                double content = imtcnt.content;
                                int height = this.sz.y - 2;
                                int h = (int) (content / capacity * (double) height);
                                g.chcolor(WItem.famountclr);
                                g.frect(new Coord(this.sz.x - spz.x - offset, height - h + 1), new Coord(3, h));
                                g.chcolor();
                                return;
                            }

                            return;
                        }
                    }
                }
            } catch (Exception var13) {
            }
        }

    }

    public void drawwearbar(GOut g, GItem item, int offset) {
        if (item.spr() != null) {
            try {
                Iterator var4 = item.info().iterator();

                while (var4.hasNext()) {
                    ItemInfo info = (ItemInfo) var4.next();
                    if (info instanceof Wear) {
                        double d = (double) ((Wear) info).d;
                        double m = (double) ((Wear) info).m;
                        double p = (m - d) / m;
                        int height = this.sz.y - 2;
                        int h = (int) (p * (double) this.sz.y);
                        g.chcolor(WItem.wearclr[p == 1.0D ? 3 : (int) (p / 0.25D)]);
                        g.frect(new Coord(this.sz.x - spz.x - offset, height - h + 1), new Coord(3, h));
                        g.chcolor();
                        break;
                    }
                }
            } catch (Exception var14) {
            }
        }

    }

    public boolean drop(Coord cc, Coord ul) {
        Equipory e = this.gameui().getequipory();
        if (e != null) {
            int sl = -1;

            for (int i = 0; i < 4; ++i) {
                if (cc.x <= (ssz.x + spz.x / 2) * items[i].slot) {
                    sl = i;
                    break;
                }
            }

            if (sl >= 0) {
                e.wdgmsg("drop", new Object[]{items[sl].eqslot});
                return true;
            }
        }

        return false;
    }

    public boolean iteminteract(Coord cc, Coord ul) {
        Equipory e = this.gameui().getequipory();
        if (e != null) {
            WItem w = null;

            for (int i = 0; i < 4; ++i) {
                if (cc.x <= (ssz.x + spz.x / 2) * items[i].slot) {
                    w = items[i].wItem;
                    break;
                }
            }

            if (w != null) {
                return w.iteminteract(cc, ul);
            }
        }

        return false;
    }

    public boolean mousedown(Coord c, int button) {
        if (this.ui.modmeta) {
            return true;
        } else if (this.ui.modctrl && button == 1 && Config.disablequickslotdrop) {
            return true;
        } else {
            Equipory e = this.gameui().getequipory();
            if (e != null) {
                WItem w = null;

                for (int i = 0; i < 4; ++i) {
                    if (c.x <= (ssz.x + spz.x / 2) * items[i].slot) {
                        w = items[i].wItem;
                        break;
                    }
                }

                if (w != null) {
                    this.dragging = null;
                    w.mousedown(new Coord(w.sz.x / 2, w.sz.y / 2), button);
                    return true;
                }

                if (button == 1) {
                    this.dragging = this.ui.grabmouse(this);
                    this.dc = c;
                    return true;
                }
            }

            return false;
        }
    }

    public void simulateclick(Coord c) {
        Equipory e = this.gameui().getequipory();
        if (e != null) {
            WItem w = null;

            for (int i = 0; i < 4; ++i) {
                if (c.x <= (ssz.x + spz.x / 2) * items[i].slot) {
                    w = items[i].wItem;
                    break;
                }
            }

            if (w != null) {
                w.item.wdgmsg("take", new Object[]{new Coord(w.sz.x / 2, w.sz.y / 2)});
            }
        }

    }

    public boolean mouseup(Coord c, int button) {
        if (this.dragging != null) {
            this.dragging.remove();
            this.dragging = null;
            Utils.setprefc("newQuickSlotWdgc", this.c);
            return true;
        } else {
            return super.mouseup(c, button);
        }
    }

    public void mousemove(Coord c) {
        if (this.dragging != null) {
            this.c = this.c.add(c.x, c.y).sub(this.dc);
        } else {
            super.mousemove(c);
        }
    }

    public Object tooltip(Coord c, Widget prev) {
        Object tt = super.tooltip(c, prev);
        double now = Utils.rtime();
        if (tt != null) {
            return tt;
        } else {
            int sl = -1;

            for (int i = 0; i < 4; ++i) {
                if (c.x <= (ssz.x + spz.x / 2) * items[i].slot) {
                    sl = i;
                    break;
                }
            }

            if (sl >= 0) {
                Widget it = items[sl].wItem;
                if (it != null) {
                    if (it != this) {
                        if (it instanceof WItem) {
                            double ps = ((WItem) it).hoverstart;
                            if (now - ps < 1.0D) {
                                this.hoverstart = now;
                            } else {
                                this.hoverstart = ps;
                            }
                        } else {
                            this.hoverstart = now;
                        }
                    }

                    try {
                        List info = items[sl].gItem.info();
                        if (info.size() < 1) {
                            return null;
                        } else {
                            if (info != this.ttinfo) {
                                this.shorttip = this.longtip = null;
                                this.ttinfo = info;
                            }

                            if (now - this.hoverstart < 1.0D && !Config.longtooltips) {
                                if (this.shorttip == null) {
                                    this.shorttip = new ShortTip(info);
                                }

                                return this.shorttip;
                            } else {
                                if (this.longtip == null) {
                                    this.longtip = new LongTip(items[sl].gItem, info);
                                }

                                return this.longtip;
                            }
                        }
                    } catch (Exception var10) {
                        var10.printStackTrace();
                        return "...";
                    }
                } else {
                    return items[sl].tts;
                }
            } else {
                return null;
            }
        }
    }

    static {
        leftCoord = new Coord((ssz.x + spz.x) * 0, 1);
        rightCoord = new Coord((ssz.x + spz.x) * 1, 1);
        beltCoord = new Coord((ssz.x + spz.x) * 2, 1);
        capeCoord = new Coord((ssz.x + spz.x) * 3, 1);
        qualitybg = new Color(20, 20, 20, 255 - Config.qualitybgtransparency);
    }

    public class LongTip extends ItemTip {
        public LongTip(GItem item, List info) {
            super(longtip(item, info));
        }
    }

    public class ShortTip extends ItemTip {
        public ShortTip(List info) {
            super(ItemInfo.shorttip(info));
        }
    }

    public class ItemTip implements Indir {
        private final TexI tex;

        public ItemTip(BufferedImage img) {
            if (img == null) {
                throw new Loading();
            } else {
                this.tex = new TexI(img);
            }
        }

        public GItem item() {
            return item;
        }

        public Tex get() {
            return this.tex;
        }
    }

    public class Item {
        WItem wItem = null;
        GItem gItem = null;
        public Coord coord;
        int slot;
        int eqslot;
        Text tts;

        public Item(int slot, int eqslot) {
            Equipory e = gameui().getequipory();
            if (e != null) {
                this.slot = slot;
                this.eqslot = eqslot;
                this.wItem = e.quickslots[eqslot];
                if (this.wItem != null) {
                    this.gItem = this.wItem.item;
                }

                this.coord = new Coord((ssz.x + spz.x) * (slot - 1) + 1, 1);
                Resource bgres = Resource.local().loadwait("gfx/hud/equip/ep" + eqslot);
                Resource.Image img = bgres.layer(Resource.imgc);
                if (img != null) {
                    this.tts = Text.render((bgres.layer(Resource.tooltip)).t);
                }

                items[slot - 1] = this;
            }

        }
    }
}
