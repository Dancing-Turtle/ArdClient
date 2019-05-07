package haven.res.ui.barterbox;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.List;
import java.util.Random;

import haven.Button;
import haven.Coord;
import haven.GItem;
import haven.GOut;
import haven.GSprite;
import haven.GSprite.Owner;
import haven.Glob;
import haven.Indir;
import haven.Inventory;
import haven.ItemInfo;
import haven.ItemInfo.SpriteOwner;
import haven.Label;
import haven.Loading;
import haven.Message;
import haven.MessageBuf;
import haven.ResData;
import haven.Resource;
import haven.Resource.Image;
import haven.Resource.Pagina;
import haven.RichText;
import haven.Tex;
import haven.TexI;
import haven.Text;
import haven.TextEntry;
import haven.UI;
import haven.Utils;
import haven.WItem;
import haven.Widget;
import haven.res.lib.tspec.Spec;
import haven.res.ui.tt.q.qbuff.QBuff;

// ui/barterstand
public class Shopbox extends Widget implements SpriteOwner, Owner {
    public static final Text any = Text.render(Resource.getLocString(Resource.BUNDLE_LABEL, "Any"));
    public static final Text qlbl = Text.render(Resource.getLocString(Resource.BUNDLE_LABEL, "Quality:"));
    public static final Tex bg = Resource.loadtex("ui/shopbox");
    public static final Coord itemc = new Coord(5, 5);
    public static final Coord buyc = new Coord(5, 66);
    public static final Coord pricec = new Coord(200, 5);
    public static final Coord qualc = (new Coord(200, 5)).add(Inventory.invsq.sz()).add(40, 0);
    public static final Coord cbtnc = new Coord(200, 66);
    public static final Coord spipec = new Coord(85, 66);
    public static final Coord bpipec = new Coord(280, 66);
    public ResData res;
    public Spec price;
    public Text num;
    public int pnum;
    public int pq;
    private Text pnumt;
    private Text pqt;
    private GSprite spr;
    private Object[] info = new Object[0];
    private Text quality;
    private Button spipe;
    private Button bpipe;
    private Button bbtn;
    private Button cbtn;
    private TextEntry pnume;
    private TextEntry pqe;
    public final boolean admin;
    public final AttrCache<Tex> itemnum = new One(this);
    private List<ItemInfo> cinfo;
    private Tex longtip = null;
    private Tex pricetip = null;
    private Random rnd = null;

    public static Widget mkwidget(UI ui, Object... var1) {
        boolean var2 = ((Integer) var1[0]).intValue() != 0;
        return new Shopbox(var2);
    }

    public Shopbox(boolean var1) {
        super(bg.sz());
        if (this.admin = var1) {
            this.spipe = (Button)this.add(new Button(75, "Connect"), spipec);
            this.bpipe = (Button)this.add(new Button(75, "Connect"), bpipec);
            this.cbtn = (Button)this.add(new Button(75, "Change"), cbtnc);
            this.pnume = (TextEntry)this.adda(new TextEntry(30, ""), pricec.add(Inventory.invsq.sz()).add(5, 0), 0.0D, 1.0D);
            this.pnume.canactivate = true;
            this.pnume.dshow = true;
            this.adda(new Label("Quality:"), qualc.add(0, 0), 0.0D, 1.0D);
            this.pqe = (TextEntry)this.adda(new TextEntry(40, ""), qualc.add(40, 0), 0.0D, 1.0D);
            this.pqe.canactivate = true;
            this.pqe.dshow = true;
        }
    }

    public void draw(GOut g) {
        g.image(bg, Coord.z);
        ResData var2 = this.res;
        GOut var3;
        if (var2 != null) {
            label56:
            {
                var3 = g.reclip(itemc, Inventory.invsq.sz());
                var3.image(Inventory.invsq, Coord.z);
                GSprite var4 = this.spr;
                if (var4 == null) {
                    try {
                        var4 = this.spr = GSprite.create(this, (Resource) var2.res.get(), var2.sdt.clone());
                    } catch (Loading var7) {
                        var3.image(((Image) WItem.missing.layer(Resource.imgc)).tex(), Coord.z, Inventory.sqsz);
                        break label56;
                    }
                }

                var4.draw(var3);
                if (this.itemnum.get() != null) {
                    var3.aimage((Tex) this.itemnum.get(), Inventory.sqsz, 1.0D, 1.0D);
                }

                if (this.num != null) {
                    g.aimage(this.num.tex(), itemc.add(Inventory.invsq.sz()).add(5, 0), 0.0D, 2.3D);
                }

                if (quality != null) {
                    g.aimage(qlbl.tex(), itemc.add(Inventory.invsq.sz()).add(5, 0), 0.0D, 1.0D);
                    g.aimage(quality.tex(), itemc.add(Inventory.invsq.sz()).add(8 + qlbl.tex().sz().x, 0), 0.0D, 1.0D);
                }
            }
        }

        Spec var8 = this.price;
        if(var8 != null) {
            var3 = g.reclip(pricec, Inventory.invsq.sz());
            var3.image(Inventory.invsq, Coord.z);

            try {
                var8.spr().draw(var3);
            } catch (Loading var6) {
                var3.image(((Image)WItem.missing.layer(Resource.imgc)).tex(), Coord.z, Inventory.sqsz);
            }

            if(!this.admin && this.pnumt != null) {
                g.aimage(this.pnumt.tex(), pricec.add(Inventory.invsq.sz()), 0.0D, 1.0D);
            }

            if(!this.admin && this.pqt != null) {
                g.aimage(qlbl.tex(), qualc, 0.0D, 1.0D);
                g.aimage(this.pqt.tex(), qualc.add(qlbl.tex().sz().x + 4, 0), 0.0D, 1.0D);
            }
        }

        super.draw(g);
    }

    public List<ItemInfo> info() {
        if (this.cinfo == null) {
            this.cinfo = ItemInfo.buildinfo(this, this.info);
            QBuff qb = quality();
            if (qb != null)
                quality = Text.render((int) qb.q + "");
        }
        return this.cinfo;
    }

    private QBuff getQBuff(List<ItemInfo> infolist) {
        for (ItemInfo info : infolist) {
            if (info instanceof QBuff)
                return (QBuff) info;
        }
        return null;
    }

    private QBuff quality() {
        try {
            for (ItemInfo info : info()) {
                if (info instanceof ItemInfo.Contents)
                    return getQBuff(((ItemInfo.Contents) info).sub);
            }
            return getQBuff(info());
        } catch (Loading l) {
        }
        return null;
    }

    public Object tooltip(Coord var1, Widget var2) {
        ResData var3 = this.res;
        if (var1.isect(itemc, Inventory.sqsz) && var3 != null) {
            try {
                if (this.longtip == null) {
                    BufferedImage var4 = ItemInfo.longtip(this.info());
                    Pagina var5 = ((Resource) var3.res.get()).layer(Resource.pagina);
                    if (var5 != null) {
                        var4 = ItemInfo.catimgs(0, new BufferedImage[]{var4, RichText.render("\n" + var5.text, 200, new Object[0]).img});
                    }

                    this.longtip = new TexI(var4);
                }

                return this.longtip;
            } catch (Loading var6) {
                return "...";
            }
        } else if (var1.isect(pricec, Inventory.sqsz) && this.price != null) {
            try {
                if (this.pricetip == null) {
                    this.pricetip = this.price.longtip();
                }

                return this.pricetip;
            } catch (Loading var7) {
                return "...";
            }
        } else {
            return super.tooltip(var1, var2);
        }
    }

    @Deprecated
    public Glob glob() {
        return this.ui.sess.glob;
    }

    public Resource resource() {
        return (Resource) this.res.res.get();
    }

    public GSprite sprite() {
        if (this.spr == null) {
            throw new Loading("Still waiting for sprite to be constructed");
        } else {
            return this.spr;
        }
    }

    public Resource getres() {
        return (Resource) this.res.res.get();
    }

    public Random mkrandoom() {
        if (this.rnd == null) {
            this.rnd = new Random();
        }

        return this.rnd;
    }

    private static Integer parsenum(TextEntry var0) {
        try {
            return var0.buf.line.equals("") ? Integer.valueOf(0) : Integer.valueOf(Integer.parseInt(var0.buf.line));
        } catch (NumberFormatException var2) {
            return null;
        }
    }

    public boolean mousedown(Coord var1, int var2) {
        if (var2 == 3 && var1.isect(pricec, Inventory.sqsz) && this.price != null) {
            this.wdgmsg("pclear", new Object[0]);
            return true;
        } else {
            return super.mousedown(var1, var2);
        }
    }

    public void wdgmsg(Widget var1, String var2, Object... var3) {
        if(var1 == this.bbtn) {
            if(ui.modshift && !ui.modctrl) {
                for (int i = 0; i <= 5; i++)
                    this.wdgmsg("buy", new Object[0]);
            }else if(ui.modshift && ui.modctrl) {
                for (int i = 0; i <= 20; i++)
                    this.wdgmsg("buy", new Object[0]);
            }else {
                this.wdgmsg("buy", new Object[0]);
            }
        } else if(var1 == this.spipe) {
            this.wdgmsg("spipe", new Object[0]);
        } else if(var1 == this.bpipe) {
            this.wdgmsg("bpipe", new Object[0]);
        } else if(var1 == this.cbtn) {
            this.wdgmsg("change", new Object[0]);
        } else if(var1 != this.pnume && var1 != this.pqe) {
            super.wdgmsg(var1, var2, var3);
        } else {
            this.wdgmsg("price", new Object[]{parsenum(this.pnume), parsenum(this.pqe)});
        }
    }

    private void updbtn() {
        boolean var1 = this.price != null && this.pnum > 0;
        if (var1 && this.bbtn == null) {
            this.bbtn = (Button) this.add(new Button(75, "Buy"), buyc);
            this.bbtn.tooltip = "Left click to buy 1, Shift left click to buy 5, ctrl shift left click to buy 20.";
        } else if (!var1 && this.bbtn != null) {
            this.bbtn.reqdestroy();
            this.bbtn = null;
        }

    }

    private static Text rnum(String var0, int var1) {
        return var1 < 1 ? null : Text.render(String.format(var0, new Object[]{Integer.valueOf(var1)}));
    }

    public void uimsg(String var1, Object... var2) {
        if (var1 == "res") {
            this.res = null;
            this.spr = null;
            if (var2.length > 0) {
                ResData var3 = new ResData(this.ui.sess.getres(((Integer) var2[0]).intValue()), Message.nil);
                if (var2.length > 1) {
                    var3.sdt = new MessageBuf((byte[]) ((byte[]) var2[1]));
                }

                this.res = var3;
            }
        } else if (var1 == "tt") {
            this.info = var2;
            this.cinfo = null;
            this.longtip = null;
        } else {
            int var7;
            if (var1 == "n") {
                var7 = ((Integer) var2[0]).intValue();
                this.num = Text.render(String.format("%d left", new Object[]{Integer.valueOf(var7)}));
            } else if (var1 == "price") {
                byte var8 = 0;
                if (var2[var8] == null) {
                    var7 = var8 + 1;
                    this.price = null;
                } else {
                    var7 = var8 + 1;
                    Indir<Resource> var4 = this.ui.sess.getres(((Integer) var2[var8]).intValue());
                    Object var5 = Message.nil;
                    if (var2[var7] instanceof byte[]) {
                        var5 = new MessageBuf((byte[]) ((byte[]) var2[var7++]));
                    }

                    Object var6 = null;
                    if (var2[var7] instanceof Object[]) {
                        for (var6 = new Object[0][]; var2[var7] instanceof Object[]; var6 = Utils.extend((Object[]) var6, var2[var7++])) {
                            ;
                        }
                    }

                    this.price = new Spec(new ResData(var4, (Message)var5), Spec.uictx(this.ui), (Object[])var6);
                }

                this.pricetip = null;
                this.pnum = ((Integer) var2[var7++]).intValue();
                this.pq = ((Integer) var2[var7++]).intValue();
                if (!this.admin) {
                    this.pnumt = rnum("×%d", this.pnum);
                    this.pqt = this.pq > 0 ? rnum("%d+", this.pq) : any;
                } else {
                    this.pnume.settext(this.pnum > 0 ? Integer.toString(this.pnum) : "");
                    this.pnume.commit();
                    this.pqe.settext(this.pq > 0 ? Integer.toString(this.pq) : "");
                    this.pqe.commit();
                }

                this.updbtn();
            } else {
                super.uimsg(var1, var2);
            }
        }
    }

    @Override
    public <C> C context(Class<C> var1) {
        return Spec.uictx.context(var1, this.ui);
    }

    public abstract class AttrCache<T> {
        private List<ItemInfo> forinfo;
        private T save;

        public AttrCache(Shopbox var1) {
            this.forinfo = null;
            this.save = null;
        }

        public T get() {
            try {
                List<ItemInfo> var1 = info();
                if (var1 != this.forinfo) {
                    this.save = find(var1);
                    this.forinfo = var1;
                }
            } catch (Loading var2) {
                return null;
            }

            return this.save;
        }

        protected abstract T find(List<ItemInfo> var1);
    }


    class One extends AttrCache<Tex> {
        One(Shopbox var1) {
            super(var1);
        }

        protected Tex find(List<ItemInfo> var1) {
            GItem.NumberInfo var2 = ItemInfo.find(GItem.NumberInfo.class, var1);
            return var2 == null ? null : new TexI(Utils.outline2(Text.render(Integer.toString(var2.itemnum()), Color.WHITE).img, Utils.contrast(Color.WHITE)));
        }
    }
}
