/*
 *  This file is part of the Haven & Hearth game client.
 *  Copyright (C) 2009 Fredrik Tolf <fredrik@dolda2000.com>, and
 *                     Björn Johannessen <johannessen.bjorn@gmail.com>
 *
 *  Redistribution and/or modification of this file is subject to the
 *  terms of the GNU Lesser General Public License, version 3, as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  Other parts of this source tree adhere to other copying
 *  rights. Please see the file `COPYING' in the root directory of the
 *  source tree for details.
 *
 *  A copy the GNU Lesser General Public License is distributed along
 *  with the source tree of which this file is a part in the file
 *  `doc/LPGL-3'. If it is missing for any reason, please see the Free
 *  Software Foundation's website at <http://www.fsf.org/>, or write
 *  to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
 */

package haven;

import static haven.DefSettings.*;
import static haven.PUtils.blurmask2;
import static haven.PUtils.rasterimg;
import static haven.Resource.BUNDLE_WINDOW;
import static haven.Resource.cdec;
import haven.HiddenWndData;

import haven.MovableWidget;
import haven.Theme;
import haven.DefSettings;
import haven.purus.pbot.PBotUtils;
import haven.res.ui.tt.Wear;
import haven.resutil.Curiosity;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.*;
import java.util.List;
import java.util.stream.Collectors;

public class Window extends MovableWidget implements DTarget {
    @Resource.LayerName("windowconfig")
    public static class WindowConfig extends Resource.Layer {
	final Coord tlc;
	final Coord brc;
	final Coord capc;
	final Coord btnc;

	public WindowConfig(Resource res, Message buf) {
	    res.super();
	    tlc = cdec(buf);
	    brc = cdec(buf);
	    capc = cdec(buf);
	    btnc = cdec(buf);
	}

	public void init() {}
    }

    // 0 = bg, 1 = bgl, 2 = bgr
    // 3 = capl, 4 = capm, 5 = capr
    // 6 = bl, 7 = br
    // 8 = l, 9 = r, 10 = b
    private static final Resource res = Theme.res("window");

    //bg, left bg, right bg
    public static final TexI bg = res.layer(Resource.imgc, 0).texi();
    public static final TexI bgl = res.layer(Resource.imgc, 1).texi();
    public static final TexI bgr = res.layer(Resource.imgc, 2).texi();
    //caption left, mid, right
    public static final TexI cl = res.layer(Resource.imgc, 3).texi();
    public static final TexI cm = res.layer(Resource.imgc, 4).texi();
    public static final TexI cr = res.layer(Resource.imgc, 5).texi();
    // bottom left, right
    public static final TexI bl = res.layer(Resource.imgc, 6).texi();
    public static final TexI br = res.layer(Resource.imgc, 7).texi();
    //left, right, bottom
    public static final TexI lm = res.layer(Resource.imgc, 8).texi();
    public static final TexI rm = res.layer(Resource.imgc, 9).texi();
    public static final TexI bm = res.layer(Resource.imgc, 10).texi();

    //top left corner, bottom right corner, caption position
    public static final WindowConfig cfg = res.layer(WindowConfig.class);
    public static boolean CurioReport = false;
    Collection Curios = new ArrayList();
    public Button checkcurios;
    public Label curiosliderlabel, studyhours;
    private Widget curiotarget, curiohigh, curiolow;
    public HSlider curioslider;
    public int curiocount =0 ;
    public boolean justclose = false;
    //Large margin vs small margin
    public static final Coord dlmrgn = new Coord(23, 14), dsmrgn = new Coord(3, 3);
    //caption foundry
    public static final BufferedImage ctex = Resource.loadimg("gfx/hud/fonttex");
    public static final Text.Furnace cf = new Text.Imager(new PUtils.TexFurn(new Text.Foundry(Text.sans, 15).aa(true), ctex)) {
        protected BufferedImage proc(Text text) {
            return(rasterimg(blurmask2(text.img.getRaster(), 1, 1, Color.BLACK)));
        }
    };
    //Basic frame box
    public static final IBox wbox = new IBox(Theme.fullres("frame")) {
        final Coord co = new Coord(3, 3), bo = new Coord(2, 2);

	    public Coord btloff() {return(super.btloff().sub(bo));}
	    public Coord ctloff() {return(super.ctloff().sub(co));}
	    public Coord bisz() {return(super.bisz().sub(bo.mul(2)));}
	    public Coord cisz() {return(super.cisz().sub(co.mul(2)));}
	};

    //margin based off large or not
    public final Coord mrgn;
    //close button
    public final IButton cbtn, lbtn;
    private IButton hbtn;
    private final BufferedImage on, off;
    public final ArrayList<IButton> btns = new ArrayList<>();

    public boolean dt = false;
    //Caption
    public Text cap;
    public String origcap;
    //Window size, usable space top left, usable space size
    public Coord wsz, atl, asz;
    //close position, close size
    public Coord ctl, csz;
    private boolean hidable = false, hidden;
    private final Collection<Widget> twdgs = new LinkedList<Widget>();
    @RName("wnd")
    public static class $_ implements Factory {
        public Widget create(UI ui, Object[] args) {
            Coord sz = (Coord)args[0];
            String cap = (args.length > 1)?(String)args[1]:null;
            boolean lg = (args.length > 2) && ((Integer)args[2] != 0);
            if(!Config.stackwindows && cap != null && cap.equals((Resource.getLocString(Resource.BUNDLE_WINDOW, "Cupboard")))) {
                return (new Window(sz, cap, lg, Coord.z, Coord.z));
            } else {
                return (new Window(sz, cap, cap, lg, Coord.z, Coord.z));
            }
        }
    }


    public Window(Coord sz, String cap, boolean lg, Coord tlo, Coord rbo) {
	this.mrgn = lg?dlmrgn:dsmrgn;
	cbtn = add(new IButton(Theme.fullres("buttons/close"), null, this::close));
	lbtn = null;
	on = off = null;
	origcap = cap;

	chcap(Resource.getLocString(Resource.BUNDLE_WINDOW, cap));
	resize2(sz);
	setfocustab(true);
    }

    public Window(Coord sz, String cap, final String moveKey, boolean lg, Coord tlo, Coord rbo) {
        super(moveKey);
        this.mrgn = lg ? dlmrgn : dsmrgn;
	cbtn = add(new IButton(Theme.fullres("buttons/close"), null, this::close));
	lbtn = add(new IButton(Theme.fullres("buttons/lock"), null, this::toggleLock));
	on = lbtn.hover;
	off = lbtn.up;
	origcap = cap;
        if(origcap.equals("Belt") || origcap.equals("Inventory") || origcap.equals("Equipment") || origcap.equals("Study") || origcap.equals("Chat") || origcap.equals("Character Sheet") || origcap.equals("Timers"))
            makeHidable();
        chcap(Resource.getLocString(Resource.BUNDLE_WINDOW, cap));
        resize2(sz);

        setfocustab(true);
    }

    public Window(Coord sz, String cap, boolean lg) {
	this(sz, cap, lg, Coord.z, Coord.z);
    }

    public Window(Coord sz, String cap, final String moveKey, boolean lg) {
	this(sz, cap, moveKey, lg, Coord.z, Coord.z);
    }

    public Window(Coord sz, String cap) {
        this(sz, cap, false);
    }

    public Window(final Coord sz, final String cap, final String moveKey) {
	this(sz, cap, moveKey,false);
    }

    protected void added() {
        parent.setfocus(this);
	super.added();
        if(lbtn != null && locked()) {
            lbtn.up = on;
            lbtn.hover = off;
        }
    }

    public void makeHidable() {
      //  hbtn = add(new IButton("custom/hud/sloth/buttons/hide", "Toggle Transparency", this::toggleHide));
       // hbtn = addBtn("buttons/hide", null, this::toggleHide);
        hbtn = addBtn_other("custom/hud/sloth/buttons/hide", "Toggle Transparency", this::toggleHide);
        if(origcap != null) {
            hidable = HiddenWndData.shouldHide(origcap);

            hidden = false;
            if(hidable) {
                final BufferedImage tmp = hbtn.down;
                hbtn.down = hbtn.up;
                hbtn.up = tmp;
            }
        }
    }

    public void toggleHide() {
        hidable = !hidable;
        hidden = false;
        final BufferedImage tmp = hbtn.down;
        hbtn.down = hbtn.up;
        hbtn.up = tmp;
        if(cap != null) {
            HiddenWndData.saveHide(cap.text, hidable);
        }

    }


    public IButton addBtn(final String res, final String tt, final Runnable action) {
        final IButton btn = add(new IButton("res/"+Theme.fullres(res), tt, action));
        btns.add(btn);
        return btn;
    }

    public IButton addBtn_other(final String res, final String tt, final Runnable action) {
        final IButton btn = add(new IButton(res, tt, action));
        btns.add(btn);
        return btn;
    }


    public void addBtn_base(final String res, final String tt, final Runnable action) {
        btns.add(add(new IButton(res, tt, action)));
    }

    @Override
    public void toggleLock() {
        if(locked()) {
	    lbtn.up = off;
	    lbtn.hover = on;
	} else {
	    lbtn.up = on;
	    lbtn.hover = off;
	}
        super.toggleLock();
    }

    public void chcap(String cap) {
        if (cap == null)
            this.cap = null;
        else
            this.cap = cf.render(cap);
    }

    public class PButton extends Button {
        public final OptWnd.Panel tgt;
        public final int key;

        public PButton(int w, String title, int key, OptWnd.Panel tgt) {
            super(w, title);
            this.tgt = tgt;
            this.key = key;
        }

        public void click() {
            // main(tgt);
        }

        public boolean type(char key, java.awt.event.KeyEvent ev) {
            if ((this.key != -1) && (key == this.key)) {
                click();
                return (true);
            }
            return (false);
        }
    }


    public void cdraw(GOut g) {
    }

    // Input time as minutes
    String sensibleTimeFormat(Double time) {
        StringBuilder sb = new StringBuilder();
        int days = new Double(time / 1440).intValue();
        time -= days * 1440;
        int hours = new Double(time / 60).intValue();
        time -= hours * 60;
        int minutes = time.intValue();
        if (days > 0) {
            sb.append(days + "d ");
        }
        sb.append(hours + "h ");
        sb.append(minutes + "m");
        return sb.toString();
    }

    String sensibleLPFormat(int LP) {
        StringBuilder sb = new StringBuilder();
        int thousands = new Double(LP / 1000).intValue();

        if (thousands > 0) {
            sb.append(thousands + "k LP");
        }
        else
            sb.append(LP + " LP");
        return sb.toString();
    }

    private List<WItem> getfoods(Inventory inv){
        List<WItem> getfoods = inv.getItemsPartial("");
        return getfoods;
    }



    private static HashMap<String, Long> recentlyTakenCutlery = new HashMap<>();
    protected void drawframe(GOut g) {
        // Study Table total LP and durations of curiosities
        Collection GetCurios = new ArrayList(); //add curios from tables to this before parsing
        Collection FinalCurios = new ArrayList(); //parsed out list for checking against the curios you should be studying from Config.curiolist
        Collection CurioCounter = new ArrayList(); //used to see if the number of curios on the table changes to redraw the addons

    if(!HUDTHEME.get().equals("ardclient")) {
        g.chcolor(DefSettings.WNDCOL.get());
        //corners
        g.image(cl, Coord.z);
        g.image(bl, new Coord(0, sz.y - bl.sz().y));
        g.image(br, sz.sub(br.sz()));
        g.image(cr, new Coord(sz.x - cr.sz().x, 0));

        //draw background
        g.rimagev(bgl, ctl, csz.y);
        g.rimagev(bgr, ctl.add(csz.x - bgr.sz().x, 0), csz.y);
        g.rimage(bg, ctl.add(bgl.sz().x, 0), csz.sub(bgl.sz().x + bgr.sz().x, 0));

        //horizontal and vertical tiling of the long parts
        g.rimagev(lm, new Coord(0, cl.sz().y), sz.y - bl.sz().y - cl.sz().y);
        g.rimagev(rm, new Coord(sz.x - rm.sz().x, cr.sz().y), sz.y - br.sz().y - cr.sz().y);
        g.rimageh(bm, new Coord(bl.sz().x, sz.y - bm.sz().y), sz.x - br.sz().x - bl.sz().x);
        g.rimageh(cm, new Coord(cl.sz().x, 0), sz.x - cl.sz().x - cr.sz().x);
        g.chcolor();
    }else {
        g.chcolor(DefSettings.WNDCOL.get());

        //draw background
        g.rimagev(bgl, ctl, csz.y);
        g.rimagev(bgr, ctl.add(csz.x - bgr.sz().x, 0), csz.y);
        g.rimage(bg, ctl.add(bgl.sz().x, 0), csz.sub(bgl.sz().x + bgr.sz().x, 0));

        //corners
        g.image(cl, Coord.z);
        g.image(bl, new Coord(0, sz.y - bl.sz().y));
        g.image(br, sz.sub(br.sz()));
        g.image(cr, new Coord(sz.x - cr.sz().x, 0));

        //horizontal and vertical tiling of the long parts
        g.rimagev(lm, new Coord(0, cl.sz().y), sz.y - bl.sz().y - cl.sz().y);
        g.rimagev(rm, new Coord(sz.x - rm.sz().x, cr.sz().y), sz.y - br.sz().y - cr.sz().y);
        g.rimageh(bm, new Coord(bl.sz().x, sz.y - bm.sz().y), sz.x - br.sz().x - bl.sz().x);
        g.rimageh(cm, new Coord(cl.sz().x, 0), sz.x - cl.sz().x - cr.sz().x);
        g.chcolor();
    }


        try {
            if (this.cap.text.equals (Resource.getLocString(Resource.BUNDLE_WINDOW, "Table"))){
                add(new Button(60, "Eat All") {
                    public void click() {
                        Resource curs = ui.root.getcurs(c);
                        if (curs.name.equals("gfx/hud/curs/eat")) {
                            Map idk = getStats();
                            GameUI gui = gameui();
                            synchronized (gui.ui.root.lchild) {
                                try {
                                    for (Widget q = gui.ui.root.lchild; q != null; q = q.rnext()) {
                                        if (q instanceof Inventory) {
                                            if(q.parent instanceof Window)
                                                if(!((Window)q.parent).cap.text.equals("Study")) {
                                                    List<WItem> foods = getfoods((Inventory) q);
                                                    for (WItem item : foods) {
                                                        if(!item.item.getname().contains("Corn")) {
                                                            GItem food = item.item;
                                                            food.wdgmsg("iact", Coord.z, -1);
                                                        }
                                                    }
                                                }
                                        }
                                    }

                                } catch (Exception q) {}
                            }
                            PBotUtils.sleep(1000);
                            Map idk2 = getStats();
                            idk2.forEach((k,v) ->{
                                if((Integer)idk2.get(k) - (Integer)idk.get(k) > 0) {
                                   // System.out.println("Bulk Stats gained : " + k + " value : " + ((Integer) idk2.get(k) - (Integer) idk.get(k)));
                                    PBotUtils.sysLogAppend("Bulk Stats gained : " + k + " value : " + ((Integer) idk2.get(k) - (Integer) idk.get(k)),"green");
                                }
                               // else
                                   // System.out.println("Old : "+idk.get(k)+" new : "+v);
                            });
                        }
                        else{
                            PBotUtils.sysMsg("Click Feast First!",Color.white);
                        }

                    }
                }, new Coord(140, 325));
                if(Config.savecutlery) {
                    for (Widget w = this.lchild; w != null; w = w.prev) {
                        if (w instanceof Inventory) {
                            for (WItem item : ((Inventory) w).wmap.values()) {
                                for (ItemInfo ii : item.item.info())
                                    if (ii instanceof Wear) {
                                        Wear wr = (Wear) ii;
                                        if (wr.d == wr.m - 1 && item.item.getres() != null && (!recentlyTakenCutlery.containsKey(item.item.getres().name) || System.currentTimeMillis() - recentlyTakenCutlery.get(item.item.getres().name) > 1000 * 60)) { // About to break
                                            item.item.wdgmsg("transfer", Coord.z);
                                            ui.gui.msg("Detected cutlery that is about to break! Taking to inventory! You may want to polish it.", Color.yellow);
                                            recentlyTakenCutlery.put(item.item.getres().name, System.currentTimeMillis());
                                        }
                                    }
                            }
                        }
                    }
                }
            }

            if (this.cap.text.equals (Resource.getLocString(Resource.BUNDLE_WINDOW, "Study Desk"))){
                int sizeY = 285;
                int totalLP = 0;
                int totalAttn = 0;
                HashMap<String, Double> studyTimes = new HashMap<String, Double>();
                HashMap<String, Integer> AttnTotal = new HashMap<String, Integer>();
                List<Curio> curiolist = new ArrayList<>();
                for (Widget wdg = this.lchild; wdg != null; wdg = wdg.prev) {
                    if (wdg instanceof Inventory) {
                        for (WItem item : ((Inventory) wdg).wmap.values()) {
                            try {
                                Curiosity ci = ItemInfo.find(Curiosity.class, item.item.info());
                                totalLP += ci.exp;
                                curiolist.add(new Curio(item.item.getname(), studyTimes.get(item.item.getname()) == null ? item.item.studytime : studyTimes.get(item.item.getname()) + item.item.studytime,ci.exp));
                                studyTimes.put(item.item.getname(), studyTimes.get(item.item.getname()) == null ? item.item.studytime : studyTimes.get(item.item.getname()) + item.item.studytime);
                                AttnTotal.put(item.item.getname(), AttnTotal.get(item.item.getname()) == null ? ci.mw : AttnTotal.get(item.item.getname()));
                            }catch(NullPointerException qq){}
                        }
                    }
                }
                g.image(Text.labelFnd.render("Total LP: " + String.format("%,d", totalLP)).tex(), new Coord(30, 306));

                int y = 320;
                List<Map.Entry<String, Integer>> lst2 = AttnTotal.entrySet().stream().sorted((e1, e2)-> e1.getValue().compareTo(e2.getValue())).collect(Collectors.toList());
                for(Map.Entry<String, Integer> entry : lst2) {
                  totalAttn +=entry.getValue();
                }
                g.image(Text.labelFnd.render("Total Attention: " + String.format("%,d", totalAttn)).tex(), new Coord(30, 293));
                //iterates the curio list to only print out total study times for unique curios
                List<Map.Entry<String, Double>> lst = studyTimes.entrySet().stream().sorted((e1, e2)-> e1.getValue().compareTo(e2.getValue())).collect(Collectors.toList());
                for(Map.Entry<String, Double> entry : lst) {
                CurioCounter.add(entry.getKey());
                    int LP = 0;
                    for(Curio c : curiolist){
                        if(c.CurioName.equals(entry.getKey()))
                            LP += c.LPGain;
                    }
                    if (entry.getValue() > Config.curiotimetarget * 3) {

                        g.image(Text.labelFnd.render(entry.getKey() + ": " + sensibleTimeFormat(entry.getValue())+ " - "+sensibleLPFormat(LP), CURIOHIGH.get()).tex(), new Coord(30, y));
                        y += 15;
                        sizeY += 15;
                        for (int i = 0; i < Curios.size(); i++) {
                            if (Curios.contains(entry.getKey())) {
                                FinalCurios.add(entry.getKey());
                            }
                        }
                        GetCurios.add(entry.getKey());
                    }
                    else if (entry.getValue() < Config.curiotimetarget) {
                        g.image(Text.labelFnd.render(entry.getKey() + ": " + sensibleTimeFormat(entry.getValue())+ " - "+sensibleLPFormat(LP), CURIOLOW.get()).tex(), new Coord(30, y));
                        y += 15;
                        sizeY += 15;
                        for (int i = 0; i < Curios.size(); i++) {
                            if (Curios.contains(entry.getKey())) {
                                FinalCurios.add(entry.getKey());
                            }
                        }
                        GetCurios.add(entry.getKey());

                    } else {
                        g.image(Text.labelFnd.render(entry.getKey() + ": " + sensibleTimeFormat(entry.getValue())+ " - "+sensibleLPFormat(LP), CURIOTARGET.get()).tex(), new Coord(30, y));
                        y += 15;
                        sizeY += 15;
                        for (int i = 0; i < Curios.size(); i++) {
                            if (Curios.contains(entry.getKey())) {
                                FinalCurios.add(entry.getKey());
                            }
                        }
                    }
                    GetCurios.add(entry.getKey());
                }

                if(curiocount != CurioCounter.size()) {
                    //messy as fuck, if curio number changes redraw everything so it's in the right place.
                    if (checkcurios != null)
                        checkcurios.destroy();
                    if (curiotarget != null)
                        curiotarget.destroy();
                    if (curiohigh != null)
                        curiohigh.destroy();
                    if (curiolow != null)
                        curiolow.destroy();
                    if (studyhours != null)
                        studyhours.destroy();
                    if (curiosliderlabel != null)
                        curiosliderlabel.destroy();
                    if (curioslider != null)
                        curioslider.destroy();
                    curiotarget = add(ColorPreWithLabel("Target Color", CURIOTARGET), new Coord(0, y - 5));
                    curiohigh = add(ColorPreWithLabel("High Color", CURIOHIGH), new Coord(0, y + 15));
                    curiolow = add(ColorPreWithLabel("Low Color", CURIOLOW), new Coord(0, y + 35));
                    studyhours = add(new Label(""), new Coord(140, y + 40));
                    curiosliderlabel = add(new Label("Curio Time Target:"), new Coord(0, y + 50));
                    curioslider = add(new HSlider(130, 0, 10080, Config.curiotimetarget) {
                        public void added() {
                            updateLabel();
                        }

                        protected void attach(UI ui) {
                            super.attach(ui);
                            val = (Config.curiotimetarget);
                        }

                        public void changed() {
                            Utils.setprefi("curiotimetarget", val);
                            Config.curiotimetarget = val;
                            updateLabel();
                        }

                        private void updateLabel() {
                            studyhours.settext(String.format("%d Hours", val / 60));
                        }
                    }, new Coord(105, y + 55));
                    checkcurios = add(new Button(110, "Check Curios") {
                        public void click() {
                            CurioReport = true;
                        }
                    }, new Coord(90, y - 5));
                }
                sizeY += 120;
                resize(265,sizeY);
                if (CurioReport) {
                    CurioReport = false;
                    Curios.clear();
                    for (String itm : Config.curioslist.keySet())
                        if (itm != null && Config.curioslist.get(itm))
                            Curios.add(itm);
                    Curios.removeAll(GetCurios);
                    if (!Curios.isEmpty()) {
                        PBotUtils.sysMsg("Missing Curios : " + Curios, Color.WHITE);
                    } else
                        PBotUtils.sysMsg("No Curios missing! GJ bro", Color.WHITE);
                }
            }
        } catch(Loading l) {}
     curiocount = CurioCounter.size(); //set this so we can only trigger the button/label redraw when the value changes.
	//caption if applies
	if(cap != null) {
	    g.image(cap.tex(), cfg.capc);
    	}
    }

    public class Curio{
        private String CurioName;
        private double StudyTime;
        private int LPGain;
        public Curio(String CurioName, double StudyTime, int LPGain) {
           this.CurioName = CurioName;
           this.StudyTime = StudyTime;
           this.LPGain = LPGain;
        }
    }

        public void draw(GOut g) {
        if(!hidden) {
            drawframe(g);
        }
        cdraw(g.reclip(atl, asz));
        super.draw(g);
    }

    private Widget ColorPreWithLabel(final String text, final IndirSetting<Color> cl) {
        final Widget container = new Widget();
        final Label lbl = new Label(text);
        final IndirColorPreview pre = new IndirColorPreview(new Coord(16, 16), cl);
        final int height = Math.max(lbl.sz.y, pre.sz.y) / 2;
        container.add(lbl, new Coord(0, height - lbl.sz.y/2));
        container.add(pre, new Coord(lbl.sz.x, height - pre.sz.y/2));
        container.pack();
        return container;
    }

    public Coord contentsz() {
        Coord max = new Coord(0, 0);
        for(Widget wdg = child; wdg != null; wdg = wdg.next) {
	    if(wdg == cbtn || wdg == lbtn)
                continue;
            if(!wdg.visible)
                continue;
            Coord br = wdg.c.add(wdg.sz);
            if(br.x > max.x)
                max.x = br.x;
            if(br.y > max.y)
                max.y = br.y;
        }
        return(max);
    }

    public void addtwdg(Widget wdg) {
        twdgs.add(wdg);
        placetwdgs();
    }

    protected void placetwdgs() {
        int x = sz.x - 20;
        for(Widget ch : twdgs) {
            if(ch.visible){
                ch.c = xlate(new Coord(x -= ch.sz.x + 5, ctl.y - ch.sz.y/2), false);
            }
        }
    }
    private void placecbtn() {
	cbtn.c = new Coord(sz.x - cbtn.sz.x - atl.x - cfg.btnc.x,-atl.y + cfg.btnc.y);
	final Coord c;
	if(lbtn != null) {
	    lbtn.c = cbtn.c.sub(lbtn.sz.x + 5, 0);
	    c = new Coord(lbtn.c.x - (lbtn.sz.x + 5), lbtn.c.y);
	} else {
	    c = new Coord(cbtn.c.x - (cbtn.sz.x + 5), cbtn.c.y);
	}
	for(final IButton btn : btns) {
	    btn.c = c.copy();
	    c.x -= btn.sz.x + 5;
	}
    }

    private void resize2(Coord sz) {
	asz = sz; //usable size for content
	csz = asz.add(mrgn.mul(2)); //add margin around usable size
	wsz = csz.add(cfg.tlc).add(cfg.brc); //usable size + margin + frame size
	//tlo, rbo = top left offset, bottom right offset usually 0 always...
	//Basically same job as tlc, brc
	this.sz = wsz;
	//top left coordinate of inner content area
	ctl = cfg.tlc;
	//Top left coordinate of where usable space starts after accounting for margin
        atl = ctl.add(mrgn);
	//Where the close button goes
	cbtn.c = new Coord(sz.x - cfg.btnc.x - cbtn.sz.x, cfg.btnc.y);
        for (Widget ch = child; ch != null; ch = ch.next)
            ch.presize();
	placecbtn();
    }

    public void resize(Coord sz) {
	resize2(sz);
    }

    public void uimsg(String msg, Object... args) {
        switch (msg) {
	    case "pack":
            pack();
		break;
	    case "dt":
            dt = (Integer) args[0] != 0;
		break;
	    case "cap":
            String cap = (String) args[0];
            chcap(cap.equals("") ? null : cap);
		break;
	    default:
            super.uimsg(msg, args);
	        break;
        }
    }

    public Coord xlate(Coord c, boolean in) {
        if (in)
            return (c.add(atl));
        else
            return (c.sub(atl));
    }

    @Override
    protected boolean moveHit(Coord c, int btn) {
	Coord cpc = c.sub(cl.sz().x, 0);
	Coord cprc = c.sub(sz.x - cr.sz().x, 0);
	//content size
	return c.isect(ctl, csz) ||
		//or left caption
		(c.isect(Coord.z, cl.sz()) && cl.back.getRaster().getSample(c.x, c.y, 3) >= 128) ||
		//or right caption
		(c.isect(new Coord(sz.x - cr.sz().x, 0), cr.sz()) &&
			cr.back.getRaster().getSample(cprc.x % cr.back.getWidth(), cprc.y, 3) >= 128) ||
		//or mid caption
		(c.isect(new Coord(cl.sz().x, 0), new Coord(sz.x - cr.sz().x - cl.sz().x, cm.sz().y)) &&
			(cm.back.getRaster().getSample(cpc.x % cm.back.getWidth(), cpc.y, 3) >= 128));
    }
    public boolean mousedown(Coord c, int button) {
        if(button == 4 || button == 5) //ignore these because why allow every mousedown to move the window?
            return false;
        if (super.mousedown(c, button)) {
            parent.setfocus(this);
            raise();
            return (true);
        }
        return (false);
    }

    public boolean mouseup(Coord c, int button) {
        super.mouseup(c, button);
        return (true);
    }

    public void mousemove(Coord c) {
        if (hidable) {
            if (c.isect(Coord.z, sz) || moving()) {
                hidden = false;
                cbtn.visible = true;
                if (lbtn != null)
                    lbtn.visible = true;
                btns.forEach(btn -> btn.visible = true);
            } else {
                hidden = true;
                cbtn.visible = false;
                if (lbtn != null)
                    lbtn.visible = false;
                btns.forEach(btn -> btn.visible = false);
            }
        }
        super.mousemove(c);
        }
    public void close() {
        wdgmsg("close");
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (sender == cbtn) {
                close();
        } else {
            super.wdgmsg(sender, msg, args);
        }
    }


    public boolean type(char key, java.awt.event.KeyEvent ev) {
        if (super.type(key, ev))
            return (true);
        if (key == 27 && Config.escclosewindows) {
            if(!this.origcap.equals("Chat") && !this.origcap.equals("Minimap")) {
                wdgmsg("close");
                return (true);
            }
        }
        return (false);
    }

    public boolean drop(Coord cc, Coord ul) {
        if (dt) {
            wdgmsg("drop", cc);
            return (true);
        }
        return (false);
    }

    public boolean iteminteract(Coord cc, Coord ul) {
        return (false);
    }

    public Object tooltip(Coord c, Widget prev) {
        Object ret = super.tooltip(c, prev);
        if (ret != null)
            return (ret);
        else
            return ("");
    }

    private Map<String, Integer> getStats(){
        CharWnd chrwdg = null;
        Map<String, Integer> statmap = new HashMap<String, Integer>();
        try {
            chrwdg = ((GameUI) parent).chrwdg;
        } catch (Exception e) { // fail silently
        }
        if(chrwdg != null){
            for (CharWnd.Attr attr2 : chrwdg.base) {
              //  System.out.println("name : "+attr2.attr.nm);
                if(attr2.attr.nm.contains("str")){
                    statmap.put("str",attr2.attr.comp);
                }
                if(attr2.attr.nm.contains("agi")){
                    statmap.put("agi",attr2.attr.comp);
                }
                if(attr2.attr.nm.contains("int")){
                    statmap.put("int",attr2.attr.comp);
                }
                if(attr2.attr.nm.contains("con")){
                    statmap.put("con",attr2.attr.comp);
                }
                if(attr2.attr.nm.contains("prc")){
                    statmap.put("prc",attr2.attr.comp);
                }
                if(attr2.attr.nm.contains("csm")){
                    statmap.put("csm",attr2.attr.comp);
                }
                if(attr2.attr.nm.contains("dex")){
                    statmap.put("dex",attr2.attr.comp);
                }
                if(attr2.attr.nm.contains("wil")){
                    statmap.put("wil",attr2.attr.comp);
                }
                if(attr2.attr.nm.contains("psy")){
                    statmap.put("psy",attr2.attr.comp);
                }
            }
        }

      //  statmap.forEach((k, v) -> System.out.println("Key : "+k+" value : "+v));
        return statmap;
    }
}
