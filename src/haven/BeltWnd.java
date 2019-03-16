package haven;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.util.Optional;
import haven.DefSettings;
import haven.BeltData;

/**
 * Hotkeys are stored in dnyamic.sqlite only for custom items not stored on server-side
 * Table: beltslot
 * Columns:
 * 	character_id
 * 	slot_id
 * 	pagina_key
 *
 */
public class BeltWnd extends MovableWidget {
    private static final Coord offc = new Coord(1,1);

    public class BeltBtn extends Widget implements DTarget, DropTarget {
        //Key to activate
        private final int key;
        private final Tex tkey;
        //Server slot id
        private int slot;
        //What to render, either a Pagina or a Resource, never both
        private MenuGrid.Pagina pag;
        private Indir<Resource> res;
        //For dragging this btn if it has anything
        private boolean dragging = false;
        private UI.Grab dm = null;
        //tooltip
        private BufferedImage tt;

        private BeltBtn(final int key) {
            super(Inventory.invsq.sz());
            this.key = key;
            cancancel = false;
            final String nm;
            if(key >= 96 && key <= 105)
                nm = "N"+KeyEvent.getKeyText(key).substring(KeyEvent.getKeyText(key).length()-1);
            else
            nm = KeyEvent.getKeyText(key).replace("NumPad-", "N");
            this.tkey = new TexI(Utils.outline2(Text.render(nm).img, Color.BLACK));
        }

        private Optional<Tex> img() {
            if(res != null) {
                try {
                    return Optional.of(res.get().layer(Resource.imgc).tex());
                } catch (Loading e) {
                    return Optional.empty();
                }
            } else if(pag != null) {
                return Optional.of(pag.img.get());
            }
            return Optional.empty();
        }

        @Override
        public void draw(GOut g) {
            if(TexGL.disableall)
                return;
            g.image(Inventory.invsq, Coord.z);
            //if we have something draw it
            img().ifPresent(tex -> {
                if(!dragging) {
                    g.image(tex, offc);
                } else {
                    ui.drawafter(g2 -> g2.image(tex, ui.mc.sub(tex.sz().div(2))));
                }
            });
            //always show our hotkey key
            final Coord tkeyc = sz.sub(tkey.sz());
            g.chcolor(new java.awt.Color(128, 128, 128, 128));
            g.frect(tkeyc, new Coord(sz.x, tkeyc.y), sz, new Coord(tkeyc.x, sz.y));
            g.chcolor();
            g.image(tkey, tkeyc);
        }

        private void reset() {
            data.remove(slot);
            res = null;
            pag = null;
            tt = null;
        }

        private void setSlot(final int slot) {
            this.slot = slot;
            tt = null;
            res = null;
            pag = null;
            if(ui.gui.belt[slot] != null) {
                res = ui.gui.belt[slot];
                data.remove(slot);
            } else {
                //Check for any pagina int his slot from db
                data.get(slot).ifPresent(key -> pag = ui.gui.menu.specialpag.get(key));

            }
        }

        private void setPag(final MenuGrid.SpecialPagina pag) {
            data.add(slot, pag.key);
            this.pag = pag;
            res = null;
            tt = null;
        }

        @Override
        public Object tooltip(Coord c, Widget prev) {
            if(tt != null) {
                //cached tt
                return tt;
            } else if(pag != null && pag.act() != null) {
                tt = pag.button().rendertt(true);
                return tt;
            }
            return null; //no tt
        }

        private void use() {
            if(res != null) {
                if (Config.confirmmagic && res.get().name.startsWith("paginae/seid/") && !res.get().name.equals("paginae/seid/rawhide")) {
                    Window confirmwnd = new Window(new Coord(225, 100), "Confirm") {
                        @Override
                        public void wdgmsg(Widget sender, String msg, Object... args) {
                            if (sender == cbtn)
                                reqdestroy();
                            else
                                super.wdgmsg(sender, msg, args);
                        }

                        @Override
                        public boolean type(char key, KeyEvent ev) {
                            if (key == 27) {
                                reqdestroy();
                                return true;
                            }
                            return super.type(key, ev);
                        }
                    };

                    confirmwnd.add(new Label(Resource.getLocString(Resource.BUNDLE_LABEL, "Using magic costs experience points. Are you sure you want to proceed?")),
                            new Coord(10, 20));
                    confirmwnd.pack();
                    Button yesbtn = new Button(70, "Yes") {
                        @Override
                        public void click() {
                            ui.gui.wdgmsg("belt", slot, 1, ui.modflags());
                            parent.reqdestroy();
                        }
                    };
                    confirmwnd.add(yesbtn, new Coord(confirmwnd.sz.x / 2 - 60 - yesbtn.sz.x, 60));
                    Button nobtn = new Button(70, "No") {
                        @Override
                        public void click() {
                            parent.reqdestroy();
                        }
                    };
                    confirmwnd.add(nobtn, new Coord(confirmwnd.sz.x / 2 + 20, 60));
                    confirmwnd.pack();

                    GameUI gui = gameui();
                    gui.add(confirmwnd, new Coord(gui.sz.x / 2 - confirmwnd.sz.x / 2, gui.sz.y / 2 - 200));
                    confirmwnd.show();
                }
                else
                ui.gui.wdgmsg("belt", slot, 1, ui.modflags());
            } else if(pag != null)
                 pag.use();
        }

        @Override
        public boolean globtype(char k, KeyEvent ev) {
            if(k != 0)
                return false;
            if(ev.getKeyCode() == this.key) {
                use();
                return true;
            } else {
                return false;
            }
        }

        @Override
        public boolean mousedown(Coord c, int button) {
            if(((res != null && res.get() != null) || pag != null) && ui.modflags() == 0) {
                dm = ui.grabmouse(this);
                return true;
            }
            return false;
        }

        @Override
        public boolean mouseup(Coord c, int button) {
            if(dm != null) {
                dm.remove();
                dm = null;
                if(dragging) {
                    if(res != null) {
                        if(ui.dropthing(ui.root, ui.mc, res.get())) {
                            reset();
                            //delete anything that might already belong to this slot
                            ui.gui.wdgmsg("setbelt", slot, 1);
                        }
                    } else {
                        if(ui.dropthing(ui.root, ui.mc, pag)) {
                            reset();
                        }
                    }
                    dragging = false;
		} else if(button == 1 && c.isect(Coord.z, sz)) {
                    use();
                } else if(button == 3) {
                    ui.gui.wdgmsg("setbelt", slot, 1);
                    reset();
                }
                return true;
            }
            return false;
        }

        @Override
        public void mousemove(Coord c) {
            super.mousemove(c);
	    if(dm != null && !BeltWnd.this.locked) {
                dragging = true;
            }
        }

        @Override
        public boolean drop(Coord cc, Coord ul) {
            //This is for dropping inventory items on our mouse to a hotkey
            ui.gui.wdgmsg("setbelt", slot, 0);
            //reset for now and wait for server to send us uimsg if this was valid drop
            reset();
            return true;
        }

        @Override
        public boolean iteminteract(Coord cc, Coord ul) {
            return false;
        }

        @Override
        public boolean dropthing(Coord cc, Object thing) {
            //don't drop things on yourself..
            if(!dragging) {
                //Dropping "things" on us, mainly menugrid items
                if (thing instanceof Resource) {
                    //Normal server-side menu items
                    ui.gui.wdgmsg("setbelt", slot, ((Resource) thing).name);
                    //reset for now and wait for server to send us uimsg if this was valid drop
                    reset();
                    return true;
                } else if (thing instanceof MenuGrid.SpecialPagina) {
                    //Not normal stuff.
                    setPag((MenuGrid.SpecialPagina) thing);
                    //delete anything that might already belong to this slot
                    ui.gui.wdgmsg("setbelt", slot, 1);
                    return true;
                }
            }
            return false;
        }
    }

    public enum Style {
        HORIZONTAL, VERTICAL, GRID
    }

    //The actual belt..
    private final String name;
    private Style style;
    private boolean locked;

    private BeltBtn[] btns = new BeltBtn[10];
    private final IButton rotate, up, down, lock;
    private final BufferedImage on, off;

    //This is all about which slots we "own" and how we split it up between pages
    //each page is 10 slots
    private int start_slot;
    private int pagecount;
    //What page we're currently on
    public int page;

    //Belt data
    private final BeltData data;


    //TODO: belts should be IndirSettings
    private BeltWnd(final String name, final BeltData data) {
        super(name);
        this.name = name;
        this.data = data;
        style = Style.valueOf(DefSettings.global.get("belt."+name+".style", String.class));
        visible = DefSettings.global.get("belt."+name+".show", Boolean.class);
        page = DefSettings.global.get("belt."+name+".page", Integer.class);
        locked = DefSettings.global.get("belt."+name+".locked", Boolean.class);

        rotate = add(new IButton("custom/belt/default/rotate", "Rotate belt", () -> {
            switch (style) {
                case HORIZONTAL:
                    style = Style.VERTICAL;
                    break;
                case VERTICAL:
                    style = Style.GRID;
                    break;
                case GRID:
                    style = Style.HORIZONTAL;
                    break;
            }
            DefSettings.global.set("belt."+name+".style", style.toString());
            reposition();
        }));
        lock = add(new IButton("custom/belt/default/lock", "Lock belt", () -> {
            locked = !locked;
            DefSettings.global.set("belt."+name+".locked", locked);
            BeltWnd.this.lock.hover = locked ? BeltWnd.this.on : BeltWnd.this.off;
            BeltWnd.this.lock.up = locked ? BeltWnd.this.off : BeltWnd.this.on;
	}));
        on = lock.up;
        off = lock.hover;
        lock.hover = locked ? on : off;
        lock.up = locked ? off : on;

        up = add(new IButton("custom/belt/default/up", "Go up a page", () -> {
            page++;
            if(page >= pagecount)
                page = 0;
            upd_page();
        }));
        down = add(new IButton("custom/belt/default/down", "Go down a page", () -> {
            page--;
            if(page < 0)
                page = pagecount-1;
            upd_page();
        }));
    }

    public BeltWnd(final String name, final BeltData data, int sk, final int ek, final int pages, final int start) {
        this(name, data);
        final int[] keys = new int[ek-sk+1];
        int i = 0;
        //48 to 57 vk 0 to vk 9
        if(!name.equals("n")) {
            while (sk != ek + 1) {
                keys[i++] = sk++;
            }
        }
        else{//messy as fuck but it reverts back to ambers 1/2/3/4/5/6/7/8/9/0 system.
            keys[0] = 49;
            keys[1] = 50;
            keys[2] = 51;
            keys[3] = 52;
            keys[4] = 53;
            keys[5] = 54;
            keys[6] = 55;
            keys[7] = 56;
            keys[8] = 57;
            keys[9] = 48;
        }
        this.pagecount = pages;
        this.start_slot = start;
        makebtns(keys);
        pack();
    }

    @Override
    public void draw(GOut g) {
        super.draw(g);
    }

    @Override
    protected void added() {
        super.added();
        upd_page();
    }

    @Override
    protected boolean moveHit(Coord c, int btn) {
        if(btn == 1 && ui.modctrl) {
            for(BeltBtn bbtn : btns) {
                if(c.isect(bbtn.c, bbtn.sz))
                    return true;
            }
        }
        return false;
    }

    public void upd_page() {
        DefSettings.global.set("belt."+name+".page", page);
        int i = 0;
        for(BeltBtn btn : btns) {
            btn.setSlot(start_slot + i + (page * 10));
            i++;
        }
    }

    public void update(int slot) {
        int idx = (slot % 10);
        if(btns[idx].slot == slot)
            btns[idx].setSlot(slot);
    }

    private void makebtns(int [] keys) {
        int i = 0;
        for(int k : keys) {
            btns[i++] = add(new BeltBtn(k), new Coord(0,0));
        }
        reposition();
    }

    private void reposition_grid() {
        int x = 0 , y = 0;
        if(!this.name.equals("np")) {
            for (BeltBtn btn : btns) {
                btn.c = new Coord(x * (Inventory.invsq.sz().x + 2),
                        y * (Inventory.invsq.sz().y + 2));
                x++;
                if (x >= 3) {
                    x = 0;
                    y++;
                }
            }
        }else {
            for (BeltBtn btn : btns) {
                if(btn.key == 96)
                    btn.c = new Coord(0,108);
                if(btn.key == 97)
                    btn.c = new Coord(0,72);
                if(btn.key == 98)
                    btn.c = new Coord(36,72);
                if(btn.key == 99)
                    btn.c = new Coord(72,72);
                if(btn.key == 100)
                    btn.c = new Coord(0,36);
                if(btn.key == 101)
                    btn.c = new Coord(36,36);
                if(btn.key == 102)
                    btn.c = new Coord(72,36);
                if(btn.key == 103)
                    btn.c = new Coord(0,0);
                if(btn.key == 104)
                    btn.c = new Coord(36,0);
                if(btn.key == 105)
                    btn.c = new Coord(72,0);
            }
            x =  1;
            y = 3;
        }

        up.c = new Coord(x * (Inventory.invsq.sz().x + 2),
                y * (Inventory.invsq.sz().y + 2));
        down.c = new Coord(x * (Inventory.invsq.sz().x + 2) + up.sz.x + 2,
                y * (Inventory.invsq.sz().y + 2));
	rotate.c = up.c.add(0, up.sz.y + 2);
	lock.c = rotate.c.add(rotate.sz.x + 2, 0);
    }

    private void reposition() {
        if(style == Style.GRID)
            reposition_grid();
        else {
            int n = 0;
            for(BeltBtn btn : btns) {
                switch(style) {
                    case VERTICAL:
                        btn.c = new Coord(0, n);
                        n += Inventory.invsq.sz().y + 2;
                        break;
                    case HORIZONTAL:
                        btn.c = new Coord(n, 0);
                        n += Inventory.invsq.sz().x + 2;
                        break;
                }
            }
            switch(style) {
                case VERTICAL:
                    up.c = new Coord(0, n);
                    down.c = new Coord(up.sz.x + 2, n);
                    rotate.c = up.c.add(0, up.sz.y + 2);
		    lock.c = rotate.c.add(rotate.sz.x + 2, 2);
                    break;
                case HORIZONTAL:
                    up.c = new Coord(n, 0);
                    down.c = new Coord(n, up.sz.y + 2);
                    rotate.c = up.c.add(up.sz.x + 2, 0);
                    lock.c = rotate.c.add(0, rotate.sz.y + 2);
                    break;
            }
        }
        pack();
    }


    public void setVisibile(final boolean vis) {
        visible = vis;
        DefSettings.global.set("belt."+name+".show", visible);
    }
}
