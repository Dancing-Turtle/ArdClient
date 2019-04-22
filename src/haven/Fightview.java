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



import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

public class Fightview extends Widget {
    static Tex bg = Theme.tex("bosq");
    static int height = 5;
    static int ymarg = 5;
    static int width = 165;
    static int oldblue, lastactopening, unarmed;
    private int damage = 0;
    static Coord avasz = new Coord(27, 27);
    static Coord cavac = new Coord(width - Avaview.dasz.x - 10, 10);
    static Coord cgivec = new Coord(cavac.x - 35, cavac.y);
    static Coord cpursc = new Coord(cavac.x - 75, cgivec.y + 35);
    public LinkedList<Relation> lsrel = new LinkedList<Relation>();
    public Relation current = null;
    public static String ttretaincur, ttretain;
    public List <Relation> notcurrent;
    public Indir<Resource> blk, batk, iatk;
    public double atkcs, atkct;
    public Indir<Resource> lastact = null;
    public double lastuse = 0;
    public double atkcd;
    public GiveButton curgive;
    private Avaview curava;
    private Button curpurs;
    public final Bufflist buffs = add(new Bufflist());
    private static final Gob.Overlay curol = new Gob.Overlay(new FightCurrentOpp());
    {
        buffs.hide();
    }
    private static final Color combatLogMeClr = new Color(86, 153, 191);
    private static final Color combatLogOpClr = new Color(234, 105, 105);

    public class Relation {
        public final long gobid;
        public final Avaview ava;
        public final GiveButton give;
        public final Button purs;
        public final Bufflist buffs = add(new Bufflist());
        {
            buffs.hide();
        }
        public int ip, oip;
        public Indir<Resource> lastact = null;
        public double lastuse = 0;

        public Relation(long gobid) {
            this.gobid = gobid;
            add(this.ava = new Avaview(avasz, gobid, "avacam")).canactivate = true;
            add(this.give = new GiveButton(0, new Coord(15, 15)));
            add(this.purs = new Button(70, "Chase"));
        }

        public void give(int state) {
            if (this == current)
                curgive.state = state;
             this.give.state = state;
        }

        public void show(boolean state) {
            ava.show(state);
            give.show(state);
            purs.show(state);
        }

        public void remove() {
            ui.destroy(ava);
            ui.destroy(give);
            ui.destroy(purs);
        }

        public void use(Indir<Resource> act) {
            try {
            lastact = act;
            lastuse = Utils.rtime();
            if(lastact != null)
            if(lastact.get().basename().contains("cleave") && Config.cleavesound) {
                try {
                    Audio.play(Resource.local().loadwait(Config.cleavesfx), Config.cleavesoundvol);
                }catch(Exception e){}//ignore because a crash here would prob get someone killed
            }
            if (lastact != null && Config.logcombatactions) {
                    Resource res = lastact.get();
                    Resource.Tooltip tt = res.layer(Resource.tooltip);
                    ttretaincur = tt.t;
                    if (tt == null) {
                        gameui().syslog.append("Combat: WARNING! tooltip is missing for " + res.name + ". Notify Jorb/Loftar about this.", combatLogOpClr);
                        return;
                    }
                    OCache oc = gameui().map.glob.oc;
                    Gob gob = oc.getgob(gobid);
                    KinInfo kininfo = gob.getattr(KinInfo.class);
                    if(kininfo != null)
                    gameui().syslog.append(String.format("%s, %s, ip %d - %d", kininfo.name, tt.t, ip, oip), combatLogOpClr);
                    else
                        if(gob.getres().basename().contains("body"))
                        gameui().syslog.append(String.format("Enemy Player, %s, ip %d - %d", tt.t, ip, oip), combatLogOpClr);
                    else
                            gameui().syslog.append(String.format("Enemy :%s, %s, ip %d - %d",gob.getres().basename(), tt.t, ip, oip), combatLogOpClr);
                }
            } catch (Loading | NullPointerException l) { }
        }
    }
    public void use(Indir<Resource> act) {
        lastact = act;
        lastuse = Utils.rtime();
        if(lastact != null)
            if(lastact.get().basename().contains("cleave") && Config.cleavesound) {
            try{
                Audio.play(Resource.local().loadwait(Config.cleavesfx), Config.cleavesoundvol);
            }catch(Exception e){}//ignore because a crash here would prob get someone killed
            }
        if (lastact != null && Config.logcombatactions) {
            try {
                Resource res = lastact.get();
                Resource.Tooltip tt = res.layer(Resource.tooltip);
                ttretain = tt.t;
                if (tt == null) {
                    gameui().syslog.append("Combat: WARNING! tooltip is missing for " + res.name + ". Notify Jorb/Loftar about this.", combatLogMeClr);
                    return;
                }
                String cd = Utils.fmt1DecPlace(atkct - lastuse);
                double cdadvantage = checkcd(cd,tt);
                unarmed = getUnarmed();
                if(cdadvantage != 0.0)
                    gameui().syslog.append(String.format("me: %s, ip %d - %d, cd %ss, Agi Delta %s", tt.t, current.ip, current.oip, cd,cdadvantage), combatLogMeClr);
                else
                    gameui().syslog.append(String.format("me: %s, ip %d - %d, cd %ss", tt.t, current.ip, current.oip, cd), combatLogMeClr);
            } catch (Loading l) {
            }
        }
    }
    private int getUnarmed(){
        CharWnd chrwdg = null;
        int unarmedcombat = 0;
        try {
            chrwdg = ((GameUI) parent.parent).chrwdg;
        } catch (Exception e) { // fail silently
        }
        if(chrwdg != null){
            for (CharWnd.SAttr attr : chrwdg.skill) {
                if(attr.attr.nm.contains("unarmed")){
                unarmedcombat = attr.attr.comp;
                }
            }
        }
        return unarmedcombat;
    }
    private Double checkcd(String cd, Resource.Tooltip tt){
        double base;
        if(tt.t.contains("Flex"))
        base = 1.8;
        else
            if(tt.t.contains("Knocks"))
                base = 2.7;
        else
            if(tt.t.contains("Teeth"))
                base = 2.1;
        else
            return 0.0;

        double converted = Double.parseDouble(cd);
        double finalcd = converted - base;
        finalcd = finalcd / base * 100;
        if(finalcd < -16.0)
            finalcd = 100;
        else
        if(finalcd > -16.0 && finalcd <= -10.0)
            finalcd = 75;
        else
        if(finalcd > -10.0 && finalcd <= -6.0)
            finalcd = 50;
        else
        if(finalcd > -6.0 && finalcd <= -4.0)
            finalcd = 25;
        else
        if(finalcd > -1.0 && finalcd <= 1.0)
            finalcd = 0;
        else
        if(finalcd > 16.0)
            finalcd = -100;
        else
        if(finalcd < 16.0 && finalcd >= 10.0)
            finalcd = -75;
        else
        if(finalcd < 10.0 && finalcd >= 6.0)
            finalcd = -50;
        else
        if(finalcd < 6.0 && finalcd >= 4.0)
            finalcd = -25;

        return finalcd;
    }

    @RName("frv")
    public static class $_ implements Factory {
        public Widget create(UI ui, Object[] args) {
            return (new Fightview());
        }
    }

    public Fightview() {
        super(new Coord(width, (bg.sz().y + ymarg) * height));
    }

    public void addchild(Widget child, Object... args) {
        if (args[0].equals("buff")) {
            Widget p;
            if (args[1] == null)
                p = buffs;
            else
                p = getrel((Integer) args[1]).buffs;
            p.addchild(child);
        } else {
            super.addchild(child, args);
        }
    }

    private void setcur(Relation rel) {
        if ((current == null) && (rel != null)) {
            add(curgive = new GiveButton(0), cgivec);
            add(curava = new Avaview(Avaview.dasz, rel.gobid, "avacam"), cavac).canactivate = true;
            add(curpurs = new Button(70, "Chase"), cpursc);
            curgive.state = rel.give.state;
        } else if ((current != null) && (rel == null)) {
            ui.destroy(curgive);
            ui.destroy(curava);
            ui.destroy(curpurs);
            curgive = null;
            curava = null;
            curpurs = null;
        } else if ((current != null) && (rel != null)) {
            curgive.state = rel.give.state;
            curava.avagob = rel.gobid;
        }
        current = rel;

        if (Config.hlightcuropp) {
            if (current != null) {
                Gob curgob = ui.sess.glob.oc.getgob(current.gobid);
                if (curgob != null && !curgob.ols.contains(curol))
                    curgob.ols.add(curol);
            }
            for (Relation r : lsrel) {
                Gob relgob = ui.sess.glob.oc.getgob(r.gobid);
                if (relgob != null && r != rel)
                    relgob.ols.remove(curol);
            }
        }
    }

    public void destroy() {
        setcur(null);
        super.destroy();
    }

    public void draw(GOut g) {
        int y = 10;
        if (curava != null)
            y = curava.c.y + curava.sz.y + 10;
        int x = width - bg.sz().x - 10;
        for (Relation rel : lsrel) {
            if (rel == current) {
                rel.show(false);
                continue;
           }
            g.image(bg, new Coord(x, y));
            rel.ava.c = new Coord(x + 25, ((bg.sz().y - rel.ava.sz.y) / 2) + y);
            rel.give.c = new Coord(x + 5, 4 + y);
            rel.purs.c = new Coord(rel.ava.c.x + rel.ava.sz.x + 5, 4 + y);

            rel.show(true);
            y += bg.sz().y + ymarg;
        }
        super.draw(g);
    }


    public static class Notfound extends RuntimeException {
        public final long id;

        public Notfound(long id) {
            super("No relation for Gob ID " + id + " found");
            this.id = id;
        }
    }

    private Relation getrel(long gobid) {
        for (Relation rel : lsrel) {
            if (rel.gobid == gobid)
                return (rel);
        }
        throw (new Notfound(gobid));
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (sender == curava) {
            wdgmsg("click", (int) current.gobid, args[0]);
            return;
        } else if (sender == curgive) {
            wdgmsg("give", (int) current.gobid, args[0]);
            return;
        } else if (sender == curpurs) {
            wdgmsg("prs", (int) current.gobid);
            return;
        }
        for (Relation rel : lsrel) {
            if (sender == rel.ava) {
                wdgmsg("click", (int) rel.gobid, args[0]);
                return;
            } else if (sender == rel.give) {
                wdgmsg("give", (int) rel.gobid, args[0]);
                return;
            } else if (sender == rel.purs) {
                wdgmsg("prs", (int) rel.gobid);
                return;
            }
        }
        super.wdgmsg(sender, msg, args);
    }

    private Indir<Resource> n2r(int num) {
        if (num < 0)
            return (null);
        return (ui.sess.getres(num));
    }

    public void uimsg(String msg, Object... args) {
        if (msg == "new") {
            Relation rel = new Relation((Integer) args[0]);
            rel.give((Integer) args[1]);
            rel.ip = (Integer) args[2];
            rel.oip = (Integer) args[3];
            lsrel.addFirst(rel);
            ui.sess.glob.oc.isfight = true;
            return;
        } else if (msg == "del") {
            Relation rel = getrel((Integer) args[0]);
            OCache oc = ui.sess.glob.oc;
            oc.removedmgoverlay(rel.gobid);
            if (Config.hlightcuropp) {
                Gob relgob = ui.sess.glob.oc.getgob(rel.gobid);
                if (relgob != null)
                    relgob.ols.remove(curol);
            }
            rel.remove();
            lsrel.remove(rel);
            if (lsrel.size() == 0) {
                oc.removedmgoverlay(MapView.plgob);
                oc.isfight = false;
            }
            if (rel == current)
                setcur(null);
            return;
        } else if (msg == "upd") {
            Relation rel = getrel((Integer) args[0]);
            rel.give((Integer) args[1]);
            rel.ip = (Integer) args[2];
            rel.oip = (Integer) args[3];
            if (rel != current)
            return;
        } else if(msg == "used") {
            use((args[0] == null)?null:ui.sess.getres((Integer)args[0]));
            return;
        } else if(msg == "ruse") {
            Relation rel = getrel((Integer)args[0]);
            rel.use((args[1] == null)?null:ui.sess.getres((Integer)args[1]));
            return;
        } else if (msg == "cur") {
            try {
                Relation rel = getrel((Integer) args[0]);
                lsrel.remove(rel);
                lsrel.addFirst(rel);
                setcur(rel);
            } catch (Notfound e) {
                setcur(null);
            }
            return;
        } else if (msg == "atkc") {
            atkcd = ((Number)args[0]).doubleValue();
            atkcs = Utils.rtime();
            atkct = atkcs + (atkcd * 0.06);
            return;
        } else if (msg == "blk") {
            blk = n2r((Integer) args[0]);
            return;
        } else if (msg == "atk") {
            batk = n2r((Integer) args[0]);
            iatk = n2r((Integer) args[1]);
            return;
        }
        super.uimsg(msg, args);
    }
}
