package haven.livestock;

import java.util.*;
import java.util.stream.Collectors;
import haven.Button;

import haven.*;
import haven.Label;
import haven.Window;
import haven.Utils;
import haven.purus.BotUtils;
import java.awt.Color;

public class LivestockManager extends Window {
    private final Panel cattle, horses, sheep, pigs, goats, ResetHighlights, Inspect, Slaughter;
    private Panel current;
    public static final int COLUMN_TITLE_X = 60;
    public static final int ENTRY_X = 20;
    private static final int WINDOW_WIDTH = 790;
    public Animal pendingAnimal;
    private GameUI gui;
    public static int quality, breedquality;
    public static boolean combined = false;



    public LivestockManager() {

        super(new Coord(WINDOW_WIDTH, 380), "Livestock Manager");
        Coord pc = new Coord(20, 55);
        cattle = add(new Panel(), pc);
        horses = add(new Panel(), pc);
        sheep = add(new Panel(), pc);
        pigs = add(new Panel(), pc);
        goats = add(new Panel(), pc);
        ResetHighlights = add(new Panel(), pc);
        Inspect = add(new Panel(), pc);
        Slaughter = add(new Panel(), pc);


        add(new PButton(80, "Cattle", cattle), new Coord(20, 10));
        add(new PButton(80, "Horses", horses), new Coord(110, 10));
        add(new PButton(80, "Sheep", sheep), new Coord(200, 10));
        add(new PButton(80, "Pigs", pigs), new Coord(290, 10));
        add(new PButton(80, "Goats", goats), new Coord(380, 10));
        add(new PButton(80, "Reset Highlights", ResetHighlights), new Coord(470, 10));
        add(new PButton(80, "Inspect", Inspect), new Coord(600, 10));
        add(new PButton(80, "Slaughter", Slaughter), new Coord(690, 10));

        Utils.loadprefchklist("flowersel", Config.flowermenus);

        CheckBox Inspectbox = new CheckBox("") {
            {
                for(CheckListboxItem itm :Config.flowermenus.values()) {
                    if (itm.name.equals("Inspect")) {
                        if (itm.selected) {
                            a = true;
                        } else {
                            a = false;
                        }
                    }
                }
        }

            public void set(boolean val) {
                for(CheckListboxItem itm :Config.flowermenus.values()) {
                    if (itm.name.equals("Inspect")) {
                        itm.selected =val;
                        a = val;
                        Utils.setprefchklst("flowersel", Config.flowermenus);
                    }
                }
            }


        };


    add(Inspectbox,640,40);

        CheckBox Slaughterbox = new CheckBox("") {
            {
                for(CheckListboxItem itm :Config.flowermenus.values()) {
                    if (itm.name.equals("Slaughter")) {
                        if (itm.selected) {
                            a = true;
                        } else {
                            a = false;
                        }
                    }
                }
            }

            public void set(boolean val) {
                for(CheckListboxItem itm :Config.flowermenus.values()) {
                    if (itm.name.equals("Slaughter")) {
                        itm.selected =val;
                        a = val;
                        Utils.setprefchklst("flowersel", Config.flowermenus);
                    }
                }
            }
        };

    add(Slaughterbox,730,40);




        createHeader(horses, Horses.columns);
        createHeader(cattle, Cattle.columns);
        createHeader(sheep, Sheep.columns);
        createHeader(pigs, Pigs.columns);
        createHeader(goats, Goat.columns);


        chpanel(cattle);
    }






    private void createHeader(Panel panel, Map<String, Column> columns) {
        List<Map.Entry<String, Column>> cols = columns.entrySet()
                .stream()
                .sorted(Comparator.comparingInt(c -> c.getValue().index))
                .collect(Collectors.toList());

        int offx = LivestockManager.COLUMN_TITLE_X - LivestockManager.ENTRY_X;

        for (Map.Entry<String, Column> col : cols) {
            Column pos = col.getValue();
            panel.add(pos.lbl, new Coord(pos.x + offx, 0));
        }

        panel.pack();
    }

    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (sender == cbtn)
            hide();
        else
            super.wdgmsg(sender, msg, args);
    }

    @Override
    public boolean type(char key, java.awt.event.KeyEvent ev) {
        if (key == 27) {
            hide();
            return true;
        }
        return super.type(key, ev);
    }

    public void chpanel(Panel p) {
        if (current != null)
            current.hide();
        (current = p).show();
    }

    public class Panel extends Widget {
        public final List<Animal> list = new ArrayList<>();

        public Scrollport scrollPort;

        public Panel() {
            visible = false;
            c = Coord.z;
            scrollPort = new Scrollport(new Coord(WINDOW_WIDTH - 40, 290)) {
                @Override
                public void draw(GOut g) {
                    g.chcolor(0, 0, 0, 128);
                    g.frect(Coord.z, sz);
                    g.chcolor();
                    super.draw(g);
                }
            };
            add(scrollPort, new Coord(0, 25));
        }

        public void delete(Animal animal) {
            for (Iterator<Animal> iterator = list.iterator(); iterator.hasNext(); ) {
                if (iterator.next() == animal) {
                    iterator.remove();
                    break;
                }
            }
        }
    }


    private class PButton extends Button {
        public final Panel tgt;

        public PButton(int w, String title, Panel tgt) {


            super(w, title);
            this.tgt = tgt;

        }

        @Override
        public void click() {

            if (tgt == ResetHighlights)
                MapView.markedGobs.clear();
                else chpanel(tgt);

            }



    }


    public void initPendingAnimal(long wdgid, String type) {
       switch (type) {
            case "Bull":
            case "Cow":
                pendingAnimal = new Cattle(wdgid, type);
                break;
            case "Mare":
            case "Stallion":
                pendingAnimal = new Horses(wdgid, type);
                break;
            case "Ram":
            case "Ewe":
                pendingAnimal = new Sheep(wdgid, type);
                break;
            case "Hog":
            case "Sow":
                pendingAnimal = new Pigs(wdgid, type);
                break;
           case "Nanny":
           case "Billy":
               pendingAnimal = new Goat(wdgid, type);
               break;
        }
    }

    public Panel getAnimalPanel(String type) {
        switch (type) {
            case "Bull":
            case "Cow":
                return cattle;
            case "Mare":
            case "Stallion":
                return horses;
            case "Ram":
            case "Ewe":
                return sheep;
            case "Hog":
            case "Sow":
                return pigs;
            case "Nanny":
            case "Billy":
                return goats;
            default:
                return null;
        }
    }

    public void applyId(Widget wdg) {
        if (pendingAnimal != null && !pendingAnimal.hasAllAttributes()) {
            pendingAnimal.gobid = ((Avaview) wdg).avagob;
            pendingAnimal.attributeResolved();
        }
    }

    public void applyName(Widget wdg) {
        if (pendingAnimal != null && !pendingAnimal.hasAllAttributes()) {
            pendingAnimal.name = ((TextEntry) wdg).text;
            pendingAnimal.attributeResolved();
        }
    }

    public void applyAttr(String type, Widget wdg) {
        if (!(wdg.prev instanceof Label))
            return;

        String name = ((Label) wdg.prev).texts;

        if (pendingAnimal == null || !pendingAnimal.containsKey(name) || pendingAnimal.hasAllAttributes())
            return;

        String valStr = ((Label) wdg).texts;
        if (valStr.endsWith("%"))
            valStr = valStr.substring(0, valStr.length() - 1);
        Integer val = new Integer(valStr);
        if (name.equals("Quality:"))
        quality = val;
        if (name.equals("Breeding quality:"))
            breedquality = val;
        if (quality > 0 && breedquality > 0 && !combined) {
            pendingAnimal.put("Combined quality:", quality + breedquality);
            pendingAnimal.attributeResolved();
            combined = true;
        }
        pendingAnimal.put(name, val);
        pendingAnimal.attributeResolved();





        if (pendingAnimal.hasAllAttributes()) {
            combined = false;
            Panel p = getAnimalPanel(type);

            if (p.list.stream().anyMatch(x -> x.gobid == pendingAnimal.gobid)) {
                pendingAnimal = null;
                return;
            }

            p.list.add(pendingAnimal);

            int y = 0;
            for (Widget child = p.scrollPort.cont.lchild; child != null; child = child.prev) {
                if (child instanceof DetailsWdg && child.c.y >= y)
                    y = child.c.y + DetailsWdg.HEIGHT;
            }

            DetailsWdg details = new DetailsWdg(pendingAnimal);
            p.scrollPort.cont.add(details, new Coord(0, y));
            details.sz = new Coord(p.scrollPort.cont.sz.x, DetailsWdg.HEIGHT);

            chpanel(p);
            pendingAnimal = null;
            quality = 0;
            breedquality = 0;
        }
    }
}
