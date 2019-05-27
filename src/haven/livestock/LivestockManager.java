package haven.livestock;

import java.util.*;
import java.util.stream.Collectors;
import haven.Button;

import haven.*;
import haven.Label;
import haven.Window;
import haven.Utils;
import haven.purus.pbot.PBotUtils;
import javafx.scene.layout.Pane;

import javax.xml.soap.Detail;
import java.awt.Color;

public class LivestockManager extends ResizableWnd {
    private final Panel cattle, horses, sheep, pigs, goats, ResetHighlights, HighlightMales, HighlightFemales, Inspect, Slaughter, DropEntrails, DropIntestines, DropMeat, DropBones;
    private Panel current;
    public static final int COLUMN_TITLE_X = 60;
    public static final int ENTRY_X = 20;
    private static final int WINDOW_WIDTH = 1150;
    public CheckBox DropEntrailsBox, DropIntestinesBox, DropMeatBox, DropBonesBox, InspectBox, SlaughterBox;
    public Animal pendingAnimal;
    private GameUI gui;
    public static int quality, breedquality, hide, meat, milk;
    public static boolean combined, combinedhide, combinedmeat, combinedmilk = false;
    private static final Tex texBull = Resource.loadtex("gfx/livestockava/bull");
    private static final Tex texBilly = Resource.loadtex("gfx/livestockava/billy");
    private static final Tex texRam = Resource.loadtex("gfx/livestockava/ram");
    private static final Tex texHog = Resource.loadtex("gfx/livestockava/hog");
    private static final Tex texStallion = Resource.loadtex("gfx/livestockava/stallion");
    private static final Tex texCow = Resource.loadtex("gfx/livestockava/cow");
    private static final Tex texNanny = Resource.loadtex("gfx/livestockava/nanny");
    private static final Tex texMare = Resource.loadtex("gfx/livestockava/mare");
    private static final Tex texSow = Resource.loadtex("gfx/livestockava/sow");
    private static final Tex texEwe = Resource.loadtex("gfx/livestockava/ewe");



    public LivestockManager() {

        super(Coord.z, "Livestock Manager");
        Coord pc = new Coord(20, 55);
        cattle = add(new Panel(), pc);
        horses = add(new Panel(), pc);
        sheep = add(new Panel(), pc);
        pigs = add(new Panel(), pc);
        goats = add(new Panel(), pc);
        ResetHighlights = add(new Panel(), pc);
        HighlightMales = add(new Panel(), pc);
        HighlightFemales = add(new Panel(), pc);
        Inspect = add(new Panel(), pc);
        Slaughter = add(new Panel(), pc);
        DropEntrails = add(new Panel(), pc);
        DropIntestines = add(new Panel(), pc);
        DropMeat = add(new Panel(), pc);
        DropBones = add(new Panel(), pc);



        add(new PButton(80, "Cattle", cattle), new Coord(20, 10));
        add(new PButton(80, "Horses", horses), new Coord(110, 10));
        add(new PButton(80, "Sheep", sheep), new Coord(200, 10));
        add(new PButton(80, "Pigs", pigs), new Coord(290, 10));
        add(new PButton(80, "Goats", goats), new Coord(380, 10));
        add(new PButton(80, "Reset Highlights", ResetHighlights), new Coord(470, 10));
        add(new PButton(80, "Highlight Males", HighlightMales), new Coord(780, 10));
        add(new PButton(80, "Highlight Females", HighlightFemales), new Coord(900, 10));
        add(new PButton(80, "Inspect", Inspect), new Coord(600, 10));
        add(new PButton(80, "Slaughter", Slaughter), new Coord(690, 10));
        add(new PButton(80, "Drop Entrails", DropEntrails), new Coord(20, 40));
        add(new PButton(80, "Drop Intestines", DropIntestines), new Coord(155, 40));
        add(new PButton(80, "Drop Meat", DropMeat), new Coord(300, 40));
        add(new PButton(80, "Drop Bones", DropBones), new Coord(420, 40));


        Utils.loadprefchklist("flowersel", Config.flowermenus);

         InspectBox = new CheckBox("") {
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
    add(InspectBox,640,40);

    SlaughterBox = new CheckBox("") {
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
    add(SlaughterBox,730,40);

        DropEntrailsBox = new CheckBox("") {
            {
              if(Config.DropEntrails)
                            a = true;
                         else
                            a = false;
            }

            public void set(boolean val) {
                Config.DropEntrails =val;
                        a = val;
                Utils.setprefb("DropEntrails", val);
                    }
        };
        add(DropEntrailsBox,120,45);

        DropIntestinesBox = new CheckBox("") {
            {
                if(Config.DropIntestines)
                    a = true;
                else
                    a = false;
            }

            public void set(boolean val) {
                Config.DropIntestines =val;
                a = val;
                Utils.setprefb("DropIntestines", val);
            }
        };
        add(DropIntestinesBox,270,45);

        DropMeatBox = new CheckBox("") {
            {
                if(Config.DropMeat)
                    a = true;
                else
                    a = false;
            }

            public void set(boolean val) {
                Config.DropMeat =val;
                a = val;
                Utils.setprefb("DropMeat", val);
            }
        };
        add(DropMeatBox,390,45);

        DropBonesBox = new CheckBox("") {
            {
                if(Config.DropBones)
                    a = true;
                else
                    a = false;
            }

            public void set(boolean val) {
                Config.DropBones =val;
                a = val;
                Utils.setprefb("DropBones", val);
            }
        };
        add(DropBonesBox,520,45);

        createHeader(horses, Horses.columns);
        createHeader(cattle, Cattle.columns);
        createHeader(sheep, Sheep.columns);
        createHeader(pigs, Pigs.columns);
        createHeader(goats, Goat.columns);


        chpanel(cattle);
        pack();
    }

    private void createHeader(Panel panel, Map<String, Column> columns) {
        List<Map.Entry<String, Column>> cols = columns.entrySet()
                .stream()
                .sorted(Comparator.comparingInt(c -> c.getValue().index))
                .collect(Collectors.toList());

        int offx = LivestockManager.COLUMN_TITLE_X - LivestockManager.ENTRY_X;

        for (Map.Entry<String, Column> col : cols) {
            Column pos = col.getValue();
            panel.add(pos.lbl, new Coord(pos.x + offx, 10));
        }

        panel.pack();
    }

    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (sender == cbtn) {
            Config.DropEntrails =false;
            Config.DropBones = false;
            Config.DropMeat = false;
            Config.DropIntestines = false;
            InspectBox.a = false;
            SlaughterBox.a = false;
            DropEntrailsBox.a = false;
            DropIntestinesBox.a = false;
            DropMeatBox.a = false;
            DropBonesBox.a = false;
            Utils.loadprefchklist("flowersel", Config.flowermenus);
            for(CheckListboxItem itm :Config.flowermenus.values()) {
                if (itm.name.equals("Inspect"))
                    itm.selected = false;
                if (itm.name.equals("Slaughter"))
                    itm.selected = false;
            }
            hide();
        }
        else
            super.wdgmsg(sender, msg, args);
    }

    @Override
    public void close(){
        Config.DropEntrails =false;
        Config.DropBones = false;
        Config.DropMeat = false;
        Config.DropIntestines = false;
        InspectBox.a = false;
        SlaughterBox.a = false;
        DropEntrailsBox.a = false;
        DropIntestinesBox.a = false;
        DropMeatBox.a = false;
        DropBonesBox.a = false;
        Utils.loadprefchklist("flowersel", Config.flowermenus);
        for(CheckListboxItem itm :Config.flowermenus.values()) {
            if (itm.name.equals("Inspect"))
                itm.selected = false;
            if (itm.name.equals("Slaughter"))
                itm.selected = false;
        }
        // visible = false;
        super.hide();
    }


    @Override
    public void resize(Coord sz) {
        super.resize(sz);
      //  this.sz = asz.sub(0);
    }

    @Override
    protected void added() {
        super.added();
      //  this.sz = asz;
    }

    @Override
    public boolean type(char key, java.awt.event.KeyEvent ev) {
        if (key == 27) {
            Config.DropEntrails =false;
            Config.DropBones = false;
            Config.DropMeat = false;
            Config.DropIntestines = false;
            InspectBox.a = false;
            SlaughterBox.a = false;
            DropEntrailsBox.a = false;
            DropIntestinesBox.a = false;
            DropMeatBox.a = false;
            DropBonesBox.a = false;
            Utils.loadprefchklist("flowersel", Config.flowermenus);
            for(CheckListboxItem itm :Config.flowermenus.values()) {
                if (itm.name.equals("Inspect"))
                    itm.selected = false;
                if (itm.name.equals("Slaughter"))
                    itm.selected = false;
            }
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
           // scrollPort = new Scrollport(new Coord(WINDOW_WIDTH - 40, 290)) {
                scrollPort = new Scrollport(new Coord(WINDOW_WIDTH - 40, 520), 25) {
                @Override
                public void draw(GOut g) {
                    g.chcolor(0, 0, 0, 128);
                    g.frect(Coord.z, sz);
                    g.chcolor();
                    super.draw(g);
                }
            };
           // add(scrollPort, new Coord(0, 25));
            add(scrollPort, new Coord(0, 22));
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
            else if (tgt == Inspect)
                PBotUtils.sysMsg("Not yet implemented.",Color.white);
            else if (tgt == Slaughter)
                PBotUtils.sysMsg("Not yet implemented.",Color.white);
            else if (tgt==DropEntrails){
                if (Config.DropEntrails) {
                    Config.DropEntrails = false;
                    DropEntrailsBox.a = false;
                    PBotUtils.sysMsg("No longer dropping entrails.",Color.white);
                }
                else
                    {
                    Config.DropEntrails =true;
                    DropEntrailsBox.a = true;
                        PBotUtils.sysMsg("Auto dropping Entrails.",Color.white);
                }
            }
            else if (tgt==DropIntestines){
                if (Config.DropIntestines) {
                    Config.DropIntestines = false;
                    PBotUtils.sysMsg("No longer dropping Intestines.",Color.white);
                    DropIntestinesBox.a = false;
                }
                else
                {
                    Config.DropIntestines =true;
                    DropIntestinesBox.a = true;
                    PBotUtils.sysMsg("Auto dropping Intestines.",Color.white);
                }
            }
            else if(tgt==DropMeat){
                if (Config.DropMeat) {
                    Config.DropMeat = false;
                    DropMeatBox.a = false;
                    PBotUtils.sysMsg("No longer dropping Meat.",Color.white);
                }
                else
                {
                    Config.DropMeat =true;
                    DropMeatBox.a = true;
                    PBotUtils.sysMsg("Auto dropping Meat.",Color.white);
                }
            }
            else if(tgt==DropBones){
                if (Config.DropBones) {
                    Config.DropBones = false;
                    DropBonesBox.a = false;
                    PBotUtils.sysMsg("No longer dropping Bones.",Color.white);
                }
                else
                {
                    Config.DropBones =true;
                    DropBonesBox.a = true;
                    PBotUtils.sysMsg("Auto dropping Bones.",Color.white);
                }
            }
            else if(tgt==HighlightFemales){
              //  Panel p = getAnimalPanel(wdg.type);
                for (Widget child = this.parent.lchild; child != null; child = child.prev) {
                    if (child instanceof Panel) {
                        System.out.println("Panel found");
                        if(child.visible) {
                            System.out.println("panel visible");
                            Scrollport.Scrollcont p = ((Panel) child).scrollPort.cont;
                            for (Widget child2 = p.lchild; child2 != null; child2 = child2.prev) {
                                if (child2 instanceof DetailsWdg) {
                                    if (((DetailsWdg) child2).animal.getAvatar().equals(texSow) || ((DetailsWdg) child2).animal.getAvatar().equals(texCow) || ((DetailsWdg) child2).animal.getAvatar().equals(texEwe)
                                            || ((DetailsWdg) child2).animal.getAvatar().equals(texMare) || ((DetailsWdg) child2).animal.getAvatar().equals(texNanny)) {
                                        child2.mousedown(Coord.z, 3);
                                    }
                                }
                            }
                        }
                    }
                }
                }
                else if(tgt==HighlightMales){
                for (Widget child = this.parent.lchild; child != null; child = child.prev) {
                    if (child instanceof Panel) {
                        System.out.println("Panel found");
                        if(child.visible) {
                            System.out.println("panel visible");
                            Scrollport.Scrollcont p = ((Panel) child).scrollPort.cont;
                            for (Widget child2 = p.lchild; child2 != null; child2 = child2.prev) {
                                if (child2 instanceof DetailsWdg) {
                                    if (((DetailsWdg) child2).animal.getAvatar().equals(texBilly) || ((DetailsWdg) child2).animal.getAvatar().equals(texBull) || ((DetailsWdg) child2).animal.getAvatar().equals(texHog)
                                            || ((DetailsWdg) child2).animal.getAvatar().equals(texRam) || ((DetailsWdg) child2).animal.getAvatar().equals(texStallion)) {
                                        child2.mousedown(Coord.z, 3);
                                    }
                                }
                            }
                        }
                    }
                }
            }
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
        if (name.equals (Resource.getLocString(Resource.BUNDLE_LABEL, "Quality:")))
            quality = val;
        if (name.equals (Resource.getLocString(Resource.BUNDLE_LABEL, "Breeding quality:")))
            breedquality = val;
        if (name.equals (Resource.getLocString(Resource.BUNDLE_LABEL, "Meat quality:")))
            meat = val;
        if (name.equals (Resource.getLocString(Resource.BUNDLE_LABEL, "Hide quality:")))
            hide = val;
        if (name.equals (Resource.getLocString(Resource.BUNDLE_LABEL, "Milk quality:")))
            milk = val;
        if(meat > 0 && quality > 0 && !combinedmeat){
            pendingAnimal.put("Meat quality2:", quality * meat / 100);
            pendingAnimal.attributeResolved();
            combinedmeat = true;
        }
        if(milk > 0 && quality > 0 && !combinedmilk){
            pendingAnimal.put("Milk quality2:", quality * milk / 100);
            pendingAnimal.attributeResolved();
            combinedmilk = true;
        }
        if(hide > 0 && quality > 0 && !combinedhide){
            pendingAnimal.put("Hide quality2:", quality * hide / 100);
            pendingAnimal.attributeResolved();
            combinedhide = true;
        }
        if (quality > 0 && breedquality > 0 && !combined) {
            pendingAnimal.put("Combined quality:", quality + breedquality);
            pendingAnimal.attributeResolved();
            combined = true;
        }
        pendingAnimal.put(name, val);
        pendingAnimal.attributeResolved();




        if (pendingAnimal.hasAllAttributes()) {
            combined = false;
            combinedmeat = false;
            combinedmilk = false;
            combinedhide = false;
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
            //p.scrollPort.cont.add(details, new Coord(0, y));
          //  details.sz = new Coord(p.scrollPort.cont.sz.x, DetailsWdg.HEIGHT);
            Scrollport.Scrollcont scrollCont = p.scrollPort.cont;
            scrollCont.add(details, new Coord(0, y));
            p.scrollPort.bar.max = Math.max(0, scrollCont.contentsz().y - scrollCont.sz.y + DetailsWdg.HEIGHT);
            details.sz = new Coord(scrollCont.sz.x, DetailsWdg.HEIGHT);

            chpanel(p);
            pendingAnimal = null;
            quality = 0;
            breedquality = 0;
            hide = 0;
            milk = 0;
            meat = 0;
        }
    }
}
