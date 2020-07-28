package haven;

import java.awt.Color;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

public class CustomQualityList extends WidgetList<CustomQualityList.Item> {
    public static ArrayList<ColorQuality> qualityList;
    public static Color NewColor = Color.WHITE;

    public static final Comparator<Item> ITEM_COMPARATOR = new Comparator<Item>() {
        @Override
        public int compare(Item o1, Item o2) {
            return Double.compare(o1.staticNumber, o2.staticNumber);
        }
    };
    public static final Comparator<ColorQuality> QUALITY_COMPARATOR = new Comparator<ColorQuality>() {
        @Override
        public int compare(ColorQuality o1, ColorQuality o2) {
            return Double.compare(o1.number, o2.number);
        }
    };

    public CustomQualityList() {
        super(new Coord(180, 25), 10);
        init();

        update();
    }

    @SuppressWarnings("SynchronizeOnNonFinalField")
    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        switch (msg) {
            case "changed": {
                double oldNumber = (double) args[0];
                double number = (double) args[1];
                Color color = (Color) args[2];
                boolean a = (boolean) args[3];
                synchronized (qualityList) {
                    if (contains(oldNumber)) {
                        get(oldNumber).number = number;
                        get(number).color = color;
                        get(number).a = a;
                    } else
                        qualityList.add(new ColorQuality(number, color, a));
                }
                upsert(oldNumber, number, color, a);
                update();
                break;
            }
            case "delete": {
                double number = (double) args[0];
                synchronized (qualityList) {
                    qualityList.remove(get(number));
                }
                remove(number);
                removeitem((Item) sender, true);
                update();
                break;
            }
            default:
                super.wdgmsg(sender, msg, args);
                break;
        }
    }

    @SuppressWarnings("SynchronizeOnNonFinalField")
    public void add(double oldNumber, double number, Color color, boolean a) {
        if (number != 0 && !contains(number)) {
            synchronized (qualityList) {
                qualityList.add(new ColorQuality(number, color, a));
            }
            upsert(oldNumber, number, color, a);
            additem(new Item(number, color, a));
            update();
        }
    }
    
    public boolean contains(double number) {
        for (ColorQuality i : qualityList)
            if (i.number == number) return true;

        return false;
    }

    public ColorQuality get(double number) {
        for (ColorQuality i : qualityList)
            if (i.number == number) return i;

        return null;
    }

    private void update() {
        Collections.sort(list, ITEM_COMPARATOR);
        qualityList.sort(QUALITY_COMPARATOR);
        int n = listitems();
        for (int i = 0; i < n; i++) {
            listitem(i).c = itempos(i);
        }
    }

    public void init() {
        Storage.dynamic.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS color_quality ( number DOUBLE PRIMARY KEY , color INT , a BOOLEAN )");
            }
        });
        Storage.dynamic.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                try (final ResultSet res = stmt.executeQuery("SELECT * FROM color_quality")) {
                    while (res.next()) {
                        double number;
                        Color color;
                        boolean a;
                        number = res.getDouble(1);
                        color = new Color(res.getInt(2), true);
                        a = res.getBoolean(3);
                        if (qualityList == null) qualityList = new ArrayList<>();
                        qualityList.add(new ColorQuality(number, color, a));
                        additem(new CustomQualityList.Item(number, color, a));
;                    }
                }
            }
        });
        if (qualityList == null || qualityList.isEmpty()) {
            qualityList = new ArrayList<ColorQuality>() {{
                add(new ColorQuality(10, Color.WHITE, true));
                add(new ColorQuality(Config.uncommonq, Config.uncommon, true));
                add(new ColorQuality(Config.rareq, Config.rare, true));
                add(new ColorQuality(Config.epicq, Config.epic, true));
                add(new ColorQuality(Config.legendaryq, Config.legendary, true));
            }};
            upsert(10, 10, Color.WHITE, true);
            upsert(Config.uncommonq, Config.uncommonq, Config.uncommon, true);
            upsert(Config.rareq, Config.rareq, Config.rare, true);
            upsert(Config.epicq, Config.epicq, Config.epic, true);
            upsert(Config.legendaryq, Config.legendaryq, Config.legendary, true);
        }
    }

    public void upsert(double oldNumber, double number, Color color, boolean a) {
        final boolean[] sqlExist = {false};
        Storage.dynamic.ensure(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("SELECT * FROM color_quality WHERE number = ?");
            stmt.setDouble(1, oldNumber);
            try (final ResultSet res = stmt.executeQuery()) {
                while (res.next()) {
                    sqlExist[0] = true;
                }
            }
        });
        if (sqlExist[0]) {
            Storage.dynamic.write(sql -> {
                final PreparedStatement stmt = Storage.dynamic.prepare("UPDATE color_quality SET number = ?, color = ? , a = ? WHERE number = ?");
                stmt.setDouble(1, number);
                stmt.setInt(2, color.hashCode());
                stmt.setBoolean(3, a);
                stmt.setDouble(4, oldNumber);
                stmt.executeUpdate();
            });
        }
        else {
            Storage.dynamic.write(sql -> {
                final PreparedStatement stmt = Storage.dynamic.prepare("INSERT INTO color_quality VALUES (? , ? , ?)");
                stmt.setDouble(1, number);
                stmt.setInt(2, color.hashCode());
                stmt.setBoolean(3, a);
                stmt.executeUpdate();
            });
        }
    }

    public void remove(double number) {
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("DELETE FROM color_quality WHERE number = ?");
            stmt.setDouble(1, number);
            stmt.executeUpdate();
        });
    }

    public static class ColorQuality {
        public double number;
        public Color color;
        public boolean a;

        public ColorQuality(double number, Color color, boolean a) {
            this.number = number;
            this.color = color;
            this.a = a;
        }
    }

    protected static class Item extends Widget {

        public double oldNumber, staticNumber;
        public Color staticColor;
        public boolean staticA;
        private final CheckBox cb;
        private final TextEntry te;
        private final ColorPreview colorPreview;
        private boolean a = false;
        private UI.Grab grab;

        public Item(double number, Color color, boolean a) {
            super(new Coord(180, 25));
            oldNumber = staticNumber = number;
            staticColor = color;
            staticA = a;

            cb = add(new CheckBox("") {
                {
                    a = staticA;
                    canactivate = true;
                }
                public void set(boolean val) {
                    staticA = val;
                    a = staticA;
                    oldNumber = staticNumber;
                    super.wdgmsg("ch");
                }
            }, 3, 3);

            te = add(new TextEntry(100, staticNumber + "") {
                @Override
                public void activate(String text) {
                    try {
                        oldNumber = staticNumber;
                        staticNumber = Double.parseDouble(text);
                        super.wdgmsg("ch");
                    } catch (Exception e) {
                        e.printStackTrace();
                        settext(staticNumber + "");
                    }
                }
            }, 25, 1);

            colorPreview = add(new ColorPreview(new Coord(17, 17), color, val -> {
                oldNumber = staticNumber;
                staticColor = val;
                super.wdgmsg("ch");
            }), 130, 4);

            add(new Button(24, "X") {
                @Override
                public void click() {
                    super.wdgmsg("activate", oldNumber);
                }

                @Override
                public boolean mouseup(Coord c, int button) {
                    //FIXME:a little hack, because WidgetList does not pass correct click coordinates if scrolled
                    return super.mouseup(Coord.z, button);
                }
            }, 160, 0);
        }

        /**@Override
        public boolean mousedown(Coord c, int button) {
            if (super.mousedown(c, button)) {
                return true;
            }
            if (button != 1)
                return (false);
            a = true;
            grab = ui.grabmouse(this);
            return (true);
        }

        @Override
        public boolean mouseup(Coord c, int button) {
            if (a && button == 1) {
                a = false;
                if (grab != null) {
                    grab.remove();
                    grab = null;
                }
                if (c.isect(new Coord(0, 0), sz))
                    click();
                return (true);
            }
            return (false);
        }**/

        /*private void click() {
            cb.a = !cb.a;
            wdgmsg("changed", staticNumber, staticColor, staticA);
        }*/

        @Override
        public void wdgmsg(Widget sender, String msg, Object... args) {
            switch (msg) {
                case "ch":
                    wdgmsg("changed", oldNumber, staticNumber, staticColor, staticA);
                    break;
                case "activate":
                    wdgmsg("delete", oldNumber);
                    break;
                default:
                    super.wdgmsg(sender, msg, args);
                    break;
            }
        }
    }
}
