package haven.overlays;

import haven.Storage;
import haven.sloth.util.ObservableCollection;
import haven.sloth.util.ObservableListener;

import java.awt.Color;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashSet;

public class OverlayData {
    private static ObservableCollection<OverlayGob> overlayed = new ObservableCollection<>(new HashSet<>()); //ObservableMap<boolean, OverlayGob>

    public static void init() {
        Storage.overlays.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS gob_text ( name TEXT PRIMARY KEY , text TEXT , tcolor INT , scolor INT , fontSize INT , font TEXT )");
            }
        });
        Storage.overlays.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS gob_highlight ( name TEXT PRIMARY KEY , hcolor INT )");
            }
        });
        Storage.overlays.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                try (final ResultSet res = stmt.executeQuery("SELECT * FROM gob_text")) {
                    while (res.next()) {
                        String name, text, font;
                        Color textColor, strokeColor;
                        int fontSize;
                        name = res.getString(1);
                        text = res.getString(2);
                        textColor = new Color(res.getInt(3), true);
                        strokeColor = new Color(res.getInt(4), true);
                        fontSize = res.getInt(5);
                        font = res.getString(6);
                        addText(name, text, textColor, strokeColor, fontSize, font, false);
                    }
                }
            }
        });
        Storage.overlays.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                try (final ResultSet res = stmt.executeQuery("SELECT * FROM gob_highlight")) {
                    while (res.next()) {
                        String name;
                        Color highlightColor;
                        name = res.getString(1);
                        highlightColor = new Color(res.getInt(2), true);
                        addHighlight(name, highlightColor, false);
                    }
                }
            }
        });
    }

    public synchronized static void listen(final ObservableListener<OverlayGob> listener) {
        overlayed.addListener(listener);
    }

    public synchronized static void unlisten(final ObservableListener<OverlayGob> listener) {
        overlayed.removeListener(listener);
    }

    public synchronized static void addText(String name, String text, Color textColor, Color strokeColor, int fontSize, String font, boolean withsql) {
        boolean isExist = false;
        for (OverlayGob og : overlayed) {
            if (og.name.equals(name)) {
                isExist = true;
                og.add(text, textColor, strokeColor, fontSize, font);
                break;
            } else isExist = false;
        }
        if (!isExist) overlayed.add(new OverlayGob(name, text, textColor, strokeColor, fontSize, font));
        if (withsql) add(name, text, textColor, strokeColor, fontSize, font);
    }

    public synchronized static void addHighlight(String name, Color highlightColor, boolean withsql) {
        boolean isExist = false;
        for (OverlayGob og : overlayed) {
            if (og.name.equals(name)) {
                isExist = true;
                og.add(highlightColor);
                break;
            } else isExist = false;
        }
        if (!isExist) overlayed.add(new OverlayGob(name, highlightColor));
        if (withsql) add(name, highlightColor);
    }

    public static void add(String name, Color highlightColor) {
        boolean isExist = false;
        for (OverlayGob og : overlayed) {
            if (og.name.equals(name)) {
                og.add(highlightColor);
                isExist = true;
                break;
            } else isExist = false;
        }
        if (!isExist) overlayed.add(new OverlayGob(name, highlightColor));

        Storage.overlays.write(sql -> {
            final PreparedStatement stmt = Storage.overlays.prepare("INSERT OR REPLACE INTO gob_highlight VALUES (? , ?)");
            stmt.setString(1, name);
            stmt.setInt(2, highlightColor.hashCode());
            stmt.executeUpdate();
        });

    }

    public static void add(String name, String text, Color textColor, Color strokeColor, int fontSize, String font) {
        boolean isExist = false;
        for (OverlayGob og : overlayed) {
            if (og.name.equals(name)) {
                og.add(text, textColor, strokeColor, fontSize, font);
                isExist = true;
                break;
            } else isExist = false;
        }
        if (!isExist) overlayed.add(new OverlayGob(name, text, textColor, strokeColor, fontSize, font));

        Storage.overlays.write(sql -> {
            final PreparedStatement stmt = Storage.overlays.prepare("INSERT OR REPLACE INTO gob_text VALUES (? , ? , ? , ? , ? , ?)");
            stmt.setString(1, name);
            stmt.setString(2, text);
            stmt.setInt(3, textColor.hashCode());
            stmt.setInt(4, strokeColor.hashCode());
            stmt.setInt(5, fontSize);
            stmt.setString(6, font);
            stmt.executeUpdate();
        });

    }

    public static OverlayGob get(String name) {
        for (OverlayGob og : overlayed) {
            if (og.name.equals(name)) {
                return og;
            }
        }
        return null;
    }

    public synchronized static void remove(final String name) {
        for (OverlayGob og : overlayed) {
            if (og.name.equals(name)) {
                overlayed.remove(og);
                break;
            }
        }
        Storage.overlays.write(sql -> {
            final PreparedStatement stmt = Storage.overlays.prepare("DELETE FROM gob_text WHERE name = ?");
            stmt.setString(1, name);
            stmt.executeUpdate();
        });
        Storage.overlays.write(sql -> {
            final PreparedStatement stmt = Storage.overlays.prepare("DELETE FROM gob_highlight WHERE name = ?");
            stmt.setString(1, name);
            stmt.executeUpdate();
        });
    }

    public synchronized static void removeText(final String name) {
        for (OverlayGob og : overlayed) {
            if (og.name.equals(name)) {
                og.add(null, null, null, 0, null);
                break;
            }
        }
        Storage.overlays.write(sql -> {
            final PreparedStatement stmt = Storage.overlays.prepare("DELETE FROM gob_text WHERE name = ?");
            stmt.setString(1, name);
            stmt.executeUpdate();
        });
    }

    public synchronized static void removeHighlight(final String name) {
        for (OverlayGob og : overlayed) {
            if (og.name.equals(name)) {
                og.add(null);
                break;
            }
        }
        Storage.overlays.write(sql -> {
            final PreparedStatement stmt = Storage.overlays.prepare("DELETE FROM gob_highlight WHERE name = ?");
            stmt.setString(1, name);
            stmt.executeUpdate();
        });
    }

    public synchronized static boolean isOverlayed(final String name) {
        boolean isExist = false;
        for (OverlayGob og : overlayed) {
            if (og.name.equals(name)) {
                isExist = true;
                break;
            } else isExist = false;
        }
        return isExist;
    }

    public synchronized static boolean isTexted(final String name) {
        boolean isExist = false;
        for (OverlayGob og : overlayed) {
            if (og.name.equals(name)) {
                if (og.text != null && og.textColor != null && og.strokeColor != null && og.fontSize != 0 && og.font != null) {
                    isExist = true;
                    break;
                }
            } else isExist = false;
        }
        return isExist;
    }

    public synchronized static boolean isHighlighted(final String name) {
        boolean isExist = false;
        for (OverlayGob og : overlayed) {
            if (og.name.equals(name)) {
                if (og.highlightColor != null) {
                    isExist = true;
                    break;
                }
            } else isExist = false;
        }
        return isExist;
    }

    public static class OverlayGob {
        public String name, text, font;
        public Color textColor, strokeColor, highlightColor;
        int fontSize;

        public OverlayGob(String name, String text, Color textColor, Color strokeColor, int fontSize, String font, Color highlightColor) {
            this.name = name;
            this.text = text;
            this.textColor = textColor;
            this.strokeColor = strokeColor;
            this.fontSize = fontSize;
            this.font = font;
            this.highlightColor = highlightColor;
        }

        public OverlayGob(String name, String text, Color textColor, Color strokeColor, int fontSize, String font) {
            this(name, text, textColor, strokeColor, fontSize, font, null);
        }

        public OverlayGob(String name, Color highlightColor) {
            this(name, null, null, null, 0, null, highlightColor);
        }

        public void add(Color highlightColor) {
            this.highlightColor = highlightColor;
        }

        public void add(String text, Color textColor, Color strokeColor, int fontSize, String font) {
            this.text = text;
            this.textColor = textColor;
            this.strokeColor = strokeColor;
            this.fontSize = fontSize;
            this.font = font;
        }
    }
}
