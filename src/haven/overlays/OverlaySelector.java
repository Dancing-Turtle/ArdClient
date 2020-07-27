package haven.overlays;

import haven.Button;
import haven.CheckBox;
import haven.ColorPreview;
import haven.Coord;
import haven.Dropbox;
import haven.GOut;
import haven.Gob;
import haven.Label;
import haven.ResizableTextEntry;
import haven.Tex;
import haven.Text;
import haven.Widget;
import haven.WidgetVerticalAppender;
import haven.Window;

import java.awt.*;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

public class OverlaySelector extends Window {
    private final String gobname;
    int selectedFontSize;
    String selectedFont;

    private CheckBox textchbox, highlightchbox;
    private ResizableTextEntry textEntry;
    private ColorPreview textColorPreview, strokeColorPreview, highlightColorPreview;
    private Color textColor, strokeColor, highlightColor;
    private Dropbox<String> fontSizeDropbox, fontDropBox;

    private CheckBox makechbox(String text, boolean defbol, String tootip) {
        return new CheckBox(text) {
            {
                a = defbol;
            }

            public void set(boolean val) {
                a = val;
            }

            @Override
            public Object tooltip(Coord c0, Widget prev) {
                Tex tex = Text.render(tootip).tex();
                return tex;
            }
        };
    }

    private String getDefaultTextName() {
        int p;
        if (gobname.contains("/")) {
            p = gobname.lastIndexOf('/');
            if (p < 0) return (gobname);
            return gobname.substring(p + 1, p + 2).toUpperCase() + gobname.substring(p + 2);
        } else return gobname;

    }
    private static final List<String> fontSizes = Arrays.asList("10", "11", "12", "13", "14", "15", "16");
    private Dropbox<String> makeFontSizeDropbox(int fontSize) {
        return new Dropbox<String>(fontSizes.size(), fontSizes) {
            {
                super.change(Integer.toString(fontSize));
                selectedFontSize = fontSize;
            }
            @Override
            protected String listitem(int i) {
                return fontSizes.get(i);
            }
            @Override
            protected int listitems() {
                return fontSizes.size();
            }
            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }
            @Override
            public void change(String item) {
                super.change(item);
                selectedFontSize = Integer.parseInt(item);
            }
        };
    }
    private Dropbox<String> makeFontsDropdown(String font) {
        final List<String> fonts = Arrays.asList(GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames());
        return new Dropbox<String>(8, fonts) {
            {
                super.change(font);
                selectedFont = font;
            }

            @Override
            protected String listitem(int i) {
                return fonts.get(i);
            }

            @Override
            protected int listitems() {
                return fonts.size();
            }

            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, Coord.z);
            }

            @Override
            public void change(String item) {
                super.change(item);
                selectedFont = item;
            }
        };
    }

    public OverlaySelector(final String gobname) {
        super(Coord.z, "Overlay Selector");
        this.gobname = gobname;
        textchbox = makechbox("Color text overlay: ", OverlayData.isTexted(gobname), "Install a text sticker on current object");
        highlightchbox = makechbox("Color overlay: ", OverlayData.isHighlighted(gobname), "Color current object");

        String text = OverlayData.isTexted(gobname) ? OverlayData.get(gobname).text : getDefaultTextName();
        textEntry = new ResizableTextEntry(text);

        textColor = OverlayData.isTexted(gobname) ? OverlayData.get(gobname).textColor : new Color(new Random().nextInt(255), new Random().nextInt(255), new Random().nextInt(255), 255);
        strokeColor = OverlayData.isTexted(gobname) ? OverlayData.get(gobname).strokeColor : new Color(0, 0, 0, 255);
        highlightColor = OverlayData.isHighlighted(gobname) ? OverlayData.get(gobname).highlightColor : new Color(new Random().nextInt(255), new Random().nextInt(255), new Random().nextInt(255), 255);

        int fontSize = OverlayData.isTexted(gobname) ? OverlayData.get(gobname).fontSize : 12;
        String font = OverlayData.isTexted(gobname) ? OverlayData.get(gobname).font : "sans";

        fontSizeDropbox = makeFontSizeDropbox(fontSize);
        fontDropBox = makeFontsDropdown(font);

        textColorPreview = new ColorPreview(new Coord(16, 16), textColor, val -> { textColor = val; });
        strokeColorPreview = new ColorPreview(new Coord(16, 16), strokeColor, val -> { strokeColor = val; });
        highlightColorPreview = new ColorPreview(new Coord(16, 16), highlightColor, val -> { highlightColor = val; });

        final WidgetVerticalAppender appender = new WidgetVerticalAppender(this);
        final WidgetVerticalAppender appender2 = new WidgetVerticalAppender(this);
        appender2.setX(200);

        appender.add(new Label("Select overlays for " + gobname));
        appender.add(textchbox); int y = appender.getX();
        appender.addRow(new Label("Text: "), textEntry);
        appender.addRow(new Label("Text color: "), textColorPreview);
        appender.addRow(new Label("Stroke color: "), strokeColorPreview);
        appender.addRow(new Label("Font size: "), fontSizeDropbox);
        appender.addRow(new Label("Font: "), fontDropBox);

        appender2.add(new Label(""));
        appender2.add(highlightchbox);
        appender2.addRow(new Label("Highlight color: "), highlightColorPreview);

        appender.add(new Button(50, "Select", this::select));
        pack();
    }

    @Override
    public void close() {
        ui.destroy(this);
    }

    private void select() { //FIXME переработать, чтобы были выключенные и выключенные и менюшку тоже
        if (textchbox.a) {
            OverlayData.addText(gobname, textEntry.text, textColor, strokeColor, selectedFontSize, selectedFont, true);
            ui.sess.glob.oc.unovTextGobs(gobname);
            ui.sess.glob.oc.ovTextGobs(gobname);
        } else {
            OverlayData.addText(gobname, null, null, null, 0, null, false);
            OverlayData.removeText(gobname);
            ui.sess.glob.oc.unovTextGobs(gobname);
        }
        if (highlightchbox.a) { //FIXME markedgobs need testing
            OverlayData.addHighlight(gobname, highlightColor, true);
//            ui.sess.glob.oc.unovHighGobs(gobname);
//            ui.sess.glob.oc.ovHighGobs(gobname);
        } else {
            OverlayData.addHighlight(gobname, null, false);
            OverlayData.removeHighlight(gobname);
//            ui.sess.glob.oc.unovHighGobs(gobname);
        }
        if (!textchbox.a && !highlightchbox.a) {
            OverlayData.remove(gobname);
        }
        ui.destroy(this);
    }
}
