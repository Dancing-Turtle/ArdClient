package haven;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.net.URI;

public class UpdateWnd extends Window {
    private static final String dwnurl = "https://github.com/Ardenneslol/ArdClient/releases/download/";

    //public static Label[] labels;
    public UpdateWnd(final String version) {
        super(Coord.z, "Update");
        int ycord = 10;
        Label lbl = new Label(Resource.getLocString(Resource.BUNDLE_LABEL, "New client version available") + " - v" + version);
        add(lbl, new Coord(20, ycord));
        String[] splitstring = Config.Changelog.split("-");
        for(int i = 0;i<splitstring.length;i++) {
           ycord = ycord+15;
           if (splitstring[i].length() > 200) {
               String suba = splitstring[i].substring(0, (splitstring[i].length() / 2));
               String subb = splitstring[i].substring((splitstring[i].length()/2)) ;
               Label lbl2 = new Label(Resource.getLocString(Resource.BUNDLE_LABEL, suba));
               add(lbl2, new Coord(20, ycord));
               ycord = ycord+15;
                lbl2 = new Label(Resource.getLocString(Resource.BUNDLE_LABEL, subb));
               add(lbl2, new Coord(20, ycord));
           }
           else {
               if(splitstring[i].contains("~"))
                   break;
               Label lbl2 = new Label(Resource.getLocString(Resource.BUNDLE_LABEL, splitstring[i]));
               add(lbl2, new Coord(20, ycord));
           }
        }


        Button btn = new Button(200, "Download Update") {
            public void click() {
                Desktop desktop = Desktop.isDesktopSupported() ? Desktop.getDesktop() : null;
                if (desktop != null && desktop.isSupported(Desktop.Action.BROWSE)) {
                    try {
                        desktop.browse(new URI(dwnurl + version + "/ArdClient-" + version + "-upd.zip"));
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        };
        add(btn, new Coord((lbl.sz.x + 20) / 2 - btn.sz.x / 2, ycord+15));
        pack();
        this.c = new Coord(HavenPanel.w / 2 - sz.x / 2, HavenPanel.h / 2 - sz.y / 2);
    }

    @Override
    public void wdgmsg(Widget sender, String msg, Object... args) {
        if (sender == cbtn) {
            ui.destroy(this);
        } else {
            super.wdgmsg(sender, msg, args);
        }
    }

    @Override
    public boolean type(char key, KeyEvent ev) {
        if (key == KeyEvent.VK_ESCAPE) {
            wdgmsg(cbtn, "click");
            return (true);
        }
        return (super.type(key, ev));
    }
}