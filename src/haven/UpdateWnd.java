package haven;

import java.awt.Desktop;
import java.awt.event.KeyEvent;
import java.net.URI;

public class UpdateWnd extends Window {
    private static final String dwnurl = "https://github.com/romovs/amber/releases/download/";

    public UpdateWnd(final String version) {
        super(Coord.z, "Update");

        Label lbl = new Label(Resource.getLocString(Resource.BUNDLE_LABEL, "New client version available") + " - v" + version + "    ");
        add(lbl, new Coord(20, 40));

        Button btn = new Button(200, "Download Update") {
            public void click() {
                Desktop desktop = Desktop.isDesktopSupported() ? Desktop.getDesktop() : null;
                if (desktop != null && desktop.isSupported(Desktop.Action.BROWSE)) {
                    try {
                        desktop.browse(new URI(dwnurl + version + "/amber-" + version + "-upd.zip"));
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        };
        add(btn, new Coord((lbl.sz.x + 20) / 2 - btn.sz.x / 2, 80));
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