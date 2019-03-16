package haven;

public class QuestWnd extends Window {
    public QuestWnd() {
        super(Coord.z, (Resource.getLocString(Resource.BUNDLE_WINDOW, "Quest Log")), "Quest Log");
	makeHidable();
        hide();
    }

    @Override
    public void close() {
        hide();
    }
    public void cresize(Widget ch) {
        pack();
        if(parent != null)
            presize();
    }

    @Override
    public void cdestroy(Widget w) {
	hide();
	ui.gui.qqview = null;
        super.cdestroy(w);
    }


    @Override
    public <T extends Widget> T add(T child) {
	show();
        return super.add(child);
    }
}
