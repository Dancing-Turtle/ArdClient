package haven;

public class ChatWnd extends ResizableWnd {
    private final ChatUI chat;
    private boolean minimized;
    private Coord szr;

    public ChatWnd(final ChatUI chat) {
        super(chat.sz, "Chat");
        this.chat = chat;
	add(chat, Coord.z);
    }

    @Override
    public void close()
    {
	hide();
      //  minimize();
    }

    @Override
    protected void added() {
	super.added();
	System.out.println(c);
	chat.resize(asz);
    }

    @Override
    public void resize(Coord sz) {
	super.resize(sz);
	chat.resize(asz);
    }

    private void minimize() {
        minimized = !minimized;
        if (minimized) {
            this.chat.hide();
        } else {
            this.chat.show();
        }

        if (minimized) {
            szr = asz;
            resize(new Coord(asz.x, 8));
        } else {
            resize(szr);
        }
    }
}
