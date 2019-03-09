package haven;

public class ChatWnd extends ResizableWnd {
    private final ChatUI chat;

    public ChatWnd(final ChatUI chat) {
        super(chat.sz, "Chat");
        this.chat = chat;
	add(chat, Coord.z);
    }

    @Override
    public void close() {
	hide();
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
}
