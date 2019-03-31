package haven.sloth.util;

public class BinaryFile {
    private byte[] back;
    private int off;
    private int size;


    public BinaryFile(final byte[] back) {
        this.back = back;
        off = 0;
        size = back.length;
    }

    public BinaryFile() {
        back = new byte[1];
        off = 0;
        size = 0;
    }

    public byte[] getBytes() {
        byte[] ret = new byte[size];
        System.arraycopy(back,0,ret,0,size);
        return ret;
    }

    private void ensure(int bytes) {
        if(off+bytes < back.length)
            return;
        final byte[] nback = new byte[back.length + back.length << 1 + bytes];
        System.arraycopy(back, 0, nback, 0, back.length);
        back = nback;
    }

    public boolean eof() {
        return off == back.length;
    }

    public void ebyte(byte b) {
        ensure(1);
        back[off++] = b;
        size++;
    }

    public void ebytes(byte bs[]) {
        ensure(bs.length);
        for(byte b : bs)
            back[off++] = b;
        size += bs.length;
    }

    public void eshort(short s) {
        ensure(2);
        back[off++] = (byte)(s & 0x00FF);
        back[off++] = (byte)(s & 0xFF00);
        size += 2;
    }

    public void eint(int i) {
        ensure(4);
        back[off++] = (byte)(i & 0x000000FF);
        back[off++] = (byte)(i & 0x0000FF00);
        back[off++] = (byte)(i & 0x00FF0000);
        back[off++] = (byte)(i & 0xFF000000);
        size += 4;
    }

    public void elong(long l) {
        ensure(8);
        back[off++] = (byte)(l & 0x00000000000000FFl);
        back[off++] = (byte)(l & 0x000000000000FF00l);
        back[off++] = (byte)(l & 0x0000000000FF0000l);
        back[off++] = (byte)(l & 0x00000000FF000000l);
        back[off++] = (byte)(l & 0x000000FF00000000l);
        back[off++] = (byte)(l & 0x0000FF0000000000l);
        back[off++] = (byte)(l & 0x00FF000000000000l);
        back[off++] = (byte)(l & 0xFF00000000000000l);
        size += 8;
    }

    public void estr(String s) {
        ebytes(s.getBytes());
        ebyte((byte)0x00);
    }

    public byte dbyte() {
        return back[off++];
    }

    public byte[] dbytes() {
        byte[] bs = new byte[back.length-off];
        int i;
        for(i=0;i<bs.length;++i)
            bs[i] = back[i+off];
        return bs;
    }

    public byte[] dbytesTill(byte stop) {
        byte[] bs;
        int len = 0, start = off;
        int i, j = 0;

        while(back[off++] != stop) len++;

        bs = new byte[len];
    
        for(i = start;i < off-1;++i)
            bs[j++] = back[i];

        return bs;
    }

    public short dshort() {
        return (short) 
            (back[off++] +
             (back[off++] << 8));
    }

    public int dint() {
        return
            back[off++]         + 
            (back[off++] << 8)  + 
            (back[off++] << 16) + 
            (back[off++] << 24);
    }

    public long dlong() {
        return
            back[off++]         + 
            (back[off++] << 8)  + 
            (back[off++] << 16) + 
            (back[off++] << 24) + 
            (back[off++] << 32) + 
            (back[off++] << 40) + 
            (back[off++] << 48) + 
            (back[off++] << 56);
    }

    public String dstr() {
        return new String(dbytesTill((byte)0x00));
    }
}
