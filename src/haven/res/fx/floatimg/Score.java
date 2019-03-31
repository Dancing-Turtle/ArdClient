package haven.res.fx.floatimg;

import haven.Message;
import haven.Resource;
import haven.Sprite;

import java.awt.*;

public class Score implements Sprite.Factory {
    private static int dup(int paramInt) {
	return paramInt << 4 | paramInt;
    }

    public Sprite create(Sprite.Owner paramOwner, Resource paramResource, Message paramMessage) {
	int i = paramMessage.int32();
	int j = paramMessage.uint8();
	int k = paramMessage.uint16();

	final String str1;
	if (i < 0) {
	    i = -i;
	    str1 = "-";
	} else {
	    str1 = (j & 0x1) != 0 ? "+" : "";
	}
	int m = (j & 0x6) >> 1;
	final String str2;
	if (m == 1) {
	    str2 = (i / 10) + "." + (i % 10);
	} else if (m == 2) {
	    str2 = String.format("%02d:%02d", (i / 60), (i % 60));
	} else {
	    str2 = Integer.toString(i);
	}

	Color localColor = new Color(dup((k & 0xF000) >> 12),
		dup((k & 0xF00) >> 8),
		dup((k & 0xF0) >> 4),
		dup((k & 0xF)));
	return new FloatText(paramOwner, paramResource, str1+str2, localColor);
    }
}
