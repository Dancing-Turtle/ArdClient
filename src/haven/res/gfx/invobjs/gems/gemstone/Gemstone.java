//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by Fernflower decompiler)
//

package haven.res.gfx.invobjs.gems.gemstone;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.util.Hashtable;

import haven.Coord;
import haven.GOut;
import haven.GSprite;
import haven.GSprite.ImageSprite;
import haven.Message;
import haven.PUtils;
import haven.PUtils.Lanczos;
import haven.Resource;
import haven.Resource.Image;
import haven.Resource.Resolver;
import haven.Resource.Tooltip;
import haven.Tex;
import haven.TexI;
import haven.TexL;
import haven.TexR;
import haven.res.ui.tt.defn.DynName;

public class Gemstone extends GSprite implements ImageSprite, DynName  {
    public final BufferedImage img;
    public final Tex tex;
    public final String name;

    public Gemstone(Owner var1, Resource var2, Message var3) {
        super(var1);
        Resolver var4 = (Resolver)var1.context(Resolver.class);
        if (!var3.eom()) {
            Resource var5 = (Resource)var4.getres(var3.uint16()).get();
            Resource var6 = (Resource)var4.getres(var3.uint16()).get();
            this.tex = new TexI(this.img = construct(var5, var6));
            this.name = ((Tooltip)var5.layer(Resource.tooltip)).t + " " + ((Tooltip)var6.layer(Resource.tooltip)).t;
        } else {
            this.tex = new TexI(this.img = TexI.mkbuf(new Coord(32, 32)));
            this.name = "Broken gem";
        }
    }

    public static BufferedImage convert(BufferedImage var0) {
        WritableRaster var1 = Raster.createInterleavedRaster(0, var0.getWidth(), var0.getHeight(), 4, (Point)null);
        BufferedImage var2 = new BufferedImage(TexI.glcm, var1, false, (Hashtable)null);
        Graphics2D var3 = var2.createGraphics();
        var3.drawImage(var0, 0, 0, (ImageObserver)null);
        var3.dispose();
        return var2;
    }

    public static final WritableRaster alphamod(WritableRaster var0) {
        int var1 = var0.getWidth();
        int var2 = var0.getHeight();

        for(int var3 = 0; var3 < var2; ++var3) {
            for(int var4 = 0; var4 < var1; ++var4) {
                var0.setSample(var4, var3, 3, var0.getSample(var4, var3, 3) * 3 / 4);
            }
        }

        return var0;
    }

    public static final WritableRaster alphasq(WritableRaster var0) {
        int var1 = var0.getWidth();
        int var2 = var0.getHeight();

        for(int var3 = 0; var3 < var2; ++var3) {
            for(int var4 = 0; var4 < var1; ++var4) {
                int var5 = var0.getSample(var4, var3, 3);
                var0.setSample(var4, var3, 3, var5 * var5 / 255);
            }
        }

        return var0;
    }

    public static BufferedImage construct(Resource var0, Resource var1) {
        BufferedImage var2 = ((TexL)((TexR)var1.layer(TexR.class)).tex()).fill();

        Image var3;
        Image var4;
        Image var5;
        BufferedImage var6;
        BufferedImage var7;
        BufferedImage var8;
        try {
            var3 = (Image)var0.layer(Resource.imgc, Integer.valueOf(0));
            var4 = (Image)var0.layer(Resource.imgc, Integer.valueOf(1));
            var5 = (Image)var0.layer(Resource.imgc, Integer.valueOf(2));
            var6 = convert(var3.img);
            var7 = convert(var4.img);
            var8 = convert(var5.img);
        } catch (RuntimeException var12) {
            throw new RuntimeException("invalid gemstone in " + var0.name, var12);
        }

        Coord var9 = new Coord(32, 32);
        var2 = PUtils.convolvedown(var2, var9, new Lanczos(3.0D));
        WritableRaster var10 = PUtils.imgraster(var9);
        PUtils.blit(var10, var6.getRaster(), var3.o);
        WritableRaster var11 = PUtils.imgraster(var9);
        PUtils.blit(var11, var7.getRaster(), var4.o);
        PUtils.alphablit(var11, var8.getRaster(), var5.o);
        PUtils.tilemod(var11, var2.getRaster(), Coord.z);
        PUtils.alphablit(var11, alphasq(PUtils.blit(PUtils.imgraster(PUtils.imgsz(var8)), var8.getRaster(), Coord.z)), var5.o);
        PUtils.alphablit(var10, var11, Coord.z);
        return PUtils.rasterimg(var10);
    }

    public Coord sz() {
        return PUtils.imgsz(this.img);
    }

    public void draw(GOut var1) {
        var1.image(this.tex, Coord.z);
    }

    public String name() {
        return this.name;
    }

    public BufferedImage image() {
        return this.img;
    }
}
