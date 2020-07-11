package modification;

import haven.Tex;
import haven.TexI;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

public class configuration {

	public static BufferedImage scaleImage(BufferedImage before, int scale) {
		try {
			int w = before.getWidth();
			int h = before.getHeight();
			// Create a new image of the proper size
			int w2 = (int) (w * scale);
			int h2 = (int) (h * scale);
			BufferedImage after = new BufferedImage(w2, h2, BufferedImage.TYPE_INT_ARGB);
			AffineTransform scaleInstance = AffineTransform.getScaleInstance(scale, scale);
			AffineTransformOp scaleOp = new AffineTransformOp(scaleInstance, AffineTransformOp.TYPE_BILINEAR);

			scaleOp.filter(before, after);
			return after;
		} catch (Exception e) {
			e.printStackTrace();
			return before;
		}
	}

	public static BufferedImage scaleImage(BufferedImage before, int newWidth, int newHeight) {
		try {
			int w = before.getWidth();
			int h = before.getHeight();
			// Create a new image of the proper size
			int w2 = newWidth;
			int h2 = newHeight;
			double scale1 = (double) w2 / w;
			double scale2 = (double) h2 / h;
			BufferedImage after = new BufferedImage(w2, h2, BufferedImage.TYPE_INT_RGB);
			AffineTransform scaleInstance = AffineTransform.getScaleInstance(scale1, scale2);
			AffineTransformOp scaleOp = new AffineTransformOp(scaleInstance, AffineTransformOp.TYPE_BILINEAR);

			scaleOp.filter(before, after);
			return after;
		} catch (Exception e) {
			e.printStackTrace();
			return before;
		}
	}

	private static Tex getTex(String name) throws IOException {
		BufferedImage in;
		File img = new File(name);
		in = ImageIO.read(img);

		BufferedImage newImage = new BufferedImage(in.getWidth(), in.getHeight(), BufferedImage.TYPE_INT_ARGB);

		Graphics2D g = newImage.createGraphics();
		g.drawImage(in, 0, 0, null);
		g.dispose();
		Tex tex = new TexI(newImage);
		return tex;
	}

	private static Tex getTex(String name, int width, int height) throws IOException {
		BufferedImage in;
		File img = new File(name);
		in = ImageIO.read(img);

		BufferedImage newImage = new BufferedImage(in.getWidth(), in.getHeight(), BufferedImage.TYPE_INT_ARGB);


		Graphics2D g = newImage.createGraphics();
		g.drawImage(in, 0, 0, null);
		g.dispose();

		Tex tex = new TexI(scaleImage(newImage, width, height));
		return tex;
	}

	public static Tex imageToTex(String name) {

		BufferedImage in;
		try {
			return getTex(name);
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	public static Tex imageToTex(String name, Tex defaultTex) {

		BufferedImage in;
		try {
			return getTex(name);
		} catch (Exception e) {
			e.printStackTrace();
			return defaultTex;
		}
	}

	public static Tex imageToTex(String name, int width, int height, Tex defaultTex) {

		BufferedImage in;
		try {
			return getTex(name, width, height);
		} catch (Exception e) {
			e.printStackTrace();
			return defaultTex;
		}
	}
}
