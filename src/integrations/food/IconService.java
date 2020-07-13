package integrations.food;

import haven.GSprite;
import haven.ItemInfo;
import haven.resutil.FoodInfo;
import org.json.JSONArray;
import org.json.JSONObject;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.HashSet;
import java.util.List;
import java.util.Scanner;

public class IconService {

	private static HashSet<String> sentIcons = new HashSet<>();

	static {
		getSentIcons();
	}

	public static void getSentIcons() {
		try {
			HttpURLConnection conn = (HttpURLConnection) new URL(FoodService.API_ENDPOINT + "data/icons.json").openConnection();
			conn.setRequestProperty("User-Agent", "H&H Client");
			conn.setRequestProperty("Cache-Control", "no-cache");
			Scanner scan = new Scanner(conn.getInputStream());
			StringBuilder sb = new StringBuilder();
			while(scan.hasNextLine())
				sb.append(scan.nextLine());
			JSONArray arr = new JSONArray(sb.toString());
			arr.forEach((s) -> sentIcons.add(s.toString()));
		} catch(IOException e) {
			e.printStackTrace();
		}
	}

	public static void checkIcon(List<ItemInfo> infos, GSprite sprite) {
		if(ItemInfo.find(FoodInfo.class, infos) == null)
			return;
		String name = null;
		for(ItemInfo info : infos) {
			if (info instanceof ItemInfo.Name) {
				name = ((ItemInfo.Name) info).str.text;
			}
		}
		if(name != null && !sentIcons.contains(name)) {
			try {
				BufferedImage img = ((GSprite.ImageSprite)sprite).image();
				if(img != null)
					sendIcon(name, img);
			} catch(Exception e) {
				e.printStackTrace();
			}
			sentIcons.add(name);
		}

	}

	private static void sendIcon(String name, BufferedImage icon) {
		Runnable run = new Runnable() {
			@Override
			public void run() {
				try {
					HttpURLConnection conn = (HttpURLConnection) new URL(FoodService.API_ENDPOINT + "icon").openConnection();
					conn.setRequestMethod("POST");
					conn.setRequestProperty("User-Agent", "H&H Client");
					conn.setRequestProperty("Content-Type", "application/json;charset=UTF-8");
					conn.setDoOutput(true);

					ByteArrayOutputStream os = new ByteArrayOutputStream();
					ImageIO.write(icon, "png", os);
					os.flush();

					JSONObject obj = new JSONObject();
					obj.put("name", name);
					obj.put("icon", new String(Base64.getEncoder().encode(os.toByteArray()), StandardCharsets.UTF_8));

					DataOutputStream dos = new DataOutputStream(conn.getOutputStream());
					dos.write(obj.toString().getBytes(StandardCharsets.UTF_8));
					dos.close();
					System.out.println("Sent a new food icon with a response code: " + conn.getResponseCode());
				} catch(IOException e) {
					e.printStackTrace();
				}
			}
		};
		FoodService.scheduler.execute(run);
	}
}
