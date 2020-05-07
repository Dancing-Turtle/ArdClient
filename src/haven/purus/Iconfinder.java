package haven.purus;

import haven.CheckListboxItem;
import haven.Config;
import haven.Resource;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Iconfinder {
    URL url = this.getClass().getResource("icons");
    static List<String> lines;

    public static Map<String, String> icons = new HashMap<>();

    public static void loadConfig() {
//		File tempFile = new File(String.valueOf(Paths.get("src/icons")));
//		boolean exists = tempFile.exists();
//		System.out.println(exists);
        try {
            lines = Files.readAllLines(Paths.get("./icons"));
            for(int i=0; i<lines.size()-1; i++) {
                String split[] = lines.get(i).split(";");
                icons.put(split[0], split[1]);
            }
        } catch (IOException e) {

        }
        try {
            if (lines == null){
                lines = Files.readAllLines(Paths.get("src/icons"));
                for(int i=0; i<lines.size()-1; i++) {
                    String split[] = lines.get(i).split(";");
                    icons.put(split[0], split[1]);
                }
            }
        } catch (IOException e){

        }
    }
    public static void updateConfig() {
        StringBuilder sb = new StringBuilder();

        HashMap<String, String> exceptions = new HashMap<String, String>() {{
            put("corkoak", "gfx/invobjs/cork");
            put("stonepine", "gfx/invobjs/stonepinecone");
            put("strawberrytree", "gfx/invobjs/woodstrawberry");
            put("oldtrunk", "gfx/invobjs/wblock-mirkwood");
        }};

        for(Map.Entry<String, CheckListboxItem> entry: Config.trees.entrySet()) {
            String icon = null;
            if(exceptions.containsKey(entry.getKey())) {
                icon = exceptions.get(entry.getKey());
            }
            if(icon == null && entry.getKey().contains("tree"))
                icon = findTreeIcon(entry.getKey().replaceAll("tree", ""));
            if(icon == null)
                icon = findTreeIcon(entry.getKey());
            if(icon == null) {
                System.out.println("Didn't find icon for: " + entry.getKey());
            } else {
                sb.append(entry.getKey() + ";" + icon + "\n");
                System.out.println("icon for: " + entry.getKey() + " is: " + icon);
            }
        }

        for(Map.Entry<String, CheckListboxItem> entry: Config.bushes.entrySet()) {
            String icon = null;
            if(entry.getKey().contains("bush"))
                icon = findTreeIcon(entry.getKey().replaceAll("bush", ""));
            if(icon == null)
                icon = findTreeIcon(entry.getKey());
            if(icon == null) {
                System.out.println("Didn't find icon for: " + entry.getKey());
            } else {
                sb.append(entry.getKey() + ";" + icon + "\n");
                System.out.println("icon for: " + entry.getKey() + " is: " + icon);
            }
        }

        for(Map.Entry<String, CheckListboxItem> entry : Config.boulders.entrySet()) {
            String icon = findStoneIcon(entry.getKey());
            if(icon == null ){
                System.out.println("Didn't find icon for: " + entry.getKey());
            } else {
                sb.append(entry.getKey() + ";" + icon + "\n");
                System.out.println("icon for: " + entry.getKey() + " is: " + icon);
            }
        }

        try {
            Files.write(Paths.get("./icons"), Collections.singleton(sb.toString()), StandardCharsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
        } catch(IOException e) {
            e.printStackTrace();
        }
    }

    private static String findStoneIcon(String name) {
        try {
            Resource res = Resource.remote().loadwait("gfx/invobjs/" + name);
            return res.name;
        } catch(Exception e) {
            // Ignore
        }
        return null;
    }

    private static String findTreeIcon(String name) {
        String options[] = { // In order of most favourable
                "leaf-" + name,
                "leaf-" + name + "tree",
                name,
                name + "fruit",
                name + "berry",
                name + "nut",
                name + "apple",
                "seed-" + name,
        };
        for(int i=0; i<options.length; i++) {
            try {
                Resource res = Resource.remote().loadwait("gfx/invobjs/" + options[i]);
                return res.name;
            } catch(Exception e) {
                // Ignore
            }
        }
        return null;
    }
}
