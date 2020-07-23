package integrations.mapv4;

import haven.*;
import haven.MCache.LoadingMap;
import haven.MapFile.Marker;
import haven.MapFile.PMarker;
import haven.MapFile.SMarker;
import haven.sloth.gob.Type;
import org.json.JSONArray;
import org.json.JSONObject;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.*;
import java.lang.ref.WeakReference;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;
import java.util.stream.Collectors;


/**
 * @author Vendan
 */
public class MappingClient {
    private ExecutorService gridsUploader = Executors.newSingleThreadExecutor();
    private ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(3);
    public String mapString;

    private static volatile MappingClient INSTANCE = null;

    public static MappingClient getInstance() {
        if (INSTANCE == null) {
            synchronized (MappingClient.class) {
                if (INSTANCE == null) {
                    INSTANCE = new MappingClient();
                }
            }
        }
        return INSTANCE;
    }

    private boolean trackingEnabled;

    /***
     * Enable tracking for this execution.  Must be called each time the client is started.
     * @param enabled
     */
    public void EnableTracking(boolean enabled) {
        trackingEnabled = enabled;
    }

    private boolean gridEnabled;

    /***
     * Enable grid data/image upload for this execution.  Must be called each time the client is started.
     * @param enabled
     */
    public void EnableGridUploads(boolean enabled) {
        gridEnabled = enabled;
    }

    private PositionUpdates pu = new PositionUpdates();

    private MappingClient() {
        scheduler.scheduleAtFixedRate(pu, 5L, 5L, TimeUnit.SECONDS);
    }

    public String endpoint;

    /***
     * Set mapping server endpoint.  Must be called each time the client is started.  Takes effect immediately.
     * @param endpoint
     */
    public void SetEndpoint(String endpoint) {
        this.endpoint = endpoint;
    }

    private String playerName;

    /***
     * Set the player name.  Typically called from Charlist.wdgmsg
     * @param name
     */
    public void SetPlayerName(String name) {
        playerName = name;
    }

    /***
     * Checks that the endpoint is functional and matches the version of this mapping client.
     * @return
     */
    public boolean CheckEndpoint() {
        try {
            HttpURLConnection connection =
                    (HttpURLConnection) new URL(endpoint + "/checkVersion?version=4").openConnection();
            connection.setRequestMethod("GET");
            return connection.getResponseCode() == 200;
        } catch(Exception ex) {
            return false;
        }
    }

    public volatile MapRef lastMapRef;

    private Coord lastGC = null;
    /***
     * Called when entering a new grid
     * @param gc Grid coordinates
     */
    public void EnterGrid(Coord gc) {
        lastGC = gc;
        GetMapRef(true);
        scheduler.execute(new GenerateGridUpdateTask(gc));
    }

    /***
     * Called as you move around, automatically calculates if you have entered a new grid and calls EnterGrid accordingly.
     * @param c Normal coordinates
     */
    public void CheckGridCoord(Coord2d c) {
        Coord gc = toGC(c);
        if(lastGC == null || !gc.equals(lastGC)) {
            EnterGrid(gc);
        }
    }

    private Map<Long, MapRef> cache = new HashMap<Long, MapRef>();
    /***
     * Gets a MapRef (mapid, coordinate pair) for the players current location
     * @return Current grid MapRef
     */
    public MapRef GetMapRef(boolean remote) {
        try {
            Gob player = Glob.getByReference().oc.getGUI().map.player();
            Coord gc = toGC(player.rc);
            synchronized(cache) {
                long id = Glob.getByReference().map.getgrid(gc).id;
                MapRef mapRef = cache.get(id);
                if(mapRef == null && remote) {
                    scheduler.execute(new Locate(id));
                }
                lastMapRef = mapRef;
                return mapRef;
            }
        } catch (Exception e) {}
        return null;
    }

    /***
     * Given a mapref, opens the map to the corresponding location
     * @param mapRef
     */
    public void OpenMap(MapRef mapRef) {
        try {
            mapString = String.format(endpoint + "/#/grid/%d/%d/%d/6", mapRef.mapID, mapRef.gc.x, mapRef.gc.y);
            WebBrowser.self.show(new URL(
                    mapString));
        } catch (Exception ex) {}
    }

    private class Locate implements Runnable {
        long gridID;

        Locate(long gridID) {
            this.gridID = gridID;
        }

        @Override
        public void run() {
            try {
                final HttpURLConnection connection =
                        (HttpURLConnection) new URL(endpoint + "/locate?gridID=" + gridID).openConnection();
                try (BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()))) {
                    String resp = reader.lines().collect(Collectors.joining());
                    String[] parts = resp.split(";");
                    if (parts.length == 3) {
                        MapRef mr = new MapRef(Integer.valueOf(parts[0]), new Coord(Integer.valueOf(parts[1]), Integer.valueOf(parts[2])));
                        synchronized(cache) {
                            cache.put(gridID, mr);
                        }
                    }

                } finally {
                    connection.disconnect();
                }

            } catch (final Exception ex) { }
        }
    }

    /***
     * Process a mapfile to extract markers to upload
     * @param mapfile
     * @param uploadCheck
     */
    public void ProcessMap(MapFile mapfile, Predicate<Marker> uploadCheck) {
        scheduler.schedule(new ExtractMapper(mapfile, uploadCheck), 5, TimeUnit.SECONDS);
    }

    private class ExtractMapper implements Runnable {
        MapFile mapfile;
        Predicate<Marker> uploadCheck;
        int retries = 5;

        ExtractMapper(MapFile mapfile, Predicate<Marker> uploadCheck) {
            this.mapfile = mapfile;
            this.uploadCheck = uploadCheck;
        }

        @Override
        public void run() {
            if (mapfile.lock.readLock().tryLock()) {
                List<MarkerData> markers;
                try {
                    markers = mapfile.markers.stream().filter(uploadCheck).map(m -> {
                        long gridid;
                        try {
                            Coord mgc = new Coord(Math.floorDiv(m.tc.x, 100), Math.floorDiv(m.tc.y, 100));
                            gridid = mapfile.segments.get(m.seg).map.get(mgc);
                        } catch (Exception ex) {
                            return null;
                        }
                        return new MarkerData(m, gridid);
                    }).filter(m -> m != null).collect(Collectors.toList());
                } catch(Exception ex) {
                    if(retries-- > 0) {
                        System.out.println("rescheduling upload");
                        scheduler.schedule(this, 5, TimeUnit.SECONDS);
                    }
                    return;
                } finally {
                    mapfile.lock.readLock().unlock();
                }
                System.out.println("collected " + markers.size() + " markers");

                scheduler.execute(new ProcessMapper(mapfile, markers));
            } else {
                if(retries-- > 0) {
                    System.out.println("rescheduling upload");
                    scheduler.schedule(this, 5, TimeUnit.SECONDS);
                }
            }
        }
    }

    private class MarkerData {
        Marker m;
        long gridID;

        MarkerData(Marker m, long gridID) {
            this.m = m;
            this.gridID = gridID;
        }
    }

    private class ProcessMapper implements Runnable {
        MapFile mapfile;
        List<MarkerData> markers;

        ProcessMapper(MapFile mapfile, List<MarkerData> markers) {
            this.mapfile = mapfile;
            this.markers = markers;
        }

        @Override
        public void run() {
            ArrayList<JSONObject> loadedMarkers = new ArrayList<>();
            while(!markers.isEmpty()) {
                System.out.println("processing " + markers.size() + " markers");
                Iterator<MarkerData> iterator = markers.iterator();
                while (iterator.hasNext()) {
                    MarkerData md = iterator.next();
                    try {
                        Coord mgc = new Coord(Math.floorDiv(md.m.tc.x, 100), Math.floorDiv(md.m.tc.y, 100));
                        long gridId = md.gridID;
                        JSONObject o = new JSONObject();
                        o.put("name", md.m.nm);
                        o.put("gridID", String.valueOf(gridId));
                        Coord gridOffset = md.m.tc.sub(mgc.mul(100));
                        o.put("x", gridOffset.x);
                        o.put("y", gridOffset.y);

                        if(md.m instanceof SMarker) {
                            o.put("type", "shared");
                            o.put("id", ((SMarker) md.m).oid);
                            o.put("image", ((SMarker) md.m).res.name);
                        } else if(md.m instanceof PMarker) {
                            o.put("type", "player");
                            o.put("color", ((PMarker) md.m).color);
                        }
                        loadedMarkers.add(o);
                        iterator.remove();
                    } catch (Loading ex) {
                    }
                }
                try {
                    Thread.sleep(50);
                } catch (InterruptedException ex) { }
            }
            System.out.println("scheduling marker upload");
            try {
                scheduler.execute(new MarkerUpdate(new JSONArray(loadedMarkers.toArray())));
            } catch(Exception ex) {
                System.out.println(ex);
            }
        }
    }

    private class MarkerUpdate implements Runnable {
        JSONArray data;

        MarkerUpdate(JSONArray data) {
            this.data = data;
        }

        @Override
        public void run() {
            try {
                System.out.println("Uploading markers");
                HttpURLConnection connection =
                        (HttpURLConnection) new URL(endpoint + "/markerUpdate").openConnection();
                connection.setRequestMethod("POST");
                connection.setRequestProperty("Content-Type", "application/json;charset=UTF-8");
                connection.setDoOutput(true);
                try (DataOutputStream out = new DataOutputStream(connection.getOutputStream())) {
                    final String json = data.toString();
                    out.write(json.getBytes(StandardCharsets.UTF_8));
                }
                System.out.println("Marker upload " + connection.getResponseCode());
            } catch (Exception ex) {
                System.out.println(ex);
            }
        }
    }

    private class PositionUpdates implements Runnable {
        private PositionUpdates() {
        }

        @Override
        public void run() {
            if (trackingEnabled) {
                JSONObject upload = new JSONObject();
                try {
                    Glob g = Glob.getByReference();
                    if(g == null){
                        return;
                    }
                    for(Gob gob : g.oc) {
                        try {
                            if(gob.type == Type.HUMAN){
                                JSONObject j = new JSONObject();
                                if(gob.isplayer()){
                                    j.put("name", playerName);
                                    j.put("type", "player");
                                } else {
                                    KinInfo ki = gob.getattr(KinInfo.class);
                                    if(ki == null) {
                                        j.put("name", "???");
                                        j.put("type", "unknown");
                                    }else{
                                        j.put("name", ki.name);
                                        j.put("type", Integer.toHexString(BuddyWnd.gc[ki.group].getRGB()));
                                    }
                                }
                                MCache.Grid grid = Glob.getByReference().map.getgrid(toGC(gob.rc));
                                j.put("gridID", String.valueOf(grid.id));
                                JSONObject c = new JSONObject();
                                Coord2d goc = gridOffset(gob.rc);
                                c.put("x", (int)(goc.x/11));
                                c.put("y", (int)(goc.y/11));
                                j.put("coords", c);
                                upload.put(String.valueOf(gob.id), j);
                            }
                        }
                        catch(Exception ex) {
                            System.out.println(ex);
                        }
                    }
                }catch(Exception ex) {
                    System.out.println(ex);
                    return;
                }

                try {
                    final HttpURLConnection connection =
                            (HttpURLConnection) new URL(endpoint + "/positionUpdate").openConnection();
                    connection.setRequestMethod("POST");
                    connection.setRequestProperty("Content-Type", "application/json;charset=UTF-8");
                    connection.setDoOutput(true);
                    try (DataOutputStream out = new DataOutputStream(connection.getOutputStream())) {
                        final String json = upload.toString();
                        out.write(json.getBytes(StandardCharsets.UTF_8));
                    }
                    connection.getResponseCode();
                } catch (final Exception ex) {
                    System.out.println(ex);
                }
            }
        }
    }

    private static class GridUpdate {
        String[][] grids;
        Map<String, WeakReference<MCache.Grid>> gridRefs;

        GridUpdate(final String[][] grids, Map<String, WeakReference<MCache.Grid>> gridRefs) {
            this.grids = grids;
            this.gridRefs = gridRefs;
        }

        @Override
        public String toString() {
            return String.format("GridUpdate (%s)", grids[1][1]);
        }
    }

    private class GenerateGridUpdateTask implements Runnable {
        Coord coord;
        int retries = 3;

        GenerateGridUpdateTask(Coord c) {
            this.coord = c;
        }

        @Override
        public void run() {
            if (gridEnabled) {
                final String[][] gridMap = new String[3][3];
                Map<String, WeakReference<MCache.Grid>> gridRefs = new HashMap<String, WeakReference<MCache.Grid>>();
                Glob glob = Glob.getByReference();
                try {
                    for(int x = -1; x <= 1; x++) {
                        for(int y = -1; y <= 1; y++) {
                            final MCache.Grid subg = glob.map.getgrid(coord.add(x, y));
                            gridMap[x+1][y+1] = String.valueOf(subg.id);
                            gridRefs.put(String.valueOf(subg.id), new WeakReference<MCache.Grid>(subg));
                        }
                    }
                    //System.out.println("Scheduling grid request");
                    scheduler.execute(new UploadGridUpdateTask(new GridUpdate(gridMap, gridRefs)));
                } catch(LoadingMap lm) {
                    retries--;
                    if(retries >= 0) {
                        scheduler.schedule(this, 1L, TimeUnit.SECONDS);
                    }
                }
            }
        }
    }

    private class UploadGridUpdateTask implements Runnable {
        private final GridUpdate gridUpdate;

        UploadGridUpdateTask(final GridUpdate gridUpdate) {
            this.gridUpdate = gridUpdate;
        }

        @Override
        public void run() {
            if (gridEnabled) {
                HashMap<String, Object> dataToSend = new HashMap<>();

                dataToSend.put("grids", this.gridUpdate.grids);
                try {
                    HttpURLConnection connection =
                            (HttpURLConnection) new URL(endpoint + "/gridUpdate").openConnection();
                    connection.setRequestMethod("POST");
                    connection.setRequestProperty("Content-Type", "application/json;charset=UTF-8");
                    connection.setDoOutput(true);
                    try (DataOutputStream out = new DataOutputStream(connection.getOutputStream())) {
                        String json = new JSONObject(dataToSend).toString();
                        //System.out.println("Sending grid update " + json);
                        out.write(json.getBytes(StandardCharsets.UTF_8));
                    }
                    if (connection.getResponseCode() == 200) {
                        DataInputStream dio = new DataInputStream(connection.getInputStream());
                        int nRead;
                        byte[] data = new byte[1024];
                        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
                        while ((nRead = dio.read(data, 0, data.length)) != -1) {
                            buffer.write(data, 0, nRead);
                        }
                        buffer.flush();
                        JSONObject jo = new JSONObject(buffer.toString(StandardCharsets.UTF_8.name()));
                        synchronized(cache) {
                            try {
                                MapRef mr = new MapRef(jo.getLong("map"), new Coord(jo.getJSONObject("coords").getInt("x"), jo.getJSONObject("coords").getInt("y")));
                                lastMapRef = mr;
                                cache.put(Long.valueOf(gridUpdate.grids[1][1]), mr);
                            } catch (Exception ex) {}
                        }
                        JSONArray reqs = jo.optJSONArray("gridRequests");
                        if(reqs != null) {
                            for(int i = 0; i < reqs.length(); i++){
                                gridsUploader.execute(new GridUploadTask(reqs.getString(i), gridUpdate.gridRefs.get(reqs.getString(i))));
                            }
                        }
                    }

                } catch (Exception ex) { }
            }
        }
    }

    private class GridUploadTask implements Runnable {
        private final String gridID;
        private final WeakReference<MCache.Grid> grid;
        private int retries = 5;

        GridUploadTask(String gridID, WeakReference<MCache.Grid> grid) {
            this.gridID = gridID;
            this.grid = grid;
        }

        @Override
        public void run() {
            try {
                Glob glob = Glob.getByReference();
                MCache.Grid g = grid.get();
                if (g != null && glob != null && glob.map != null) {
                    BufferedImage image = MinimapImageGenerator.drawmap(glob.map, g);
                    if (image == null) {
                        throw new Loading();
                    }
                    try {
                        JSONObject extraData = new JSONObject();
                        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
                        ImageIO.write(image, "png", outputStream);
                        ByteArrayInputStream inputStream = new ByteArrayInputStream(outputStream.toByteArray());
                        MultipartUtility multipart = new MultipartUtility(endpoint + "/gridUpload", "utf-8");
                        multipart.addFormField("id", this.gridID);
                        multipart.addFilePart("file", inputStream, "minimap.png");

                        extraData.put("season", glob.ast.is);

                        multipart.addFormField("extraData", extraData.toString());

                        MultipartUtility.Response response = multipart.finish();
                        if (response.statusCode != 200) {
                            System.out.println("Upload Error: Code" + response.statusCode + " - " + response.response);
                        } else {
                            System.out.println("Uploaded " + gridID);
                        }
                    } catch (IOException e) {
                        System.out.println("Cannot upload " + gridID + ": " + e.getMessage());
                    }
                }
            } catch (Loading ex) {
                // Retry on Loading
                if(retries-- > 0) {
                    gridsUploader.submit(this);
                }
            }

        }
    }

    private static Coord toGC(Coord2d c) {
        return new Coord(Math.floorDiv((int)c.x, 1100), Math.floorDiv((int)c.y, 1100));
    }
    private static Coord toGridUnit(Coord2d c) {
        return new Coord(Math.floorDiv((int)c.x, 1100) * 1100, Math.floorDiv((int)c.y, 1100) * 1100);
    }
    private static Coord2d gridOffset(Coord2d c) {
        Coord gridUnit = toGridUnit(c);
        return new Coord2d(c.x - gridUnit.x, c.y - gridUnit.y);
    }

    public class MapRef{
        public Coord gc;
        public long mapID;

        private MapRef(long mapID, Coord gc) {
            this.gc = gc;
            this.mapID = mapID;
        }

        public String toString() {
            return (gc.toString() + " in map space " + mapID);
        }
    }
}
