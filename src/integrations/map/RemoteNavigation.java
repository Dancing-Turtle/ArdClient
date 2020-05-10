package integrations.map;

import haven.*;
import org.json.JSONArray;
import org.json.JSONObject;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.*;
import java.lang.ref.WeakReference;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.List;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/**
 * @author APXEOLOG (Artyom Melnikov), at 31.01.2019
 */
public class RemoteNavigation {
    private static final String INDEX_FILE_URL = Config.mapperUrl + "/grids/mapdata_index";
    private static final String API_ENDPOINT = Config.mapperUrl + "/api";

    private final File localMapdataIndexFile = new File(System.getProperty("user.dir"), "mapdata_index_local");

    private final ConcurrentLinkedQueue<GridData> receivedGrids = new ConcurrentLinkedQueue<>();
    private final AtomicInteger sessionCounter = new AtomicInteger(0);
    private final ConcurrentHashMap<Integer, Pair<Coord, Coord>> sessionAbsoluteRealPair = new ConcurrentHashMap<>();

    private final ExecutorService gridsUploader = Executors.newSingleThreadExecutor();
    private final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(3);

    private final ConcurrentHashMap<Long, Coord> globalMapdataIndex = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<Long, Coord> localMapdataIndex = new ConcurrentHashMap<>();

    private static volatile RemoteNavigation INSTANCE = null;

    public static RemoteNavigation getInstance() {
        if (INSTANCE == null) {
            synchronized (RemoteNavigation.class) {
                if (INSTANCE == null) {
                    INSTANCE = new RemoteNavigation();
                }
            }
        }
        return INSTANCE;
    }

    private RemoteNavigation() {
        // Load local caches
        File globalMapdataIndexFile = new File(System.getProperty("user.dir"), "mapdata_index_global");
        if (globalMapdataIndexFile.exists()) {
            scheduler.execute(new LoadIndexFileTask(globalMapdataIndexFile, globalMapdataIndex));
        }
        if (localMapdataIndexFile.exists()) {
            scheduler.execute(new LoadIndexFileTask(localMapdataIndexFile, localMapdataIndex));
        }
        // Request new mapdata cache and schedule update every 30 minutes
        scheduler.scheduleAtFixedRate(new LoadRemoteIndexTask(globalMapdataIndexFile, globalMapdataIndex),
                0L, 30, TimeUnit.MINUTES);
        // Update character's position on the map
        scheduler.scheduleAtFixedRate(new UpdateCharacterPosition(), 2L, 2L, TimeUnit.SECONDS);
    }

    private boolean firstTimeUpload = true;

    public void uploadMarkerData(MapFile mapFile) {
        if (firstTimeUpload) {
            scheduler.schedule(new UploadMarkersTask(mapFile), 5, TimeUnit.SECONDS);
            firstTimeUpload = false;
        }
    }

    /**
     * Find absolute coordinates for the specific gridId
     * @return null is there is no such data
     */
    public CompletableFuture<Coord> locateGridCoordinates(long gridId, boolean includeRemote) {
        Coord coordinates = globalMapdataIndex.get(gridId);
        if (coordinates != null) {
            //System.out.println(gridId + " found in global cache: " + coordinates);
            return CompletableFuture.completedFuture(coordinates);
        }
        coordinates = localMapdataIndex.get(gridId);
        if (coordinates != null) {
            //System.out.println(gridId + " found in local cache: " + coordinates);
            return CompletableFuture.completedFuture(coordinates);
        }
        if (includeRemote) {
            return CompletableFuture.supplyAsync(() -> {
                String response = getUrlResponse(API_ENDPOINT + "/v1/locate?gridId=" + gridId);
                if (response != null) {
                    String[] parts = response.split(";");
                    if (parts.length == 2) {
                        return new Coord(Integer.valueOf(parts[0]), Integer.valueOf(parts[1]));
                    }
                }
                return null;
            });
        } else {
            return CompletableFuture.completedFuture(null);
        }
    }

    Coord locateGridCoordinates(long gridId) {
        try {
            return locateGridCoordinates(gridId, false).get();
        } catch (Exception ex) {
            return null;
        }
    }

    void setCharacterGrid(long gridId, Coord virtualCoordinates, Coord detectedAbsoluteCoordinates) {
        final int sessionId = sessionCounter.get();
        System.out.println("Setting character grid " + gridId + " " + virtualCoordinates + " " + detectedAbsoluteCoordinates);
        if (sessionAbsoluteRealPair.containsKey(sessionId)) {
            // Do nothing, we already have this session processed
            System.out.println("This session is already present");
            return;
        } else {
            if (detectedAbsoluteCoordinates != null) {
                sessionAbsoluteRealPair.put(sessionId, new Pair<>(null, null));
            }
        }
        locateGridCoordinates(gridId, true).thenApply(cachedCoordinates -> {
            if (cachedCoordinates != null) return cachedCoordinates;
            else {
                if (detectedAbsoluteCoordinates != null) {
                    //System.out.println("Using detected absolute coordinates for grid " + gridId + ": " + detectedAbsoluteCoordinates);
                }
                return detectedAbsoluteCoordinates;
            }
        }).thenAccept(absoluteCoordinates -> {
            if (absoluteCoordinates == null) {
                System.out.println("No absolute coordinates for grid " + gridId);
                return;
            }
            localMapdataIndex.put(gridId, absoluteCoordinates);
            sessionAbsoluteRealPair.put(sessionId, new Pair<>(absoluteCoordinates, virtualCoordinates));

            Iterator<GridData> iterator = receivedGrids.iterator();
            while (iterator.hasNext()) {
                GridData gridData = iterator.next();
                if (gridData.sessionId < sessionId) {
                    iterator.remove();
                    System.out.println("Removed obsolete grid for session" + gridData.sessionId + " - " + gridData);
                } else if (gridData.sessionId == sessionId) {
                    gridData.calculateAbsoluteCoordinates(absoluteCoordinates, virtualCoordinates);
                    localMapdataIndex.put(gridData.gridId, gridData.absoluteGridCoordinates);
                    System.out.println("Calculated grid: " + gridData.toString());
                    iterator.remove();
                    if (Config.enableNavigationTracking) {
                        gridsUploader.submit(new UploadGridDataTask(gridData));
                    }
                }
            }
            Navigation.recalculateAbsoluteCoordinates();
            scheduler.execute(new SaveIndexFileTask(localMapdataIndexFile, localMapdataIndex));
        });
    }

    public void openBrowserMap(Coord gridCoord) {
        try {
            WebBrowser.self.show(new URL(
                    String.format(Config.mapperUrl + "/#/grid/%d/%d/6", gridCoord.x, gridCoord.y)));
        } catch (Exception ex) {}
    }

    /**
     * Receive new grid
     */
    public void receiveGrid(MCache.Grid grid) {
        int currentSession = sessionCounter.get();
        GridData gridData = new GridData(grid, currentSession);
        Pair<Coord, Coord> sessionOffsets = sessionAbsoluteRealPair.get(currentSession);
        if (sessionOffsets != null && sessionOffsets.a != null && sessionOffsets.b != null) {
            gridData.calculateAbsoluteCoordinates(sessionOffsets.a, sessionOffsets.b);
            localMapdataIndex.put(gridData.gridId, gridData.absoluteGridCoordinates);
            if (Config.enableNavigationTracking) {
                gridsUploader.submit(new UploadGridDataTask(gridData));
            }
            System.out.println("Detected grid on receive: " + gridData.toString());
        } else {
            receivedGrids.add(gridData);
        }
    }

    /**
     * Trim all grids
     */
    void removeAllGrids() {
        System.out.println("Remove all grids");
        receivedGrids.clear();
        sessionAbsoluteRealPair.clear();
        sessionCounter.incrementAndGet();
    }

    private static class GridData {
        WeakReference<MCache.Grid> gridReference;
        int sessionId;
        long gridId;
        Coord virtualGridCoordinates;
        Coord absoluteGridCoordinates = null;

        GridData(MCache.Grid grid, int sessionId) {
            this.gridReference = new WeakReference<>(grid);
            this.virtualGridCoordinates = grid.gc;
            this.sessionId = sessionId;
            this.gridId = grid.id;
        }

        void calculateAbsoluteCoordinates(Coord sessionACGC, Coord sessionVCGC) {
            this.absoluteGridCoordinates = sessionACGC.add(this.virtualGridCoordinates.sub(sessionVCGC));
        }

        @Override
        public String toString() {
            return String.format("Grid (%d) V:%s, A:%s", gridId,
                    String.valueOf(virtualGridCoordinates), String.valueOf(absoluteGridCoordinates));
        }
    }

    private static void importGridDataToMap(String gridData, Map<Long, Coord> target) {
        String[] parts = gridData.split(",");
        if (parts.length == 3) {
            target.put(Long.valueOf(parts[0]), new Coord(Integer.valueOf(parts[1]), Integer.valueOf(parts[2])));
        }
    }

    private static class LoadIndexFileTask implements Runnable {
        private File source;
        private Map<Long, Coord> target;

        LoadIndexFileTask(File source, Map<Long, Coord> target) {
            this.source = source;
            this.target = target;
        }

        @Override
        public void run() {
            try {
                Files.readAllLines(source.toPath(), StandardCharsets.UTF_8)
                        .forEach(line -> importGridDataToMap(line, target));
                System.out.println("Loaded index file: " + source);
            } catch (Exception ex) {
                System.err.println("Cannot load index file " + source + ": " + ex.getMessage());
            }
        }
    }

    private static class LoadRemoteIndexTask implements Runnable {
        private File cacheFile;
        private Map<Long, Coord> target;

        LoadRemoteIndexTask(File cacheFile, Map<Long, Coord> target) {
            this.cacheFile = cacheFile;
            this.target = target;
        }

        @Override
        public void run() {
            try {
                HttpURLConnection connection = (HttpURLConnection) new URL(INDEX_FILE_URL).openConnection();
                try (BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()))) {
                    target.clear();
                    reader.lines().forEach(line -> importGridDataToMap(line, target));
                    System.out.println("Loaded remote index");
                } finally {
                    connection.disconnect();
                }
            } catch (Exception ex) {
                System.err.println("Cannot load remote index file: " + ex.getMessage());
            }
            try {
                // Update cache
                List<String> gridDataList = target.entrySet().stream().map(entry -> String.format("%d,%d,%d",
                        entry.getKey(), entry.getValue().x, entry.getValue().y)).collect(Collectors.toList());
                Files.write(cacheFile.toPath(), gridDataList, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
            } catch (Exception ex) {
                System.err.println("Cannot save remote index file: " + ex.getMessage());
            }
        }
    }

    private static class SaveIndexFileTask implements Runnable {
        private File cacheFile;
        private Map<Long, Coord> source;

        SaveIndexFileTask(File cacheFile, Map<Long, Coord> source) {
            this.cacheFile = cacheFile;
            this.source = source;
        }

        @Override
        public void run() {
            try {
                // Update cache
                List<String> gridDataList = source.entrySet().stream().map(entry -> String.format("%d,%d,%d",
                        entry.getKey(), entry.getValue().x, entry.getValue().y)).collect(Collectors.toList());
                Files.write(cacheFile.toPath(), gridDataList, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
            } catch (Exception ex) {
                System.err.println("Cannot save index file: " + ex.getMessage());
            }
        }
    }

    private class UploadGridDataTask implements Runnable {
        private GridData gridData;

        UploadGridDataTask(GridData gridData) {
            this.gridData = gridData;
        }

        @Override
        public void run() {
            try {
                MCache.Grid grid = this.gridData.gridReference.get();
                Glob glob = Glob.getByReference();
                if (grid != null && glob != null && glob.map != null) {
                    BufferedImage image = MinimapImageGenerator.drawmap(glob.map, grid);
                    if (image == null) {
                        throw new Loading();
                    }
                    try {
                        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
                        ImageIO.write(image, "png", outputStream);
                        ByteArrayInputStream inputStream = new ByteArrayInputStream(outputStream.toByteArray());
                        MultipartUtility multipart = new MultipartUtility(API_ENDPOINT + "/v2/updateGrid", "utf-8");
                        multipart.addFormField("id", String.valueOf(this.gridData.gridId));
                        multipart.addFormField("x", String.valueOf(this.gridData.absoluteGridCoordinates.x));
                        multipart.addFormField("y", String.valueOf(this.gridData.absoluteGridCoordinates.y));
                        multipart.addFilePart("file", inputStream, "minimap.png");
                        MultipartUtility.Response response = multipart.finish();
                        if (response.statusCode != 200) {
                            System.out.println("Upload Error: Code" + response.statusCode + " - " + response.response);
                        } else {
                            System.out.println("Uploaded " + gridData);
                        }
                    } catch (IOException e) {
                        System.out.println("Cannot upload " + gridData + ": " + e.getMessage());
                    }
                }
            } catch (Loading ex) {
                // Retry on Loading
                gridsUploader.submit(this);
            }
        }
    }

    private static class UpdateCharacterPosition implements Runnable {
        @Override
        public void run() {
            if (Config.enableNavigationTracking && Navigation.getCharacterId() > -1) {
                Coord2d coordinates = Navigation.getAbsoluteCoordinates();
                Coord2d detectedAC = Navigation.getDetectedAbsoluteCoordinates();
                HashMap<String, Object> dataToSend = new HashMap<>();
                if (coordinates != null) {
                    dataToSend.put("x", (int) (coordinates.x / 11));
                    dataToSend.put("y", (int) (coordinates.y / 11));
                    dataToSend.put("type", "located");
                } else if (detectedAC != null) {
                    dataToSend.put("x", (int) (detectedAC.x / 11));
                    dataToSend.put("y", (int) (detectedAC.y / 11));
                    dataToSend.put("type", "detected");
                }
                if (dataToSend.size() > 0) {
                    dataToSend.put("name", Navigation.getCharacterName());
                    dataToSend.put("id", Navigation.getCharacterId());
                    try {
                        HttpURLConnection connection =
                                (HttpURLConnection) new URL(API_ENDPOINT + "/v2/updateCharacter").openConnection();
                        connection.setRequestMethod("POST");
                        connection.setRequestProperty("Content-Type", "application/json;charset=UTF-8");
                        connection.setDoOutput(true);
                        try (DataOutputStream out = new DataOutputStream(connection.getOutputStream())) {
                            String json = new JSONObject(dataToSend).toString();
                            out.write(json.getBytes(StandardCharsets.UTF_8));
                        }
                        connection.getResponseCode();
                    } catch (Exception ex) { }
                }
            }
        }
    }

    private static class MarkerData {
        String name;
        Coord gridOffset;
        String image = null;
        Long objectId;
        long gridId;
        long gridTime;
        Color color;

        @Override
        public String toString() {
            return String.format("%s - %s (%d)", name, image, objectId);
        }

        public HashMap<String, Object> dataToSend() {
            HashMap<String, Object> tmp = new HashMap<>();
            tmp.put("name", name);
            tmp.put("gridId", gridId);
            tmp.put("x", gridOffset.x);
            tmp.put("y", gridOffset.y);
            tmp.put("image", image);
            return tmp;
        }
    }

    private static class UploadMarkersTask implements Runnable {
        private MapFile mapFile;

        public UploadMarkersTask(MapFile mapFile) {
            this.mapFile = mapFile;
        }

        @Override
        public void run() {
            if (mapFile.lock.readLock().tryLock()) {
                List<MarkerData> markers;
                try {
                    markers= mapFile.markers.stream()
                            .map(marker -> {
                                Coord markerGridOffset = new Coord((int) Math.floor(marker.tc.x / 100.0),
                                        (int) Math.floor(marker.tc.y / 100.0));
                                MapFile.Segment segment = mapFile.segments.get(marker.seg);
                                MarkerData markerData = new MarkerData();
                                markerData.name = marker.nm;
                                markerData.gridOffset = marker.tc.sub(markerGridOffset.mul(100));
                                markerData.gridId = segment.map.get(markerGridOffset);
                                if (marker instanceof MapFile.SMarker) {
                                    markerData.image = ((MapFile.SMarker) marker).res.name;
                                    markerData.objectId = ((MapFile.SMarker) marker).oid;
                                } else if (marker instanceof MapFile.PMarker) {
                                    markerData.color = ((MapFile.PMarker) marker).color;
                                }
                                return markerData;
                            }).collect(Collectors.toList());
                } finally {
                    mapFile.lock.readLock().unlock();
                }
                ArrayList<MarkerData> loadedMarkers = new ArrayList<>();
                while (!markers.isEmpty()) {
                    Iterator<MarkerData> iterator = markers.iterator();
                    while (iterator.hasNext()) {
                        MarkerData markerData = iterator.next();
                        try {
                            if (markerData.color != null) {
                                if (Config.sendCustomMarkers && markerData.color.equals(Color.GREEN)) {
                                    loadedMarkers.add(markerData);
                                }
                            } else {
                                loadedMarkers.add(markerData);
                            }
                            iterator.remove();
                        } catch (Loading ex) { }
                    }
                    try {
                        Thread.sleep(50);
                    } catch (InterruptedException ex) { }
                }
                System.out.println("Loaded " + loadedMarkers.size() + " markers");
                if (!loadedMarkers.isEmpty()) {
                    try {
                        HttpURLConnection connection = (HttpURLConnection)
                                new URL(API_ENDPOINT + "/v1/uploadMarkers").openConnection();
                        connection.setRequestMethod("POST");
                        connection.setRequestProperty("Content-Type", "application/json;charset=UTF-8");
                        connection.setDoOutput(true);
                        try (OutputStream outputStream = connection.getOutputStream()) {
                            Collection collection = loadedMarkers.stream()
                                    .map(MarkerData::dataToSend).collect(Collectors.toCollection(ArrayList::new));
                            String json = new JSONArray(collection).toString();
                            outputStream.write(json.getBytes(StandardCharsets.UTF_8));
                        }
                        connection.getResponseCode();
                        System.out.println("Sent markers data");
                    } catch (Exception ex) {
                        System.out.println("Cannot upload markers: " + ex.getMessage());
                    }
                }
            } else {
                System.out.println("Mapfile lock is busy");
            }
        }
    }

    private static String getUrlResponse(String url) {
        try {
            HttpURLConnection connection = (HttpURLConnection) new URL(url).openConnection();
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()))) {
                return reader.lines().collect(Collectors.joining());
            } finally {
                connection.disconnect();
            }
        } catch (Exception ex) {
            return null;
        }
    }
}
