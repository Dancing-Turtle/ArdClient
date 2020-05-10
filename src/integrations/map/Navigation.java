package integrations.map;

import haven.Config;
import haven.Coord;
import haven.Coord2d;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author APXEOLOG (Artyom Melnikov), at 27.01.2019
 */
public class Navigation {
    public static final List<String> UNDERGROUND_TILES = Arrays.asList("gfx/tiles/mine", "gfx/tiles/cave");
    public static final List<String> WATER_TILES = Arrays.asList("gfx/tiles/water", "gfx/tiles/deep");
    private static final Coord2d MAGIC_WISP_POSITION = new Coord2d(-9938.5, -9960.5);

    public enum GridType { UNKNOWN, CAVE, HOUSE, SURFACE, UNKNOWN_WATER, UNKNOWN_PAVING,
        CHARACTER_GENERATION, CHARACTER_SWITCH }

    private static class PlayerPartyCoordinates {
        Coord2d realGridUnitCoordinates;    // Player's real grid TL coordinates in units
        Coord2d virtualCoordinates = null;  // Player's virtual coordinates

        PlayerPartyCoordinates(Coord2d realGridUnitCoordinates) {
            this.realGridUnitCoordinates = realGridUnitCoordinates;
        }

        boolean isWispPosition() {
            return MAGIC_WISP_POSITION.equals(realGridUnitCoordinates) && MAGIC_WISP_POSITION.equals(virtualCoordinates);
        }

        boolean ready() {
            return realGridUnitCoordinates != null && virtualCoordinates != null;
        }
    }

    public static boolean isValidGridType(GridType gridType) {
        return gridType == GridType.SURFACE || gridType == GridType.UNKNOWN_WATER;
    }

    /**
     * Tracks current session type
     */
    private static boolean isSessionSetup = false;
    private static GridType sessionType = null;

    private static void setSessionType(GridType type) {
        sessionType = type;
        isSessionSetup = true;
        logMessage("Navigation: Set session type to "  + type);
    }

    /*
     * Methods call order:
     * 0. setCharacterName from CharacterList when we select character to login
     * 1. addPartyCoordinates from Party, fill all party coordinates
     * 2. setCharacterId from MapView
     * 3. setPlayerCoordinates (from (2), position also comes from MapView)
     * 4. receiveGridData from MCache (grids are requested by MapView)
     */

    /**
     * Current character name
     */
    private static String characterName = "";

    public static synchronized void setCharacterName(String name) {
        if (lastPlayerCoordinates != null) {
            setSessionType(GridType.CHARACTER_SWITCH);
        }
        if (Config.mapperHashName) {
            try {
                MessageDigest digest = MessageDigest.getInstance("SHA-256");
                byte[] hash = digest.digest(name.getBytes(StandardCharsets.UTF_8));
                characterName = Base64.getEncoder().encodeToString(hash);
            } catch (NoSuchAlgorithmException e) {
                e.printStackTrace();
                characterName = "(ง'̀-'́)ง";
            }
        } else {
            characterName = name;
        }
    }

    public static String getCharacterName() {
        return characterName;
    }

    /**
     * Coordinates of the party players on initial login. Contains both real and virtual coordinates
     */
    private static HashMap<Long, PlayerPartyCoordinates> sessionPartyCoordinates = new HashMap<>();

    public static void addPartyCoordinates(long gobId, Coord2d coordinates) {
        if (!isSessionSetup) {
            synchronized (Navigation.class) {
                PlayerPartyCoordinates partyCoordinates = sessionPartyCoordinates.get(gobId);
                if (partyCoordinates == null) {
                    sessionPartyCoordinates.put(gobId, new PlayerPartyCoordinates(coordinates));
                } else {
                    if (partyCoordinates.virtualCoordinates == null) {
                        partyCoordinates.virtualCoordinates = coordinates;
                    }
                }
            }
        }
    }

    /**
     * Current character id
     * Process sessionPartyCoordinates when we receive this:
     *  - Check for the character selection instance by coordinates
     *  - Calculate absolute coordinates for the player
     */
    private static long characterId = -1;

    public static synchronized void setCharacterId(long id, Coord2d coordinates) {
        characterId = id;
        setPlayerCoordinates(coordinates);
    }

    public static long getCharacterId() {
        return characterId;
    }

    /**
     * Player's character virtual coordinates
     */
    private static Coord2d lastPlayerCoordinates = null;


    private static ConcurrentHashMap<Coord, Long> virtualGridsCache = new ConcurrentHashMap<>();

    public static synchronized void setPlayerCoordinates(Coord2d coordinates) {
        lastPlayerCoordinates = coordinates;
        recalculateAbsoluteCoordinates();
    }

    static synchronized void recalculateAbsoluteCoordinates() {
        Long gridId = virtualGridsCache.get(lastPlayerCoordinates.toGridCoordinate());
        if (gridId != null) {
            Coord gridCoord = RemoteNavigation.getInstance().locateGridCoordinates(gridId);
            if (gridCoord != null) {
                absoluteCoordinates = lastPlayerCoordinates.gridOffset().add(gridCoord.mul(1100));
            } else {
                absoluteCoordinates = null;
            }
        } else absoluteCoordinates = null;
    }

    public static synchronized void receiveGridData(Coord gridCoordinate, long gridId, GridType gridType) {
        if (lastPlayerCoordinates == null)
            return;
        System.out.print(gridCoordinate + " + " + gridId + " + " + gridType);
        virtualGridsCache.put(gridCoordinate, gridId);

        if (gridCoordinate.equals(lastPlayerCoordinates.toGridCoordinate())) {
            if (detectedAbsoluteCoordinates != null) {
                detectedAbsoluteCoordinates = null;
            }
            if (sessionType == null) {
                setSessionType(gridType);
            }
            if (isValidGridType(gridType)) {
                // Check if this is first login
                if (!sessionPartyCoordinates.isEmpty()) {
                    PlayerPartyCoordinates partyCoordinates = sessionPartyCoordinates.get(characterId);
                    if (partyCoordinates != null) {
                        if (partyCoordinates.isWispPosition()) {
                            setSessionType(GridType.CHARACTER_GENERATION);
                        } else if (partyCoordinates.ready()) {
                            detectedAbsoluteCoordinates = partyCoordinates.realGridUnitCoordinates.inv();
                            System.out.println("Detected AC: " + detectedAbsoluteCoordinates);
                        }
                    }
                    sessionPartyCoordinates.clear();
                }
                if (isValidGridType(sessionType)) {
                    RemoteNavigation.getInstance().setCharacterGrid(gridId, gridCoordinate, null);
                }
            }
        }
        recalculateAbsoluteCoordinates();
    }

    /**
     * Generate absolute coordinates
     */
    private static volatile Coord2d absoluteCoordinates = null;

    private static volatile Coord2d detectedAbsoluteCoordinates = null;

    /**
     * Get absolute grid coordinates
     */

    public static synchronized void mapdataReset() {
        logMessage("Navigation: Big jump detected, session type cleanup");
        sessionType = null;
        absoluteCoordinates = null;
        virtualGridsCache.clear();
        RemoteNavigation.getInstance().removeAllGrids();
    }

    private static void logMessage(String msg) {
        System.out.println(msg);
    }

    /**
     * Call from Session() to reset before login
     */
    public static synchronized void reset() {
        isSessionSetup = false;
        lastPlayerCoordinates = null;
        sessionType = null;
        characterName = null;
        characterId = -1;
        sessionPartyCoordinates.clear();
        virtualGridsCache.clear();
        RemoteNavigation.getInstance().removeAllGrids();
    }

    public static Coord2d getAbsoluteCoordinates() {
        return absoluteCoordinates;
    }

    public static Coord2d getDetectedAbsoluteCoordinates() {
        return detectedAbsoluteCoordinates;
    }
}
