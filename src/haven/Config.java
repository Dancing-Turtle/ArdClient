/*
 *  This file is part of the Haven & Hearth game client.
 *  Copyright (C) 2009 Fredrik Tolf <fredrik@dolda2000.com>, and
 *                     Björn Johannessen <johannessen.bjorn@gmail.com>
 *
 *  Redistribution and/or modification of this file is subject to the
 *  terms of the GNU Lesser General Public License, version 3, as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  Other parts of this source tree adhere to other copying
 *  rights. Please see the file `COPYING' in the root directory of the
 *  source tree for details.
 *
 *  A copy the GNU Lesser General Public License is distributed along
 *  with the source tree of which this file is a part in the file
 *  `doc/LPGL-3'. If it is missing for any reason, please see the Free
 *  Software Foundation's website at <http://www.fsf.org/>, or write
 *  to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
 */

package haven;

import haven.error.ErrorHandler;
import org.json.JSONArray;
import org.json.JSONObject;

import java.awt.event.KeyEvent;
import java.io.*;
import java.net.URL;
import java.util.*;

import static haven.Utils.getprop;

public class Config {
    public static final File HOMEDIR = new File("").getAbsoluteFile();
    public static boolean dumpcode = getprop("haven.dumpcode", "off").equals("on");
    public static final boolean iswindows = System.getProperty("os.name").startsWith("Windows");
    public static final String LINE_SEPARATOR = System.getProperty("line.separator");
    public static String authuser = null;
    public static String authserv = null;
    public static String defserv = null;
    public static URL resurl = null;
    public static boolean dbtext = false;
    public static boolean profile = false;
    public static boolean profilegpu = false;
    public static boolean nopreload = false;
    public static int mainport = 1870;
    public static int authport = 1871;
    public static boolean lowerterraindistance = Utils.getprefb("lowerterraindistance", false);
    public static boolean hidesky = Utils.getprefb("hidesky", false);
    public static boolean noloadscreen = Utils.getprefb("noloadscreen", false);
    public static URL screenurl = geturl("http://game.havenandhearth.com/mt/ss");
    public static boolean hideflocomplete = Utils.getprefb("hideflocomplete", false);
    public static boolean hideflovisual = Utils.getprefb("hideflovisual", false);
    public static boolean daylight = Utils.getprefb("daylight", false);
    public static boolean longtooltips = Utils.getprefb("longtooltips", true);
    public static boolean showkinnames = Utils.getprefb("showkinnames", true);
    public static boolean savemmap = Utils.getprefb("savemmap", false);
    public static boolean studylock = Utils.getprefb("studylock", false);
    public static boolean chatsave = Utils.getprefb("chatsave", false);
    public static boolean chattimestamp = Utils.getprefb("chattimestamp", true);
    public static boolean flatcupboards = Utils.getprefb("flatcupboards", true);
    public static boolean showquality = Utils.getprefb("showquality", true);
    public static boolean showroadendpoint = Utils.getprefb("showroadendpoint", true);
    public static boolean showroadmidpoint = Utils.getprefb("showroadmidpoint", false);
    public static boolean qualitywhole = Utils.getprefb("qualitywhole", true);
    public static int badcamsensitivity = Utils.getprefi("badcamsensitivity", 5);
    public static List<LoginData> logins = new ArrayList<LoginData>();
    public static boolean shooanimals = Utils.getprefb("shooanimals",false);
    public static boolean horseautorun = Utils.getprefb("horseautorun",true);
    public static boolean mapshowgrid = Utils.getprefb("mapshowgrid", false);
    public static boolean mapshowviewdist = Utils.getprefb("mapshowviewdist", false);
    public static boolean disabletiletrans = Utils.getprefb("disabletiletrans", false);
    public static boolean itemmeterbar = Utils.getprefb("itemmeterbar", false);
    public static boolean showprogressperc = Utils.getprefb("showprogressperc", true);

    public static boolean quickslots = Utils.getprefb("quickslots", true);
    public static boolean quickbelt = Utils.getprefb("quickbelt", false);
    public static boolean statuswdgvisible = Utils.getprefb("statuswdgvisible", false);

    public static boolean batcircle = Utils.getprefb("batcircle", true);
    public static boolean slimecircle = Utils.getprefb("slimecircle", true);
    public static boolean errorsounds = Utils.getprefb("errorsounds", true);
    public static boolean cleavesound = Utils.getprefb("cleavesound", true);
    public static boolean chatsounds = Utils.getprefb("chatsounds", true);
    public static boolean discordsounds = Utils.getprefb("discordsounds", true);
    public static boolean realmchatalerts = Utils.getprefb("realmchatalerts", false);

    public static double sfxchipvol = Utils.getprefd("sfxchipvol", 0.9);
    public static double sfxquernvol = Utils.getprefd("sfxquernvol", 0.9);
    public static double sfxfirevol = Utils.getprefd("sfxfirevol", 1.0);
    public static double sfxclapvol = Utils.getprefd("sfxclapvol", 1.0);
    public static double sfxbeehivevol = Utils.getprefd("sfxbeehivevol", 1.0);
    public static double sfxchatvol = Utils.getprefd("sfxchatvol", 1.0);
    public static double sfxcauldronvol = Utils.getprefd("sfxcauldronvol", 1.0);
    public static double sfxwhistlevol = Utils.getprefd("sfxwhistlevol", 1.0);
    public static boolean showcraftcap = Utils.getprefb("showcraftcap", true);
    public static boolean showgobhp = Utils.getprefb("showgobhp", false);
    public static boolean showplantgrowstage = Utils.getprefb("showplantgrowstage", false);
    public static boolean notifykinonline = Utils.getprefb("notifykinonline", true);
    public static boolean showminerad = Utils.getprefb("showminerad", false);
    public static boolean showTroughrad = Utils.getprefb("showTroughrad", false);
    public static boolean showBeehiverad = Utils.getprefb("showBeehiverad", false);
    public static boolean showweather = Utils.getprefb("showweather", true);
    public static boolean simplecrops = Utils.getprefb("simplecrops", false);
    public static boolean logincharsheet = Utils.getprefb("logincharsheet", false);
    public static boolean loginstudywnd = Utils.getprefb("loginstudywnd", false);
    public static boolean loginbelt = Utils.getprefb("loginbelt", true);
    public static boolean escclosewindows = Utils.getprefb("escclosewindows", true);
    public static boolean afklogout = Utils.getprefb("afklogout", false);
    public static int afklogouttime = Utils.getprefi("afklogouttime", 0);
    public static boolean simpleforage = Utils.getprefb("simpleforage", false);
    //public static boolean hidecrops = Utils.getprefb("hidecrops", false);
    public static boolean showfps = Utils.getprefb("showfps", false);
    public static boolean autohearth = Utils.getprefb("autohearth", false);
    public static boolean showplayerpaths = Utils.getprefb("showplayerpaths", false);
    public static boolean showinvonlogin = Utils.getprefb("showinvonlogin", false);
    public static boolean runonlogin = Utils.getprefb("runonlogin", false);
    public static Coord chatsz = Utils.getprefc("chatsz", new Coord(683, 111));
    public static boolean autostudy = Utils.getprefb("autostudy", false);
    public static boolean showdmgop = Utils.getprefb("showdmgop", true);
    public static boolean showothercombatinfo = Utils.getprefb("showothercombatinfo", true);
    public static boolean hidegobs = Utils.getprefb("hidegobs", false);
    public static boolean qualitybg = Utils.getprefb("qualitybg", true);
    public static int qualitybgtransparency = Utils.getprefi("qualitybgtransparency", 5);
    public static boolean showwearbars = Utils.getprefb("showwearbars", true);
    public static boolean tilecenter = Utils.getprefb("tilecenter", false);
    public static boolean userazerty = Utils.getprefb("userazerty", false);
    public static boolean hlightcuropp = Utils.getprefb("hlightcuropp", false);
    public static boolean cRackmissing = Utils.getprefb("cRackmissing", false);
    public static boolean reversebadcamx = Utils.getprefb("reversebadcamx", false);
    public static boolean reversebadcamy = Utils.getprefb("reversebadcamy", false);
    public static boolean showservertime = Utils.getprefb("showservertime", false);
    public static boolean enabletracking = Utils.getprefb("enabletracking", false);
    public static boolean enableswimming = Utils.getprefb("enableswimming", false);
    public static boolean autoconnectdiscord = Utils.getprefb("autoconnectdiscord", false);
    public static boolean autoconnectarddiscord = Utils.getprefb("autoconnectarddiscord", false);
    public static boolean enablecrime = Utils.getprefb("enablecrime", false);
    public static boolean resinfo = Utils.getprefb("resinfo", true);
    public static boolean showanimalrad = Utils.getprefb("showanimalrad", true);
    public static boolean hwcursor = Utils.getprefb("hwcursor", false);
    public static boolean showboundingboxes = Utils.getprefb("showboundingboxes", false);
    public static boolean showcooldown = Utils.getprefb("showcooldown", false);
    public static boolean nodropping = Utils.getprefb("nodropping", false);
    public static boolean nodropping_all = Utils.getprefb("nodropping_all", false);
    public static boolean fbelt = Utils.getprefb("fbelt", false);
    public static boolean histbelt = Utils.getprefb("histbelt", false);
    public static boolean dropMinedStones = Utils.getprefb("dropMinedStones", true);
    public static boolean dropMinedOre = Utils.getprefb("dropMinedOre", true);
    public static boolean dropMinedOrePrecious = Utils.getprefb("dropMinedOrePrecious", true);
    public static boolean dropMinedCatGold = Utils.getprefb("dropMinedCatGold", true);
    public static boolean dropMinedSeaShells = Utils.getprefb("dropMinedSeaShells", true);
    public static boolean dropMinedCrystals = Utils.getprefb("dropMinedCrystals", true);
    public static boolean dropsmelterstones = Utils.getprefb("dropsmelterstones", true);
    public static boolean showdframestatus = Utils.getprefb("showdframestatus", true);
    public static boolean showcoopstatus = Utils.getprefb("showcoopstatus", true);
    public static boolean hideallicons = Utils.getprefb("hideallicons", false);
    public static boolean showhutchstatus = Utils.getprefb("showhutchstatus", true);
    public static boolean showrackstatus = Utils.getprefb("showrackstatus", true);
    public static boolean showcupboardstatus = Utils.getprefb("showcupboardstatus", true);
    public static boolean enableorthofullzoom = Utils.getprefb("enableorthofullzoom", false);
    public static boolean partycircles =  Utils.getprefb("partycircles", false);
    public static boolean kincircles =  Utils.getprefb("kincircles", false);
    public static boolean playercircle =  Utils.getprefb("playercircle", false);
    public static boolean stranglevinecircle =  Utils.getprefb("stranglevinecircle", false);
    public static boolean doubleradius =  Utils.getprefb("doubleradius", false);
    public static boolean noquests =  Utils.getprefb("noquests", false);
    public static boolean dungeonkeyalert =  Utils.getprefb("dungeonkeyalert", true);
    public static double sfxwhipvol = Utils.getprefd("sfxwhipvol", 0.9);
    public static boolean showarchvector =  Utils.getprefb("showarchvector", false);
    //public static boolean showcddelta =  Utils.getprefb("showcddelta", false);
    public static boolean disabledrinkhotkey =  Utils.getprefb("disabledrinkhotkey", false);
    public static boolean autologout =  Utils.getprefb("autologout", false);
    public static int combatkeys =  Utils.getprefi("combatkeys", 0);
    public static boolean logcombatactions =  Utils.getprefb("logcombatactions", false);
    public static boolean autopickmussels =  Utils.getprefb("autopickmussels", false);
    public static boolean autopickclay =  Utils.getprefb("autopickclay", false);
    //public static boolean studydeskalerts =  Utils.getprefb("studydeskalerts", false);
    public static boolean confirmmagic =  Utils.getprefb("confirmmagic", true);
    public static boolean altfightui =  Utils.getprefb("altfightui", false);
    public static boolean combshowkeys =  Utils.getprefb("combshowkeys", true);
    public static boolean combaltopenings =  Utils.getprefb("combaltopenings", true);
    public static boolean studyhist =  Utils.getprefb("studyhist", false);
    public static boolean studybuff =  Utils.getprefb("studybuff", false);
    public static int zkey =  Utils.getprefi("zkey", KeyEvent.VK_Z);
    public static boolean disableterrainsmooth =  Utils.getprefb("disableterrainsmooth", false);
    public static boolean disableelev =  Utils.getprefb("disableelev", false);
    public static String treeboxclr =  Utils.getpref("treeboxclr", "D7FF00");
    public static String chatalert =  Utils.getpref("chatalert", "Null");
    public static String AlertChannel =  Utils.getpref("AlertChannel", "Null");
    public static String discordchannel =  Utils.getpref("discordchannel", "Null");
    public static boolean discordchat =  Utils.getprefb("discordchat", false);
    public static String discordbotkey =  Utils.getpref("discordbotkey", "Null");
    public static boolean highlightpots = Utils.getprefb("highlightpots", false);
    public static boolean abandonrightclick = Utils.getprefb("abandonrightclick", false);
    public static boolean DropEntrails = Utils.getprefb("DropEntrails", false);
    public static boolean DropIntestines = Utils.getprefb("DropIntestines", false);
    public static boolean StarveAlert = Utils.getprefb("StarveAlert", true);
    public static boolean DropMeat = Utils.getprefb("DropMeat", false);
    public static boolean DropBones = Utils.getprefb("DropBones", false);
    public static boolean bonsai = Utils.getprefb("bonsai", false);
    public static int fontsizechat = Utils.getprefi("fontsizechat", 14);
    public static int curiotimetarget = Utils.getprefi("curiotimetarget", 1440);
    public static int statgainsize = Utils.getprefi("statgainsize", 1);
    public static int caveinduration = Utils.getprefi("caveinduration", 1);
    public static boolean colorfulcaveins = Utils.getprefb("colorfulcaveins", false);
    public static boolean fontaa = Utils.getprefb("fontaa", false);
    public static boolean usefont = Utils.getprefb("usefont", false);
    public static String font = Utils.getpref("font", "SansSerif");
    public static int fontadd = Utils.getprefi("fontadd", 0);
    public static boolean proximityaggro = Utils.getprefb("proximityaggro", false);
    public static boolean proximityaggropvp = Utils.getprefb("proximityaggropvp", false);
    public static boolean disablemenugrid = Utils.getprefb("disablemenugrid", false);
    public static boolean pf = false;
    public static String playerposfile;
    public static byte[] authck = null;
    public static String prefspec = "hafen";
    //public static String version;
    public static String version = Utils.getpref("version", "1.0");
    public static String newversion;
    public static String Changelog;
    public static String[] Changelogarray;
    public static StringBuffer Changelogbuffer;
    public static String gitrev;
    public static boolean fepmeter = Utils.getprefb("fepmeter", true);
    public static boolean hungermeter = Utils.getprefb("hungermeter", true);
    public static boolean leechdrop = Utils.getprefb("leechdrop", false);
    public static boolean hideTrees = Utils.getprefb("hideTrees", true);
    public static boolean hideTarKilns = Utils.getprefb("hideTarKilns", true);
    public static boolean hideSmelters = Utils.getprefb("hideSmelters", true);
    public static boolean hidemoths = Utils.getprefb("hidemoths", false);
    public static boolean hideCrops = Utils.getprefb("hideCrops", true);
    public static boolean hideWalls = Utils.getprefb("hideWalls", true);
    public static boolean hideanimals = Utils.getprefb("hideanimals", false);
    public static boolean hideWagons = Utils.getprefb("hideWagons", false);
    public static boolean hideHouses = Utils.getprefb("hideHouses", false);
    public static boolean hideBushes = Utils.getprefb("hideBushes", true);
    public static boolean hideDFrames = Utils.getprefb("hideDFrames", false);
    public static boolean hideDCatchers = Utils.getprefb("hideDCatchers", false);
    public static boolean showoverlay = Utils.getprefb("showoverlay", true);
    public static boolean disableAllAnimations = Utils.getprefb("disableAllAnimations", false);
    public static boolean hidecalendar = Utils.getprefb("hidecalendar", false);
    public static int hidered = Utils.getprefi("hidered", 0);
    public static int hidegreen = Utils.getprefi("hidegreen", 0);
    public static int hideblue = Utils.getprefi("hideblue", 255);
    public static int smatdangerred = Utils.getprefi("smatdangerred",255);
    public static int smatdangergreen= Utils.getprefi("smatdangergreen",0);
    public static int smatdangerblue = Utils.getprefi("smatdangerblue",0);
    public static int smatSupportsred = Utils.getprefi("smatSupportsred",0);
    public static int smatSupportsgreen= Utils.getprefi("smatSupportsgreen",255);
    public static int smatSupportsblue = Utils.getprefi("smatSupportsblue",0);
    public static int cRackred = Utils.getprefi("cRackred",255);
    public static int cRackgreen = Utils.getprefi("cRackgreen",0);
    public static int cRackblue = Utils.getprefi("cRackblue",255);
    public static String confid = "ArdClient";
    public static final boolean isUpdate;

    public static int AmbientRed = Utils.getprefi("AmbientRed",200);
    public static int AmbientGreen = Utils.getprefi("AmbientGreen",200);
    public static int AmbientBlue = Utils.getprefi("AmbientBlue",200);
    public static int DiffuseRed = Utils.getprefi("DiffuseRed",200);
    public static int DiffuseGreen = Utils.getprefi("DiffuseGreen",200);
    public static int DiffuseBlue = Utils.getprefi("DiffuseBlue",200);
    public static int SpecRed = Utils.getprefi("SpecRed",255);
    public static int SpecGreen = Utils.getprefi("SpecGreen",255);
    public static int SpecBlue = Utils.getprefi("SpecBlue",255);

    public static boolean chatalarm = Utils.getprefb("chatalarm", true);
    public static double chatalarmvol = Utils.getprefd("chatalarmvol", 0.8);
    public static boolean timersalarm = Utils.getprefb("timersalarm", false);
    public static boolean timersort = Utils.getprefb("timersort", true);
    public static double timersalarmvol = Utils.getprefd("timersalarmvol", 0.8);
    public static String alarmunknownplayer = Utils.getpref("alarmunknownplayer", "sfx/OhShitItsAGuy");
    public static double alarmunknownvol = Utils.getprefd("alarmunknownvol", 0.32);
    public static String alarmredplayer = Utils.getpref("alarmredplayer", "sfx/Siren");
    public static double alarmredvol = Utils.getprefd("alarmredvol", 0.32);
    public static String alarmforagable = Utils.getpref("alarmforagable", "sfx/awwyeah");
    public static double alarmonforagablesvol = Utils.getprefd("alarmonforagablesvol", 0.8D);
    public static String alarmbear = Utils.getpref("alarmbear", "sfx/BearRoar");
    public static double alarmbearsvol = Utils.getprefd("alarmbearsvol", 0.7D);
    public static String alarmlynx = Utils.getpref("alarmlynx", "sfx/lynx");
    public static double alarmlynxvol = Utils.getprefd("alarmlynxvol", 0.32);
    public static String alarmadder = Utils.getpref("alarmadder", "sfx/DangerNoodle");
    public static double alarmaddervol = Utils.getprefd("alarmaddervol", 0.32);
    public static String alarmwalrus = Utils.getpref("alarmwalrus", "sfx/Walrus");
    public static double alarmwalrusvol = Utils.getprefd("alarmwalrusvol", 0.32);
    public static String alarmseal = Utils.getpref("alarmseal", "sfx/seal");
    public static double alarmsealvol = Utils.getprefd("alarmsealvol", 0.32);
    public static String alarmtroll = Utils.getpref("alarmtroll", "sfx/troll");
    public static double alarmtrollvol = Utils.getprefd("alarmtrollvol", 0.8D);
    public static String alarmmammoth = Utils.getpref("alarmmammoth", "sfx/mammoth");
    public static double alarmmammothvol = Utils.getprefd("alarmmammothvol", 0.32);
    public static String alarmeagle = Utils.getpref("alarmeagle", "sfx/EagleScreech");
    public static double alarmeaglevol = Utils.getprefd("alarmeaglevol", 0.32);
    public static String alarmdoomed = Utils.getpref("alarmdoomed", "sfx/Doomed");
    public static String alarmwball = Utils.getpref("alarmwball", "sfx/WreckingBall");
    public static double alarmwballvol = Utils.getprefd("alarmwballvol", 1.0D);
    public static String alarmswag = Utils.getpref("alarmswag", "sfx/Swag");
    public static double alarmlocresvol = Utils.getprefd("alarmlocresvol", 0.8D);
    public static String alarmeyeball = Utils.getpref("alarmeyeball", "sfx/OhShitItsAGuy");
    public static double alarmeyeballvol = Utils.getprefd("alarmeyeballvol", 1.0D);
    public static String alarmnidbane = Utils.getpref("alarmnidbane", "sfx/GhostBusters");
    public static double alarmnidbanevol = Utils.getprefd("alarmnidbanevol", 1.0);
    public static String alarmdungeon = Utils.getpref("alarmdungeon", "sfx/Zelda");
    public static double alarmdungeonvol = Utils.getprefd("alarmdungeonvol", 1.0D);
    public static String alarmbeaverdungeon = Utils.getpref("alarmbeaverdungeon", "sfx/BeaverDungeon");
    public static String alarmsiege = Utils.getpref("alarmsiege", "sfx/siege");
    public static double alarmbramvol = Utils.getprefd("alarmbramvol", 1.0D);
    public static String alarmstudy = Utils.getpref("alarmstudy","sfx/Study");
    public static double studyalarmvol = Utils.getprefd("studyalarmvol", 0.8);
    public static String cleavesfx = Utils.getpref("cleavesfx","sfx/oof");
    public static double cleavesoundvol = Utils.getprefd("cleavesoundvol", 0.8);
    public static String attackedsfx = Utils.getpref("attackedsfx","None");
    public static double attackedvol = Utils.getprefd("attackedvol", 0.8);

    public static HashMap<String,Boolean> curioslist = null;







    public final static String chatfile = "chatlog.txt";
    public static PrintWriter chatlog = null;

    public final static HashMap<String, CheckListboxItem> boulders = new HashMap<String, CheckListboxItem>(15) {{
        put("alabaster", new CheckListboxItem("Alabaster"));
        put("basalt", new CheckListboxItem("Basalt"));
        put("schist", new CheckListboxItem("Schist"));
        put("dolomite", new CheckListboxItem("Dolomite"));
        put("gneiss", new CheckListboxItem("Gneiss"));
        put("granite", new CheckListboxItem("Granite"));
        put("porphyry", new CheckListboxItem("Porphyry"));
        put("quartz", new CheckListboxItem("Quartz"));
        put("limestone", new CheckListboxItem("Limestone"));
        put("sandstone", new CheckListboxItem("Sandstone"));
        put("cinnabar", new CheckListboxItem("Cinnabar"));
        put("feldspar", new CheckListboxItem("Feldspar"));
        put("marble", new CheckListboxItem("Marble"));
        put("flint", new CheckListboxItem("Flint"));
        put("hornblende", new CheckListboxItem("Hornblende"));
        put("olivine", new CheckListboxItem("Olivine"));
        put("apatite", new CheckListboxItem("Apatite"));
    }};

    public final static HashMap<String, CheckListboxItem> bushes = new HashMap<String, CheckListboxItem>(23) {{
        put("arrowwood", new CheckListboxItem("Arrowwood"));
        put("crampbark", new CheckListboxItem("Crampbark"));
        put("sandthorn", new CheckListboxItem("Sandthorn"));
        put("blackberrybush", new CheckListboxItem("Blackberry"));
        put("dogrose", new CheckListboxItem("Dogrose"));
        put("spindlebush", new CheckListboxItem("Spindlebush"));
        put("blackcurrant", new CheckListboxItem("Blackcurrant"));
        put("elderberrybush", new CheckListboxItem("Elderberry"));
        put("teabush", new CheckListboxItem("Tea"));
        put("blackthorn", new CheckListboxItem("Blackthorn"));
        put("gooseberrybush", new CheckListboxItem("Gooseberry"));
        put("tibast", new CheckListboxItem("Tibast"));
        put("bogmyrtle", new CheckListboxItem("Bogmyrtle"));
        put("hawthorn", new CheckListboxItem("Hawthorn"));
        put("tundrarose", new CheckListboxItem("Tundrarose"));
        put("boxwood", new CheckListboxItem("Boxwood"));
        put("holly", new CheckListboxItem("Hollyberry"));
        put("woodbine", new CheckListboxItem("Fly Woodbine"));
        put("bsnightshade", new CheckListboxItem("Bittersweet Nightshade"));
        put("raspberrybush", new CheckListboxItem("Raspberry"));
        put("caprifole", new CheckListboxItem("Caprifole"));
        put("redcurrant", new CheckListboxItem("Redcurrant"));
        put("gorse", new CheckListboxItem("Gorse"));
    }};

    public final static HashMap<String, CheckListboxItem> trees = new HashMap<String, CheckListboxItem>(53) {{
        put("chastetree", new CheckListboxItem("Chaste Tree"));
        put("silverfir", new CheckListboxItem("Silver Fir"));
        put("treeheath", new CheckListboxItem("Heath Tree"));
        put("terebinth", new CheckListboxItem("Terebinth"));
        put("lotetree", new CheckListboxItem("Lote Tree"));
        put("sorbtree", new CheckListboxItem("Sorb Tree"));
        put("alder", new CheckListboxItem("Alder"));
        put("corkoak", new CheckListboxItem("Corkoak"));
        put("plumtree", new CheckListboxItem("Plum Tree"));
        put("juniper", new CheckListboxItem("Juniper"));
        put("crabappletree", new CheckListboxItem("Crabapple"));
        put("kingsoak", new CheckListboxItem("King's Oak"));
        put("oak", new CheckListboxItem("Oak"));
        put("walnuttree", new CheckListboxItem("Walnut Tree"));
        put("birdcherrytree", new CheckListboxItem("Birdcherry Tree"));
        put("larch", new CheckListboxItem("Larch"));
        put("poplar", new CheckListboxItem("Poplar"));
        put("whitebeam", new CheckListboxItem("Whitebeam"));
        put("appletree", new CheckListboxItem("Apple Tree"));
        put("cypress", new CheckListboxItem("Cypress"));
        put("buckthorn", new CheckListboxItem("Buckthorn"));
        put("laurel", new CheckListboxItem("Laurel"));
        put("ash", new CheckListboxItem("Ash"));
        put("elm", new CheckListboxItem("Elm"));
        put("rowan", new CheckListboxItem("Rowan"));
        put("willow", new CheckListboxItem("Willow"));
        put("cedar", new CheckListboxItem("Cedar"));
        put("linden", new CheckListboxItem("Linden"));
        put("olivetree", new CheckListboxItem("Olive Tree"));
        put("aspen", new CheckListboxItem("Aspen"));
        put("fir", new CheckListboxItem("Fir"));
        put("baywillow", new CheckListboxItem("Baywillow"));
        put("goldenchain", new CheckListboxItem("Goldenchain"));
        put("peartree", new CheckListboxItem("Pear Tree"));
        put("sallow", new CheckListboxItem("Sallow"));
        put("yew", new CheckListboxItem("Yew"));
        put("cherry", new CheckListboxItem("Cherry"));
        put("maple", new CheckListboxItem("Maple"));
        put("beech", new CheckListboxItem("Beech"));
        put("chestnuttree", new CheckListboxItem("Chestnut Tree"));
        put("hazel", new CheckListboxItem("Hazel"));
        put("spruce", new CheckListboxItem("Spruce"));
        put("hornbeam", new CheckListboxItem("Hornbeam"));
        put("oldtrunk", new CheckListboxItem("Mirkwood Log"));
        put("conkertree", new CheckListboxItem("Conker Tree"));
        put("mulberry", new CheckListboxItem("Mulberry"));
        put("sweetgum", new CheckListboxItem("Sweetgum"));
        put("pine", new CheckListboxItem("Pine"));
        put("birch", new CheckListboxItem("Birch"));
        put("planetree", new CheckListboxItem("Plane Tree"));
        put("quincetree", new CheckListboxItem("Quince"));
        put("almondtree", new CheckListboxItem("Almond"));
        put("persimmontree", new CheckListboxItem("Persimmon"));
        put("mayflower", new CheckListboxItem("Mayflower"));
        put("towercap", new CheckListboxItem("Towercaps"));
    }};

    public final static HashMap<String, CheckListboxItem> icons = new HashMap<String, CheckListboxItem>(56) {{
        put("dandelion", new CheckListboxItem("Dandelion"));
        put("chantrelle", new CheckListboxItem("Chantrelle"));
        put("blueberry", new CheckListboxItem("Blueberry"));
        put("rat", new CheckListboxItem("Rat"));
        put("chicken", new CheckListboxItem("Chicken"));
        put("chick", new CheckListboxItem("Chick"));
        put("spindlytaproot", new CheckListboxItem("Spindly Taproot"));
        put("stingingnettle", new CheckListboxItem("Stinging Nettle"));
        put("dragonfly", new CheckListboxItem("Dragonfly"));
        put("toad", new CheckListboxItem("Toad"));
        put("bram", new CheckListboxItem("Battering Ram"));
        put("magpie", new CheckListboxItem("Magpie"));
        put("mistletoe", new CheckListboxItem("Mistletoe"));
        put("firefly", new CheckListboxItem("Firefly"));
        put("rowboat", new CheckListboxItem("Rowboat"));
        put("dugout", new CheckListboxItem("Dugout"));
        put("arrow", new CheckListboxItem("Arrow"));
        put("boarspear", new CheckListboxItem("Boar Spear"));
        put("cavemoth", new CheckListboxItem("Cave Moth"));
        put("frog", new CheckListboxItem("Frog"));
        put("wagon", new CheckListboxItem("Wagon"));
        put("wheelbarrow", new CheckListboxItem("Wheelbarrow"));
        put("cart", new CheckListboxItem("Cart"));
        put("wball", new CheckListboxItem("Wrecking Ball"));
        put("windweed", new CheckListboxItem("Wild Windsown Weed"));
        put("mussels", new CheckListboxItem("Mussels"));
        put("mallard", new CheckListboxItem("Duck"));
        put("ladybug", new CheckListboxItem("Ladybug"));
        put("silkmoth", new CheckListboxItem("Silkmoth"));
        put("hedgehog", new CheckListboxItem("Hedgehog"));
        put("squirrel", new CheckListboxItem("Squirrel"));
        put("rabbit", new CheckListboxItem("Rabbit"));
        put("lingon", new CheckListboxItem("Lingonberries"));
        put("grub", new CheckListboxItem("Grub"));
        put("yellowfoot", new CheckListboxItem("Yellowfoot"));
        put("chives", new CheckListboxItem("Chives"));
        put("rustroot", new CheckListboxItem("Rustroot"));
        put("boostspeed", new CheckListboxItem("Speed Boost"));
        put("adder", new CheckListboxItem("Adder"));
        put("crab", new CheckListboxItem("Crab"));
        put("clover", new CheckListboxItem("Clover"));
        put("ladysmantle", new CheckListboxItem("Lady's Mantle"));
        put("grasshopper", new CheckListboxItem("Grasshopper"));
        put("irrbloss", new CheckListboxItem("Irrlight"));
        put("opiumdragon", new CheckListboxItem("Opium Dragon"));
        put("snapdragon", new CheckListboxItem("Uncommon Snapdragon"));
        put("cattail", new CheckListboxItem("Cattail"));
        put("forestsnail", new CheckListboxItem("Forest Snail"));
        put("forestlizard", new CheckListboxItem("Forest Lizard"));
        put("greenkelp", new CheckListboxItem("Green Kelp"));
        put("waterstrider", new CheckListboxItem("Water Strider"));
        put("frogspawn", new CheckListboxItem("Frog Spawn"));
        put("oyster",new CheckListboxItem("Oysters"));
        put("jellyfish", new CheckListboxItem("Jellyfish"));
        put("clay-gray",new CheckListboxItem("Gray Clay"));
        put("bat", new CheckListboxItem("Bats"));
        put("stagbeetle", new CheckListboxItem("Stagbeetles"));
        put("monarchbutterfly", new CheckListboxItem("Monarch Butterfly"));
    }};

    public final static HashMap<String, CheckListboxItem> flowermenus = new HashMap<String, CheckListboxItem>(33) {{
        put("Pick", new CheckListboxItem("Pick", Resource.BUNDLE_FLOWER));
        put("Drink", new CheckListboxItem("Drink", Resource.BUNDLE_FLOWER));
        put("Harvest", new CheckListboxItem("Harvest", Resource.BUNDLE_FLOWER));
        put("Eat", new CheckListboxItem("Eat", Resource.BUNDLE_FLOWER));
        put("Split", new CheckListboxItem("Split", Resource.BUNDLE_FLOWER));
        put("Kill", new CheckListboxItem("Kill", Resource.BUNDLE_FLOWER));
        put("Slice", new CheckListboxItem("Slice", Resource.BUNDLE_FLOWER));
        put("Pluck", new CheckListboxItem("Pluck", Resource.BUNDLE_FLOWER));
        put("Empty", new CheckListboxItem("Empty", Resource.BUNDLE_FLOWER));
        put("Clean", new CheckListboxItem("Clean", Resource.BUNDLE_FLOWER));
        put("Skin", new CheckListboxItem("Skin", Resource.BUNDLE_FLOWER));
        put("Flay", new CheckListboxItem("Flay", Resource.BUNDLE_FLOWER));
        put("Collect bones", new CheckListboxItem("Collect bones", Resource.BUNDLE_FLOWER));
        put("Crumble", new CheckListboxItem("Crumble", Resource.BUNDLE_FLOWER));
        put("Butcher", new CheckListboxItem("Butcher", Resource.BUNDLE_FLOWER));
        put("Giddyup!", new CheckListboxItem("Giddyup!", Resource.BUNDLE_FLOWER));
        put("Break", new CheckListboxItem("Break", Resource.BUNDLE_FLOWER));
        put("Cargo", new CheckListboxItem("Cargo", Resource.BUNDLE_FLOWER));
        put("Sleep", new CheckListboxItem("Sleep", Resource.BUNDLE_FLOWER));
        put("Shear wool", new CheckListboxItem("Shear wool", Resource.BUNDLE_FLOWER));
        put("Harvest wax", new CheckListboxItem("Harvest wax", Resource.BUNDLE_FLOWER));
        put("Slice up", new CheckListboxItem("Slice up", Resource.BUNDLE_FLOWER));
        put("Chip stone", new CheckListboxItem("Chip stone", Resource.BUNDLE_FLOWER));
        put("Study", new CheckListboxItem("Study", Resource.BUNDLE_FLOWER));
        put("Peer into", new CheckListboxItem("Peer into", Resource.BUNDLE_FLOWER));
        put("Tether horse", new CheckListboxItem("Tether horse", Resource.BUNDLE_FLOWER));
        put("Wring neck", new CheckListboxItem("Wring neck", Resource.BUNDLE_FLOWER));
        put("Open", new CheckListboxItem("Open", Resource.BUNDLE_FLOWER));
        put("Inspect", new CheckListboxItem("Inspect", Resource.BUNDLE_FLOWER));
        put("Slaughter", new CheckListboxItem("Slaughter", Resource.BUNDLE_FLOWER));
        put("Crack open", new CheckListboxItem("Crack Open",Resource.BUNDLE_FLOWER));
        put("Collect coal", new CheckListboxItem("Collect Coal",Resource.BUNDLE_FLOWER));
        put("Pick leaf", new CheckListboxItem("Pick Leaf",Resource.BUNDLE_FLOWER));
        put("Ride", new CheckListboxItem("Ride",Resource.BUNDLE_FLOWER));
    }};



    public final static HashMap<String, CheckListboxItem> curiolist = new HashMap<String, CheckListboxItem>(37) {{
        put("Bar of Soap", new CheckListboxItem("Bar of Soap"));
        put("Barkboat", new CheckListboxItem("Barkboat"));
        put("Batwing Necklace", new CheckListboxItem("Batwing Necklace"));
        put("Beast Unborn", new CheckListboxItem("Beast Unborn"));
        put("Blacksmith's Bauble", new CheckListboxItem("Blacksmith's Bauble"));
        put("Chiming Bluebell", new CheckListboxItem("Chiming Bluebell"));
        put("Bronze Steed", new CheckListboxItem("Bronze Steed"));
        put("Cigar", new CheckListboxItem("Cigar"));
        put("Deep Sea Atavism", new CheckListboxItem("Deep Sea Atavism"));
        put("Feather Duster", new CheckListboxItem("Feather Duster"));
        put("Feather Trinket", new CheckListboxItem("Feather Trinket"));
        put("Bouquet of Flowers", new CheckListboxItem("Bouquet of Flowers"));
        put("Fossil Collection", new CheckListboxItem("Fossil Collection"));
        put("Glue Troll", new CheckListboxItem("Glue Troll"));
        put("Golden Cat", new CheckListboxItem("Golden Cat"));
        put("Golden Tooth", new CheckListboxItem("Golden Tooth"));
        put("Grand Haruspex", new CheckListboxItem("Grand Haruspex"));
        put("Great Wax Seal", new CheckListboxItem("Great Wax Seal"));
        put("Ivory Figurine", new CheckListboxItem("Ivory Figurine"));
        put("Mirkwood Offering", new CheckListboxItem("Mirkwood Offering"));
        put("Onion Braid", new CheckListboxItem("Onion Braid"));
        put("Porcelain Doll", new CheckListboxItem("Porcelain Doll"));
        put("Seer's Bowl", new CheckListboxItem("Seer's Bowl"));
        put("Seer's Bones", new CheckListboxItem("Seer's Bones"));
        put("Seer's Spindle", new CheckListboxItem("Seer's Spindle"));
        put("Seer's Stones", new CheckListboxItem("Seer's Stones"));
        put("Seer's Tealeaves", new CheckListboxItem("Seer's Tealeaves"));
        put("Shiny Marbles", new CheckListboxItem("Shiny Marbles"));
        put("Silken Ribbon", new CheckListboxItem("Silken Ribbon"));
        put("Silver Rose", new CheckListboxItem("Silver Rose"));
        put("Snow Globe", new CheckListboxItem("Snow Globe"));
        put("Stained Glass Heart", new CheckListboxItem("Stained Glass Heart"));
        put("Straw Doll", new CheckListboxItem("Straw Doll"));
        put("Stuffed Bear", new CheckListboxItem("Stuffed Bear"));
        put("Tafl Board", new CheckListboxItem("Tafl Board"));
        put("Tiny Abacus", new CheckListboxItem("Tiny Abacus"));
        put("Uncrushed Husk", new CheckListboxItem("Uncrushed Husk"));
        put("Easter Egg", new CheckListboxItem("Easter Egg"));
    }};

    public final static Map<String, Tex> additonalicons = new HashMap<String, Tex>(17) {{
        put("gfx/terobjs/vehicle/bram", Resource.loadtex("gfx/icons/bram"));
        put("gfx/kritter/toad/toad", Resource.loadtex("gfx/icons/toad"));
        put("gfx/terobjs/vehicle/rowboat", Resource.loadtex("gfx/icons/rowboat"));
        put("gfx/terobjs/vehicle/dugout",Resource.loadtex("gfx/icons/dugout"));
        put("gfx/kritter/chicken/chicken", Resource.loadtex("gfx/icons/deadhen"));
        put("gfx/kritter/chicken/rooster", Resource.loadtex("gfx/icons/deadrooster"));
        put("gfx/kritter/rabbit/rabbit", Resource.loadtex("gfx/icons/deadrabbit"));
        put("gfx/kritter/hedgehog/hedgehog", Resource.loadtex("gfx/icons/deadhedgehog"));
        put("gfx/kritter/squirrel/squirrel", Resource.loadtex("gfx/icons/deadsquirrel"));
        put("gfx/terobjs/items/arrow", Resource.loadtex("gfx/icons/arrow"));
        put("gfx/terobjs/items/boarspear", Resource.loadtex("gfx/icons/arrow"));
        put("gfx/kritter/frog/frog", Resource.loadtex("gfx/icons/frog"));
        put("gfx/terobjs/vehicle/wagon", Resource.loadtex("gfx/icons/wagon"));
        put("gfx/terobjs/vehicle/wheelbarrow", Resource.loadtex("gfx/icons/wheelbarrow"));
        put("gfx/terobjs/vehicle/cart", Resource.loadtex("gfx/icons/cart"));
        put("gfx/terobjs/vehicle/wreckingball", Resource.loadtex("gfx/icons/wball"));
        put("gfx/kritter/nidbane/nidbane", Resource.loadtex("gfx/icons/spooky"));
    }};

    public final static HashMap<String, CheckListboxItem> alarmitems = new HashMap<String, CheckListboxItem>(51) {{
        put("gfx/terobjs/herbs/flotsam", new CheckListboxItem("Peculiar Flotsam"));
        put("gfx/terobjs/herbs/chimingbluebell", new CheckListboxItem("Chiming Bluebell"));
        put("gfx/terobjs/herbs/edelweiss", new CheckListboxItem("Edelweiß"));
        put("gfx/terobjs/herbs/bloatedbolete", new CheckListboxItem("Bloated Bolete"));
        put("gfx/terobjs/herbs/glimmermoss", new CheckListboxItem("Glimmermoss"));
        put("gfx/terobjs/herbs/camomile", new CheckListboxItem("Camomile"));
        put("gfx/terobjs/herbs/clay-cave", new CheckListboxItem("Cave Clay"));
        put("gfx/terobjs/herbs/mandrake", new CheckListboxItem("Mandrake Root"));
        put("gfx/terobjs/herbs/clay-gray", new CheckListboxItem("Gray Clay"));
        put("gfx/terobjs/herbs/dandelion", new CheckListboxItem("Dandelion"));
        put("gfx/terobjs/herbs/chantrelle", new CheckListboxItem("Chantrelle"));
        put("gfx/terobjs/herbs/blueberry", new CheckListboxItem("Blueberry"));
        put("gfx/terobjs/herbs/strawberry", new CheckListboxItem("Strawberry"));
        put("gfx/kritter/rat/rat", new CheckListboxItem("Rat"));
        put("gfx/kritter/chicken/chicken", new CheckListboxItem("Chicken"));
        put("gfx/kritter/chick/chick", new CheckListboxItem("Chick"));
        put("gfx/terobjs/herbs/spindlytaproot", new CheckListboxItem("Spindly Taproot"));
        put("gfx/terobjs/herbs/stingingnettle", new CheckListboxItem("Stinging Nettle"));
        put("gfx/kritter/dragonfly/dragonfly", new CheckListboxItem("Dragonfly"));
        put("gfx/kritter/toad/toad", new CheckListboxItem("Toad"));
        put("gfx/kritter/frog/frog", new CheckListboxItem("Frog"));
        put("gfx/terobjs/herbs/windweed", new CheckListboxItem("Wild Windsown Weed"));
        put("gfx/terobjs/herbs/mussels", new CheckListboxItem("Mussels"));
        put("gfx/kritter/mallard/mallard", new CheckListboxItem("Duck"));
        put("gfx/kritter/ladybug/ladybug", new CheckListboxItem("Ladybug"));
        put("gfx/kritter/silkmoth/silkmoth", new CheckListboxItem("Silkmoth"));
        put("gfx/kritter/hedgehog/hedgehog", new CheckListboxItem("Hedgehog"));
        put("gfx/kritter/squirrel/squirrel", new CheckListboxItem("Squirrel"));
        put("gfx/kritter/rabbit/rabbit", new CheckListboxItem("Rabbit"));
        put("gfx/terobjs/herbs/lingon", new CheckListboxItem("Lingonberries"));
        put("gfx/kritter/grub/grub", new CheckListboxItem("Grub"));
        put("gfx/terobjs/herbs/yellowfoot", new CheckListboxItem("Yellowfoot"));
        put("gfx/terobjs/herbs/chives", new CheckListboxItem("Chives"));
        put("gfx/terobjs/herbs/rustroot", new CheckListboxItem("Rustroot"));
        put("gfx/kritter/crab/crab", new CheckListboxItem("Crab"));
        put("gfx/terobjs/herbs/clover", new CheckListboxItem("Clover"));
        put("gfx/terobjs/herbs/ladysmantle", new CheckListboxItem("Lady's Mantle"));
        put("gfx/kritter/grasshopper/grasshopper", new CheckListboxItem("Grasshopper"));
        put("gfx/kritter/irrbloss/irrbloss", new CheckListboxItem("Irrlight"));
        put("gfx/kritter/opiumdragon/opiumdragon", new CheckListboxItem("Opium Dragon"));
        put("gfx/terobjs/herbs/snapdragon", new CheckListboxItem("Uncommon Snapdragon"));
        put("gfx/terobjs/herbs/cattail", new CheckListboxItem("Cattail"));
        put("gfx/kritter/forestsnail/forestsnail", new CheckListboxItem("Forest Snail"));
        put("gfx/kritter/forestlizard/forestlizard", new CheckListboxItem("Forest Lizard"));
        put("gfx/terobjs/herbs/greenkelp", new CheckListboxItem("Green Kelp"));
        put("gfx/terobjs/herbs/yarrow", new CheckListboxItem("Yarrow"));
        put("gfx/terobjs/herbs/candleberry", new CheckListboxItem("Candleberry"));
        put("gfx/terobjs/herbs/oyster", new CheckListboxItem("Oysters"));
        put("gfx/kritter/jellyfish/jellyfish", new CheckListboxItem("Jellyfish"));
        put("gfx/terobjs/herbs/seashell",new CheckListboxItem("Rainbowshell"));
        put("gfx/terobjs/herbs/giantpuffball", new CheckListboxItem("Giant Puff Ball"));
    }};

    public final static Set<String> locres = new HashSet<String>(Arrays.asList(
            "gfx/terobjs/saltbasin",
            "gfx/terobjs/abyssalchasm",
            "gfx/terobjs/windthrow",
            "gfx/terobjs/icespire",
            "gfx/terobjs/woodheart",
            "gfx/terobjs/lilypadlotus",
            "gfx/terobjs/fairystone",
            "gfx/terobjs/jotunmussel",
            "gfx/terobjs/guanopile",
            "gfx/terobjs/geyser",
            "gfx/terobjs/claypit",
            "gfx/terobjs/caveorgan",
            "gfx/terobjs/crystalpatch"));

    public final static Set<String> mineablesStone = new HashSet<String>(Arrays.asList(
            "gneiss",
            "basalt",
            "cinnabar",
            "dolomite",
            "feldspar",
            "flint",
            "granite",
            "hornblende",
            "limestone",
            "marble",
            "porphyry",
            "quartz",
            "sandstone",
            "schist",
            "blackcoal",
            "mica",
            "apatite",
            "sodalite",
            "gabbro",
            "kyanite",
            "zincspar",
            "fluorospar",
            "microlite",
            "olivine",
            "soapstone",
            "orthoclase",
            "alabaster"
    ));

    public final static Set<String> mineablesOre = new HashSet<String>(Arrays.asList(
            "cassiterite",
            "chalcopyrite",
            "malachite",
            "ilmenite",
            "limonite",
            "hematite",
            "magnetite",
            "peacockore"
    ));

    public final static Set<String> mineablesOrePrecious = new HashSet<String>(Arrays.asList(
            "galena",
            "argentite",
            "hornsilver",
            "petzite",
            "sylvanite",
            "nagyagite"
    ));

    public final static Set<String> mineablesCurios = new HashSet<String>(Arrays.asList(
            "catgold",
            "petrifiedshell",
            "strangecrystal"
    ));

    public final static HashMap<String, CheckListboxItem> disableanim = new HashMap<String, CheckListboxItem>(6) {{
        put("gfx/terobjs/beehive", new CheckListboxItem("Beehives"));
        put("gfx/terobjs/pow", new CheckListboxItem("Fires"));
        put("gfx/terobjs/stockpile-trash", new CheckListboxItem("Full trash stockpiles"));
        put("/idle", new CheckListboxItem("Idle animals"));
        put("gfx/terobjs/steelcrucible", new CheckListboxItem("Steel Crucible"));
        put("gfx/terobjs/villageidol", new CheckListboxItem("Village Idol"));
        put("gfx/terobjs/tarkiln",new CheckListboxItem("Tar Kilns"));
        put("gfx/terobjs/oven",new CheckListboxItem("Ovens"));
        put("gfx/terobjs/smelter",new CheckListboxItem("Smelters"));
        put("gfx/terobjs/arch/visflag", new CheckListboxItem("Visitor Flags"));
        put("gfx/terobjs/flagpole", new CheckListboxItem("Flag Poles"));
    }};

    public final static HashMap<String, CheckListboxItem> disableshiftclick = new HashMap<String, CheckListboxItem>(7){{
       put("steelcrucible", new CheckListboxItem("Steel Crucibles"));
        put("ttub", new CheckListboxItem("Tanning Tub"));
        put("smelter", new CheckListboxItem("Smelters"));
        put("oven", new CheckListboxItem("Ovens"));
        put("kiln", new CheckListboxItem("Kilns"));
        put("htable", new CheckListboxItem("Herb Tables"));
        put("cupboard", new CheckListboxItem("Cupboards"));
    }};

    public final static HashMap<String, String> alarms = new HashMap<String,String>(33){{
        put("None","None");
        put("Pony Alarm","sfx/alarmpony");
        put("Awwwwww Yeah","sfx/awwyeah");
        put("Bear Roar","sfx/BearRoar");
        put("Jcoles Beaver Dungeon","sfx/BeaverDungeon");
        put("JColes Danger Noodle","sfx/DangerNoodle");
        put("DaveyJones","sfx/DaveyJones");
        put("Ding","sfx/Ding");
        put("Doomed","sfx/Doomed");
        put("EagleScreech","sfx/EagleScreech");
        put("GhostBusters","sfx/GhostBusters");
        put("Gold","sfx/gold");
        put("Oof!","sfx/oof");
        put("lynx","sfx/lynx");
        put("mammoth","sfx/mammoth");
        put("Oh Shit!","sfx/OhShit");
        put("JColes OhFuckItsAGuy","sfx/OhShitItsAGuy");
        put("Enemy Siren","sfx/redenemy");
        put("Arf Arf","sfx/seal");
        put("Siege Warning","sfx/siege");
        put("Silver","sfx/silver");
        put("Unknown Player Siren","sfx/Siren");
        put("Female Scream","sfx/Scream");
        put("Study Ding","sfx/Study");
        put("Swag!","sfx/Swag");
        put("Thank youuuuuuu","sfx/thankyourick");
        put("Timer alarm","sfx/timer");
        put("Troll in the dungeon!","sfx/troll");
        put("JColes Wallllllrus","sfx/Walrus");
        put("Wrecking Ball!","sfx/WreckingBall");
        put("Zelda Secret","sfx/Zelda");
        put("Trumpets","sfx/trumpets");
        put("No Dick!","sfx/nodick");
        put("Snek!","sfx/snek");
    }};

    public final static HashMap<String, String[]> cures = new HashMap<String, String[]>(20) {{
        put("paginae/wound/antburn", new String[]{
                "gfx/invobjs/herbs/yarrow"
        });
        put("paginae/wound/blunttrauma", new String[]{
                "gfx/invobjs/toadbutter",
                "gfx/invobjs/leech",
                "gfx/invobjs/gauze",
                "gfx/invobjs/hartshornsalve",
                "gfx/invobjs/camomilecompress",
                "gfx/invobjs/opium"
        });
        put("paginae/wound/bruise", new String[]{
                "gfx/invobjs/leech"
        });
        put("paginae/wound/concussion", new String[]{
                "gfx/invobjs/coldcompress",
                "gfx/invobjs/opium"
        });
        put("paginae/wound/cruelincision", new String[]{
                "gfx/invobjs/gauze",
                "gfx/invobjs/stitchpatch",
                "gfx/invobjs/rootfill"
        });
        put("paginae/wound/deepcut", new String[]{
                "gfx/invobjs/gauze",
                "gfx/invobjs/stingingpoultice",
                "gfx/invobjs/rootfill",
                "gfx/invobjs/herbs/waybroad",
                "gfx/invobjs/honeybroadaid"
        });
        put("paginae/wound/fellslash", new String[]{
                "gfx/invobjs/gauze"
        });
        put("paginae/wound/nicksnknacks", new String[]{
                "gfx/invobjs/herbs/yarrow",
                "gfx/invobjs/honeybroadaid"
        });
        put("paginae/wound/punchsore", new String[]{
                "gfx/invobjs/mudointment",
                "gfx/invobjs/opium"
        });
        put("paginae/wound/scrapesncuts", new String[]{
                "gfx/invobjs/herbs/yarrow",
                "gfx/invobjs/mudointment",
                "gfx/invobjs/honeybroadaid"
        });
        put("paginae/wound/severemauling", new String[]{
                "gfx/invobjs/hartshornsalve",
                "gfx/invobjs/opium"
        });
        put("paginae/wound/swollenbump", new String[]{
                "gfx/invobjs/coldcompress",
                "gfx/invobjs/leech",
                "gfx/invobjs/stingingpoultice"
        });
        put("paginae/wound/unfaced", new String[]{
                "gfx/invobjs/toadbutter",
                "gfx/invobjs/leech",
                "gfx/invobjs/mudointment",
                "gfx/invobjs/kelpcream"
        });
        put("paginae/wound/wretchedgore", new String[]{
                "gfx/invobjs/stitchpatch"
        });
        put("paginae/wound/blackeye", new String[]{
                "gfx/invobjs/hartshornsalve",
                "gfx/invobjs/honeybroadaid",
                "gfx/invobjs/toadbutter"
        });
        put("paginae/wound/bladekiss", new String[]{
                "gfx/invobjs/gauze",
                "gfx/invobjs/toadbutter"
        });
        put("paginae/wound/somethingbroken", new String[]{
                "gfx/invobjs/camomilecompress"
        });
        put("paginae/wound/infectedsore", new String[]{
                "gfx/invobjs/camomilecompress",
                "gfx/invobjs/soapbar",
                "gfx/invobjs/opium"
        });
        put("paginae/wound/nastylaceration", new String[]{
                "gfx/invobjs/stitchpatch",
                "gfx/invobjs/toadbutter"
        });
        put("paginae/wound/sealfinger", new String[]{
                "gfx/invobjs/hartshornsalve",
                "gfx/invobjs/kelpcream"
        });
        put("paginae/wound/coalcough", new String[]{
                "gfx/invobjs/opium"
        });
        put("paginae/wound/beesting", new String[]{
                "gfx/invobjs/kelpcream"
        });
    }};

    public static final Map<Long, Pair<String, String>> gridIdsMap = new HashMap<>(58000);

    static {
        Utils.loadprefchklist("disableanim", Config.disableanim);
        Utils.loadprefchklist("alarmitems", Config.alarmitems);
        Utils.loadprefchklist("disableshiftclick", Config.disableshiftclick);
        Utils.loadCurioList();
        String p;
        if ((p = getprop("haven.authck", null)) != null)
            authck = Utils.hex2byte(p);
      /*  try {
            InputStream in = ErrorHandler.class.getResourceAsStream("/buildinfo");
            try {
                if (in != null) {
                    java.util.Scanner s = new java.util.Scanner(in);
                    String[] binfo = s.next().split(",");
                   // version = binfo[0];
                    gitrev = binfo[1];
                }
            } finally {
                in.close();
            }
        } catch (Exception e) {}*/
        loadBuildVersion();
        isUpdate = (!version.equals(newversion)) || !getFile("changelog.txt").exists();
        if (isUpdate) {
         //   Config.version = newversion;
            Utils.setpref("version",newversion);
        }
        try {
            InputStream in = ErrorHandler.class.getResourceAsStream("/CHANGELOG");
            try {
                if (in != null) {
                    java.util.Scanner s = new java.util.Scanner(in);
                    Changelogbuffer = new StringBuffer();
                    while(s.hasNextLine()) {
                        Changelogbuffer.append("-");
                        Changelogbuffer.append(s.nextLine());
                    }
                    //  }
                    Changelog = Changelogbuffer.toString();
                }
            } finally {
                in.close();
            }
        } catch (Exception e) {}

        // populate grid ids map
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader("grid_ids.txt"));
            String line;
            while ((line = reader.readLine()) != null) {
                String[] tknzed = line.split(",");
                try {
                    gridIdsMap.put(Long.parseLong(tknzed[2]), new Pair<>(tknzed[0], tknzed[1]));
                } catch (NumberFormatException nfe) {
                }
            }
        } catch (IOException e){
            e.printStackTrace();
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) { // ignored
                }
            }
        }

        loadLogins();
    }

        private static void loadBuildVersion() {
            InputStream in = Config.class.getResourceAsStream("/buildinfo");
            try {
                try {
                    if(in != null) {
                        Properties info = new Properties();
                        info.load(in);
                        newversion = info.getProperty("version");
                        gitrev = info.getProperty("git-rev");
                    }
                } finally {
                    if (in != null) { in.close(); }
                }
            } catch(IOException e) {
                throw(new Error(e));
            }
        }
    public static File getFile(String name) {
        return new File(HOMEDIR, name);
    }
    public static String loadFile(String name) {
        InputStream inputStream = getFSStream(name);
        if(inputStream == null) {
            inputStream = getJarStream(name);
        }
        return getString(inputStream);
    }

    public static String loadJarFile(String name) {
        return getString(getJarStream(name));
    }

    public static String loadFSFile(String name) {
        return getString(getFSStream(name));
    }

    private static InputStream getFSStream(String name) {
        InputStream inputStream = null;
        File file = Config.getFile(name);
        if(file.exists() && file.canRead()) {
            try {
                inputStream = new FileInputStream(file);
            } catch (FileNotFoundException ignored) {
            }
        }
        return inputStream;
    }

    private static InputStream getJarStream(String name) {
        if(name.charAt(0) != '/') {
            name = '/' + name;
        }
        return Config.class.getResourceAsStream(name);
    }

    private static String getString(InputStream inputStream) {
        if(inputStream != null) {
            try {
                return Utils.stream2str(inputStream);
            } catch (Exception ignore) {
            } finally {
                try {inputStream.close();} catch (IOException ignored) {}
            }
        }
        return null;
    }

    public static void saveFile(String name, String data){
        File file = Config.getFile(name);
        boolean exists = file.exists();
        if(!exists){
            try {
                //noinspection ResultOfMethodCallIgnored
                String parent = file.getParent();
                new File(parent).mkdirs();
                exists = file.createNewFile();
            } catch (IOException ignored) {}
        }
        if(exists && file.canWrite()){
            PrintWriter out = null;
            try {
                out = new PrintWriter(file);
                out.print(data);
            } catch (FileNotFoundException ignored) {
            } finally {
                if (out != null) {
                    out.close();
                }
            }
        }
    }

    private static int getint(String name, int def) {
        String val = getprop(name, null);
        if(val == null)
            return(def);
        return(Integer.parseInt(val));
    }

    private static URL geturl(String name, String def) {
        String val = getprop(name, def);
        if(val.equals(""))
            return(null);
        try {
            return(new URL(val));
        } catch(java.net.MalformedURLException e) {
            throw(new RuntimeException(e));
        }
    }



    private static void loadLogins() {
        try {
            String loginsjson = Utils.getpref("logins", null);
            if (loginsjson == null)
                return;
            JSONArray larr = new JSONArray(loginsjson);
            for (int i = 0; i < larr.length(); i++) {
                JSONObject l = larr.getJSONObject(i);
                logins.add(new LoginData(l.get("name").toString(), l.get("pass").toString()));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void saveLogins() {
        try {
            List<String> larr = new ArrayList<String>();
            for (LoginData ld : logins) {
                String ldjson = new JSONObject(ld, new String[] {"name", "pass"}).toString();
                larr.add(ldjson);
            }
            String jsonobjs = "";
            for (String s : larr)
                jsonobjs += s + ",";
            if (jsonobjs.length() > 0)
                jsonobjs = jsonobjs.substring(0, jsonobjs.length()-1);
            Utils.setpref("logins", "[" + jsonobjs + "]");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static URL geturl(String url) {
        if (url.equals(""))
            return null;
        try {
            return new URL(url);
        } catch(java.net.MalformedURLException e) {
            throw(new RuntimeException(e));
        }
    }

    private static void usage(PrintStream out) {
        out.println("usage: haven.jar [OPTIONS] [SERVER[:PORT]]");
        out.println("Options include:");
        out.println("  -h                 Display this help");
        out.println("  -d                 Display debug text");
        out.println("  -P                 Enable profiling");
        out.println("  -G                 Enable GPU profiling");
        out.println("  -p FILE            Write player position to a memory mapped file");
        out.println("  -U URL             Use specified external resource URL");
        out.println("  -A AUTHSERV[:PORT] Use specified authentication server");
        out.println("  -u USER            Authenticate as USER (together with -C)");
        out.println("  -C HEXCOOKIE       Authenticate with specified hex-encoded cookie");
    }

    public static void cmdline(String[] args) {
        PosixArgs opt = PosixArgs.getopt(args, "hdPGp:U:r:A:u:C:");
        if (opt == null) {
            usage(System.err);
            System.exit(1);
        }
        for (char c : opt.parsed()) {
            switch (c) {
                case 'h':
                    usage(System.out);
                    System.exit(0);
                    break;
                case 'd':
                    dbtext = true;
                    break;
                case 'P':
                    profile = true;
                    break;
                case 'G':
                    profilegpu = true;
                    break;
                case 'A':
                    int p = opt.arg.indexOf(':');
                    if (p >= 0) {
                        authserv = opt.arg.substring(0, p);
                        authport = Integer.parseInt(opt.arg.substring(p + 1));
                    } else {
                        authserv = opt.arg;
                    }
                    break;
                case 'U':
                    try {
                        resurl = new URL(opt.arg);
                    } catch (java.net.MalformedURLException e) {
                        System.err.println(e);
                        System.exit(1);
                    }
                    break;
                case 'u':
                    authuser = opt.arg;
                    break;
                case 'C':
                    authck = Utils.hex2byte(opt.arg);
                    break;
                case 'p':
                    playerposfile = opt.arg;
                    break;
            }
        }
        if (opt.rest.length > 0) {
            int p = opt.rest[0].indexOf(':');
            if (p >= 0) {
                defserv = opt.rest[0].substring(0, p);
                mainport = Integer.parseInt(opt.rest[0].substring(p + 1));
            } else {
                defserv = opt.rest[0];
            }
        }
    }

    static {
        Console.setscmd("stats", (cons, args) -> dbtext = Utils.parsebool(args[1]));
        Console.setscmd("profile", (cons, args) -> {
            if (args[1].equals("none") || args[1].equals("off")) {
                profile = profilegpu = false;
            } else if (args[1].equals("cpu")) {
                profile = true;
            } else if (args[1].equals("gpu")) {
                profilegpu = true;
            } else if (args[1].equals("all")) {
                profile = profilegpu = true;
            }
        });
    }
}
