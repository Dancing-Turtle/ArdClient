package haven;

import com.google.common.flogger.FluentLogger;
import haven.overlays.OverlayData;
import haven.sloth.gob.Alerted;
import haven.sloth.gob.Deleted;
import haven.sloth.gob.Hidden;
import haven.sloth.gob.Movable;
import haven.sloth.io.HighlightData;

import java.awt.*;
import java.util.ArrayList;
import java.util.Optional;

//import haven.sloth.gob.*;
//import haven.sloth.io.HighlightData;
//import haven.sloth.io.Storage;

/**
 * A global view of all our settings. This should at some point cover all of Ape.
 *
 * Missing settings:
 *  Graphics
 *   more-flav-objs
 *   tree-scale
 *   simple-crops
 *   colorful-cave-dust
 *   gob-path-color
 *   animal-path-color
 *   hide-color
 *   target-color
 *   show-transition-tiles
 *   show-skybox
 *   no-gob-overlay
 *   dont-delete-grids
 *   skybox-range
 *   map-grids-radius
 *   show-grass
 *  And every other section
 *
 *
 * TODO: There should be a distinction between global settings and character-specific
 */
public class DefSettings {
	private static final FluentLogger logger = FluentLogger.forEnclosingClass();
	public static final Settings global = new Settings("config.ini");
	//Settings in 'session' don't save, only valid for the lifetime of the session
	public static final Settings session = new Settings("");

	//Sections & settings for each one, for 'global'
	//Custom stuff
	public static final IndirSetting<Boolean> SKIPLOADING = new IndirSetting<>(global, "graphics.skip-loading");             //[Bool] Skip loading screens
	public static final IndirSetting<Boolean> SHOWFLAVOBJS = new IndirSetting<>(global, "graphics.flav-objs-show");          //[Bool] Don't show flav objs
	public static final IndirSetting<Boolean> SYMMETRICOUTLINES = new IndirSetting<>(global, "graphics.outlines-symmetric"); //[Bool] Make outlines symmetric (bolder)
	public static final IndirSetting<Integer> SHADOWSQUALITY = new IndirSetting<>(global, "graphics.shadows-quality");       //[Int ] Shadow quality level [0-7], 4 is default
	public static final IndirSetting<Integer> SHADOWSIZE = new IndirSetting<>(global, "graphics.shadows-size");              //[Int ] Shadow size, 750 is default
	public static final IndirSetting<Integer> MSAALEVEL = new IndirSetting<>(global, "graphics.msaa-level");                 //[Int ] MSAA level [1-8]
	public static final IndirSetting<Boolean> WIREFRAMEMODE = new IndirSetting<>(global, "graphics.wireframe-mode");         //[Bool] Display everything as wireframe
	public static final IndirSetting<Boolean> WEATHER = new IndirSetting<>(global, "graphics.weather-show");                 //[Bool] Show weather or not
	public static final IndirSetting<Boolean> ANIMATIONS = new IndirSetting<>(global, "graphics.animations-show");           //[Bool] Turn animations orr or on
	public static final IndirSetting<Boolean> SHOWMAP = new IndirSetting<>(global, "graphics.map-show");                     //[Bool] Toggle mapgrid on/off
	public static final IndirSetting<Boolean> SHOWGOBS = new IndirSetting<>(global, "graphics.gobs-show");                   //[Bool] Toggle gobs on/off
	public static final IndirSetting<Boolean> NIGHTVISION = new IndirSetting<>(global, "graphics.nightvision");              //[Bool] Toggle nightvision
	public static final IndirSetting<Boolean> FLATWORLD = new IndirSetting<>(global, "graphics.flatworld");                  //[Bool] Toggle flatworld (haven)
	public static final IndirSetting<Boolean> SHOWTRANTILES = new IndirSetting<>(global, "graphics.show-transition-tiles");  //[Bool] Toggle transition tiles
	public static final IndirSetting<Boolean> COLORFULDUST = new IndirSetting<>(global, "graphics.colorful-cave-dust");      //[Bool] Toggle colorful cave dust
	public static final IndirSetting<Integer> DRAWGRIDRADIUS = new IndirSetting<>(global, "graphics.map-grid-draw-radius");  //[Int ] Map grid draw radius, default -> 2
	public static final IndirSetting<Color> NVAMBIENTCOL = new IndirSetting<>(global, "graphics.nightvision-ambient");       //[RBGA] Nightvision ambient color
	public static final IndirSetting<Color> NVDIFFUSECOL = new IndirSetting<>(global, "graphics.diffuse-color");             //[RGBA] Nightvision diffuse color
	public static final IndirSetting<Color> NVSPECCOC = new IndirSetting<>(global, "graphics.specular-color");               //[RGBA] Nightvision specular color
	public static final IndirSetting<Color> DEEPWATERCOL = new IndirSetting<>(global, "graphics.deepwater-color");           //[RGBA] Deep Ocean Water Color
	public static final IndirSetting<Color> ALLWATERCOL = new IndirSetting<>(global, "graphics.watercol-color");             //[RGBA] Color of all other water color
	public static final IndirSetting<Color> CURIOTARGET = new IndirSetting<>(global, "graphics.curiotarget-color");             //[RGBA] Color of all other water color
	public static final IndirSetting<Color> CURIOHIGH = new IndirSetting<>(global, "graphics.curiohigh-color");             //[RGBA] Color of all other water color
	public static final IndirSetting<Color> CURIOLOW = new IndirSetting<>(global, "graphics.curiolow-color");             //[RGBA] Color of all other water color
	public static final IndirSetting<Boolean> DARKMODE = new IndirSetting<>(global, "graphics.dark-mode");                   //[Bool] Toggle dark mode, overrides Nightvision
	public static final IndirSetting<Boolean> QUICKMENU = new IndirSetting<>(global, "gameplay.quick-flowermenu");           //[Bool] Toggle quick flowermenu
	public static final IndirSetting<Boolean> QUICKERMENU = new IndirSetting<>(global, "gameplay.quicker-flowermenu");           //[Bool] Toggle quick-er flowermenu will autoselect items if so selected without even popping up flowermenu
	public static final IndirSetting<Boolean> AMBERMENU = new IndirSetting<>(global, "gameplay.amber-flowermenu");           //[Bool] Use amber flowermenu
	public static final IndirSetting<Boolean> BUGGEDMENU = new IndirSetting<>(global, "gameplay.bugged-flowermenu");         //[Bool] Whether not flowermenu should close on clicks outside of it
	public static final IndirSetting<Boolean> SIMPLECROPS = new IndirSetting<>(global, "gameplay.simple-crops");             //[Bool] Toggle simple crop meshes
	public static final IndirSetting<Boolean> SHOWCROPSTAGE = new IndirSetting<>(global, "gameplay.show-crop-stage");        //[Bool] Toggle crop stages off/on
	public static final IndirSetting<Boolean> SHOWGOBHP = new IndirSetting<>(global, "gameplay.show-gob-hp");                //[Bool] Toggle gob hp visibility
	public static final IndirSetting<Boolean> SHOWGOBPATH =  new IndirSetting<>(global, "gameplay.show-gob-path");            //[Bool] Toggle gob path rendering
	public static final IndirSetting<Boolean> SHOWPLAYERPATH =  new IndirSetting<>(global, "gameplay.show-player-path");            //[Bool] Toggle player path rendering
	public static final IndirSetting<Color> PLAYERPATHCOL = new IndirSetting<>(global, "gameplay.player-path-color");               //[RGBA] Path color for gobs, only for unknown gobs
	public static final IndirSetting<Color> GOBPATHCOL = new IndirSetting<>(global, "gameplay.gob-path-color");               //[RGBA] Path color for gobs, only for unknown gobs
	public static final IndirSetting<Color> VEHPATHCOL = new IndirSetting<>(global, "gameplay.vehicle-path-color");           //[RGBA] Path color for vehicle                //[Bool] Toggles Gob audio
	public static final IndirSetting<Color> MMPATHCOL = new IndirSetting<>(global, "gameplay.minimap.path-col");              //[RGBA] Path color for minimap
	public static final IndirSetting<Boolean> SHOWANIMALPATH =  new IndirSetting<>(global, "gameplay.show-animal-path");      //[Bool] Toggle animal path rendering
	public static final IndirSetting<Color> ANIMALPATHCOL =  new IndirSetting<>(global, "gameplay.animal-path-color");        //[RBGA] Path color for animals
	public static final IndirSetting<Boolean> SHOWANIMALRADIUS = new IndirSetting<>(global, "gameplay.show-animal-radius");  //[Bool] Toggle radius on dangerous animals
	public static final IndirSetting<Boolean> SHOWFARMRADIUS = new IndirSetting<>(global, "gameplay.show-farming-radius");   //[Bool] Toggle radius on farming equipment (beehive/trough)
	public static final IndirSetting<Boolean> SHOWHITBOX = new IndirSetting<>(global, "gameplay.show-hitbox");               //[Bool] Toggle hitbox squares
	public static final IndirSetting<Boolean> SHOWHIDDEN = new IndirSetting<>(global, "gameplay.show-hidden");               //[Bool] Toggle hidden squares
	public static final IndirSetting<Color> HIDDENCOLOR = new IndirSetting<>(global, "gameplay.hidden-color");               //[RGBA] Color of hidden squares
	public static final IndirSetting<Color> GUIDESCOLOR = new IndirSetting<>(global, "gameplay.guides-color");               //[RGBA] Color of guide lines (grids and boundboxes)
	public static final IndirSetting<Color> TROUGHCOLOR = new IndirSetting<>(global, "gameplay.trough-color");               //[RGBA] Color trough radii circles
	public static final IndirSetting<Color> BEEHIVECOLOR = new IndirSetting<>(global, "gameplay.beehive-color");             //[RGBA] Color beehive radii circles
	public static final IndirSetting<Color> ANIMALDANGERCOLOR = new IndirSetting<>(global, "gameplay.danger-color");         //[RGBA] Color of animal radii circles
	public static final IndirSetting<Color> SUPPORTDANGERCOLOR = new IndirSetting<>(global, "gameplay.support-color");       //[RGBA] Color of mine suport radii circles
	public static final IndirSetting<Color> CHEESERACKMISSINGCOLOR = new IndirSetting<>(global, "gameplay.cheeserack-color");//[RGBA] Color of cheese racks missing trays
	public static final IndirSetting<Color> GARDENPOTDONECOLOR = new IndirSetting<>(global, "gameplay.gardenpot-color");//[RGBA] Color of cheese racks missing trays
	public static final IndirSetting<Color> ERRORTEXTCOLOR = new IndirSetting<>(global, "gameplay.errortext-color");//[RGBA] Color of error messages displayed
	public static final IndirSetting<Boolean> SHOWQUALITY = new IndirSetting<>(global, "gameplay.show-item-quality");        //[Bool] Toggle item quality
	public static final IndirSetting<Boolean> SHOWWEAR = new IndirSetting<>(global, "gameplay.show-item-wear");              //[Bool] Toggle item wear bar
	public static final IndirSetting<Boolean> SHOWCMETER = new IndirSetting<>(global, "gameplay.show-content-meter");        //[Bool] Toggle item content bar
	public static final IndirSetting<Boolean> AUTOEQUIP = new IndirSetting<>(global, "gameplay.item-equip-on-right-click");  //[Bool] Toggle equiping items on right click if possible
	public static final IndirSetting<Boolean> SHOWPCLAIM = new IndirSetting<>(global, "gameplay.show-pclaim");               //[Bool] Toggle pclaims
	public static final IndirSetting<Boolean> SHOWVCLAIM = new IndirSetting<>(global, "gameplay.show-vclaim");               //[Bool] Toggle vclaims
	public static final IndirSetting<Boolean> SHOWKCLAIM = new IndirSetting<>(global, "gameplay.show-kclaim");               //[Bool] Toggle kingdom claims
	public static final IndirSetting<Integer> BADKIN = new IndirSetting<>(global, "gameplay.bad-kin-color");                 //[Int] Bad kin color, default: 2 (red)
	public static final IndirSetting<Boolean> ALWAYSLONGTIP = new IndirSetting<>(global, "gameplay.always-show-longtip");    //[Bool] Toggle only longtips on item tooltips
	public static final IndirSetting<Boolean> SHOWFPS = new IndirSetting<>(global, "gameplay.show-fps");                     //[Bool] Toggle FPS counter
	public static final IndirSetting<Boolean> SHOWHOVERTOOLTIPS = new IndirSetting<>(global, "gameplay.show-hover-tooltip");  //[Bool] Toggle hover tooltips
	public static final IndirSetting<Boolean> SHOWMMMARKERS = new IndirSetting<>(global, "gameplay.show-minimap-markers");    //[Bool] Toggle minimap icons
	public static final IndirSetting<Boolean> SMALLMMMARKERS = new IndirSetting<>(global, "gameplay.small-minimap-markers");  //[Bool] Toggle SMALL minimap icons
	public static final IndirSetting<Boolean> AUTOTRACK = new IndirSetting<>(global, "gameplay.auto-turn-on-tracking");       //[Bool] Toggle turning on tracking on login
	public static final IndirSetting<Boolean> AUTOCRIME = new IndirSetting<>(global, "gameplay.auto-turn-on-crime");          //[Bool] Toggle turning on criminal acts on login
	public static final IndirSetting<Boolean> SHOWHALO = new IndirSetting<>(global, "gameplay.show-gob-halo");                //[Bool] Toggle halo on human gobs
	public static final IndirSetting<Boolean> SHOWHALOONHEARTH = new IndirSetting<>(global, "gameplay.show-gob-halo-hearth"); //[Bool] Toggle halo on human gobs
	public static final IndirSetting<Boolean> WATERDROPITEMCTRL = new IndirSetting<>(global, "gameplay.drop-item-on-water-with-ctrl"); //[Bool] Toggle the need to hold ctrl to drop items while over water
	public static final IndirSetting<Boolean> FORAGEANIMALS = new IndirSetting<>(global, "gameplay.small-animaling-foraging"); //[Bool] Consider small animals with the forage keybind
	public static final IndirSetting<Integer> PATHFINDINGTIER = new IndirSetting<>(global, "gameplay.pathfinding-tier");	    //[Int] 0, 1, 2 = { perfect, medium, fast }
	public static final IndirSetting<Boolean> SHOWFKBELT = new IndirSetting<>(global, "belt.fk.show");                       //[Bool] Toggle F key belt
	public static final IndirSetting<Integer> FKBELTPAGE = new IndirSetting<>(global, "belt.fk.page");                       //[Int] Page F key belt is on
	public static final IndirSetting<String> FKBELTSTYLE = new IndirSetting<>(global, "belt.fk.style");                      //[String] F key belt style
	public static final IndirSetting<Boolean> FKBELTLOCK =  new IndirSetting<>(global, "belt.fk.locked");                      //[Bool] Prevent removing icons off F key belt
	public static final IndirSetting<Boolean> SHOWNPBELT = new IndirSetting<>(global, "belt.np.show");                       //[Bool] Toggle NumPad belt
	public static final IndirSetting<Integer> NPBELTPAGE = new IndirSetting<>(global, "belt.np.page");                       //[Int] Page F key belt is on
	public static final IndirSetting<String> NPBELTSTYLE = new IndirSetting<>(global, "belt.np.style");                      //[String] F key belt style
	public static final IndirSetting<Boolean> NPBELTLOCK =  new IndirSetting<>(global, "belt.np.locked");                      //[Bool] Prevent removing icons off numpad key belt
	public static final IndirSetting<Boolean> SHOWNBELT = new IndirSetting<>(global, "belt.n.show");                         //[Bool] Toggle Number belt
	public static final IndirSetting<Integer> NBELTPAGE = new IndirSetting<>(global, "belt.n.page");                         //[Int] Page F key belt is on
	public static final IndirSetting<String> NBELTSTYLE = new IndirSetting<>(global, "belt.n.style");                        //[String] F key belt style
	public static final IndirSetting<Boolean> NBELTLOCK =  new IndirSetting<>(global, "belt.n.locked");                        //[Bool] Prevent removing icons off n key belt
	public static final IndirSetting<Boolean> MMSHOWGRID = new IndirSetting<>(global, "minimap.show-grid");                  //[Bool] Toggle minimap grid
	public static final IndirSetting<Boolean> MMSHOWVIEW = new IndirSetting<>(global, "minimap.show-view");                  //[Bool] Toggle minimap view box
	public static final IndirSetting<String> CAMERA = new IndirSetting<>(global, "camera.camera-type");                      //[String] Camera type, default: Ortho
	public static final IndirSetting<Boolean> FREECAMREXAXIS = new IndirSetting<>(global, "camera.free.reverse-x-axis");     //[Bool] Reverse free cam x axis on drag or not
	public static final IndirSetting<Boolean> FREECAMREYAXIS = new IndirSetting<>(global, "camera.free.reverse-y-axis");     //[Bool] Reverse free cam y axis on drag or not
	public static final IndirSetting<Boolean> FREECAMLOCKELAV = new IndirSetting<>(global, "camera.free.lock-elevation");    //[Bool] Lock freecam elevation angle
	public static final IndirSetting<Integer> TIMERVOLUME = new IndirSetting<>(global, "audio.timer-volume");                //[Int] Timer volume
	public static final IndirSetting<Boolean> NOGOBAUDIO = new IndirSetting<>(global, "audio.no-gob-audio");

	//Debug
	public static final IndirSetting<Boolean> DEBUG = new IndirSetting<>(global, "system.debug");

	//Themes
	public static final IndirSetting<String[]> THEMES = new IndirSetting<>(session, "theme.themes");
	public static final IndirSetting<String> HUDTHEME = new IndirSetting<>(global, "theme.hud");                             //[String] Hud theme to use, default: sloth
	public static final IndirSetting<Color> WNDCOL = new IndirSetting<>(global, new IndirSetting.IndirFormatKey("theme.%s.wnd.color", HUDTHEME));
	public static final IndirSetting<Color> BTNCOL = new IndirSetting<>(global, new IndirSetting.IndirFormatKey("theme.%s.button.color", HUDTHEME));
	public static final IndirSetting<Color> TXBCOL = new IndirSetting<>(global, new IndirSetting.IndirFormatKey("theme.%s.textbox.color", HUDTHEME));
	public static final IndirSetting<Color> SLIDERCOL = new IndirSetting<>(global, new IndirSetting.IndirFormatKey("theme.%s.slider.color", HUDTHEME));
	private static final String
			WNDCOLFMT = "theme.%s.wnd.color",
			BTNCOLFMT = "theme.%s.button.color",
			BTNTEXTCOLFMT = "theme.%s.buttontext.color",
			TXBCOLFMT = "theme.%s.textbox.color",
			SLIDERCOLFMT = "theme.%s.slider.color";

	//Unused
	public static final IndirSetting<Integer> TREESCALE = new IndirSetting<>(global, "graphics.tree-scale");                 //[Int ] Tree scaling value [1-16], 1 is default
	public static final IndirSetting<Boolean> MOREFLAVOBJS = new IndirSetting<>(global, "graphics.more-flavor-objs");        //[Bool] Toggle more flavor objects
	public static final IndirSetting<Boolean> SHOWSKYBOX = new IndirSetting<>(global, "graphics.show-skybox");               //[Bool] Toggle skybox
	public static final IndirSetting<Integer> SKYBOXRANGE = new IndirSetting<>(global, "graphics.skybox-range");             //[Int ] Skybox range, default -> 1500?
	public static final IndirSetting<Boolean> KEEPGRIDS = new IndirSetting<>(global, "graphics.dont-delete-grids");          //[Bool] Don't delete map grids
	public static final IndirSetting<Boolean> AUTOHEARTH = new IndirSetting<>(global, "gameplay.autohearth");                //[Bool] Toggle auto hearth on players
	public static final IndirSetting<Boolean> SMARTAIM = new IndirSetting<>(global, "gameplay.smartaim");                    //[Bool] Toggle smart aim for archery
	public static final IndirSetting<String> MENUTHEME = new IndirSetting<>(global, "theme.menu");                           //[String] Menu theme to use, default: default


	//Session based
	public static final IndirSetting<Boolean> PAUSED = new IndirSetting<>(session, "session.paused");
	public static final IndirSetting<Boolean> SHOWGRID = new IndirSetting<>(session, "session.show-grid");

	/**
	 * Checks out settings nad saves them if they are dirty
	 */
	public static void checkForDirty() {
		if (global.dirty())
			global.save();
	}

	/**
	 * Ensure certain settings exist
	 */
	public static void init() {
		global.load();
		//Custom Graphics
		SKIPLOADING.ensure(true);
		SHOWFLAVOBJS.ensure(false);
		SKIPLOADING.ensure(true);
		SHOWFLAVOBJS.ensure(true);
		SYMMETRICOUTLINES.ensure(false);
		SHADOWSQUALITY.ensure(4);
		SHADOWSIZE.ensure(750);
		MSAALEVEL.ensure(4);
		WIREFRAMEMODE.ensure(false);
		WEATHER.ensure(true);
		ANIMATIONS.ensure(true);
		SHOWMAP.ensure(true);
		SHOWGOBS.ensure(true);
		NIGHTVISION.ensure(false);
		NVAMBIENTCOL.ensure(new Color(200,200,200));
		NVDIFFUSECOL.ensure(new Color(200,200,200));
		NVSPECCOC.ensure(new Color(255,255,255));
		DEEPWATERCOL.ensure(new Color(0, 0, 0));
		ALLWATERCOL.ensure(new Color(0, 16, 48));
		CURIOTARGET.ensure(new Color(0,255,0));
		CURIOHIGH.ensure(new Color(255,0,0));
		CURIOLOW.ensure(new Color(255,182,193));
		FLATWORLD.ensure(false);
		SHOWTRANTILES.ensure(true);
		TREESCALE.ensure(1);
		COLORFULDUST.ensure(true);
		MOREFLAVOBJS.ensure(false);
		SHOWSKYBOX.ensure(false);
		SKYBOXRANGE.ensure(15000);
		KEEPGRIDS.ensure(false);
		DRAWGRIDRADIUS.ensure(2);
		DARKMODE.ensure(false);
		//Gameplay
		QUICKMENU.ensure(false);
		QUICKERMENU.ensure(false);
		AMBERMENU.ensure(true);
		BUGGEDMENU.ensure(false);
		SIMPLECROPS.ensure(false);
		SHOWCROPSTAGE.ensure(false);
		AUTOHEARTH.ensure(false);
		SMARTAIM.ensure(false);
		SHOWGOBHP.ensure(false);
		SHOWPLAYERPATH.ensure(false);
		SHOWGOBPATH.ensure(false);
		PLAYERPATHCOL.ensure(new Color(255, 255, 255, 80));
		GOBPATHCOL.ensure(new Color(255, 0, 0, 168));
		VEHPATHCOL.ensure(new Color(111, 255, 138, 168));
		MMPATHCOL.ensure(Color.magenta);
		SHOWANIMALPATH.ensure(false);
		ANIMALPATHCOL.ensure(new Color(144, 255, 171, 146));
		SHOWANIMALRADIUS.ensure(false);
		SHOWFARMRADIUS.ensure(false);
		SHOWHITBOX.ensure(false);
		SHOWHIDDEN.ensure(true);
		HIDDENCOLOR.ensure(Color.RED);
		GUIDESCOLOR.ensure(Color.WHITE);
		TROUGHCOLOR.ensure(new Color(0,0,255,80));
		BEEHIVECOLOR.ensure(new Color(255,255,0,80));
		ANIMALDANGERCOLOR.ensure(new Color(255,0,0,80));
		SUPPORTDANGERCOLOR.ensure(new Color(0,255,0,80));
		CHEESERACKMISSINGCOLOR.ensure(Color.GREEN);
		GARDENPOTDONECOLOR.ensure(Color.BLACK);
		ERRORTEXTCOLOR.ensure(Color.red);
		SHOWQUALITY.ensure(true);
		SHOWWEAR.ensure(true);
		SHOWCMETER.ensure(true);
		AUTOEQUIP.ensure(true);
		SHOWPCLAIM.ensure(false);
		SHOWVCLAIM.ensure(false);
		SHOWKCLAIM.ensure(false);
		BADKIN.ensure(2);
		ALWAYSLONGTIP.ensure(true);
		SHOWFPS.ensure(true);
		SHOWHOVERTOOLTIPS.ensure(false);
		AUTOTRACK.ensure(true);
		AUTOCRIME.ensure(false);
		SMALLMMMARKERS.ensure(false);
		SHOWMMMARKERS.ensure(true);
		SHOWHALO.ensure(false);
		SHOWHALOONHEARTH.ensure(false);
		WATERDROPITEMCTRL.ensure(true);
		FORAGEANIMALS.ensure(true);
		PATHFINDINGTIER.ensure(1);
		DEBUG.ensure(false);
		//Belts
		SHOWFKBELT.ensure(true);
		FKBELTPAGE.ensure(0);
		FKBELTSTYLE.ensure(BeltWnd.Style.HORIZONTAL.toString());
		SHOWNPBELT.ensure(true);
		NPBELTPAGE.ensure(0);
		NPBELTSTYLE.ensure(BeltWnd.Style.HORIZONTAL.toString());
		SHOWNBELT.ensure(true);
		NBELTPAGE.ensure(0);
		NBELTSTYLE.ensure(BeltWnd.Style.HORIZONTAL.toString());
		FKBELTLOCK.ensure(false);
		NBELTLOCK.ensure(false);
		NPBELTLOCK.ensure(false);
		//Minimap
		MMSHOWGRID.ensure(false);
		MMSHOWVIEW.ensure(false);
		//Cameras
		CAMERA.ensure("sortho");
		FREECAMREXAXIS.ensure(false);
		FREECAMREYAXIS.ensure(false);
		FREECAMLOCKELAV.ensure(false);
		//Figure out our themes
		HUDTHEME.ensure("ardclient");
		MENUTHEME.ensure("default");
		{
			final Color slothc = new Color(85, 144, 87, 228);
			global.ensure(String.format(WNDCOLFMT, "sloth"), slothc);
			final ArrayList<String> huds = new ArrayList<>();
			huds.add("Default");
			global.ensure(String.format(WNDCOLFMT, "Default"), Color.WHITE);
			global.ensure(String.format(BTNCOLFMT, "Default"), Color.WHITE);
			global.ensure(String.format(TXBCOLFMT, "Default"), Color.WHITE);
			global.ensure(String.format(SLIDERCOLFMT, "Default"), Color.WHITE);
			huds.add("sloth");
			global.ensure(String.format(WNDCOLFMT, "sloth"), Color.WHITE);
			global.ensure(String.format(BTNCOLFMT, "sloth"), Color.WHITE);
			global.ensure(String.format(TXBCOLFMT, "sloth"), Color.WHITE);
			global.ensure(String.format(SLIDERCOLFMT, "sloth"), Color.WHITE);
			huds.add("ardclient");
			global.ensure(String.format(WNDCOLFMT, "ardclient"), Color.WHITE);
			global.ensure(String.format(BTNCOLFMT, "ardclient"), Color.WHITE);
			global.ensure(String.format(TXBCOLFMT, "ardclient"), Color.WHITE);
			global.ensure(String.format(SLIDERCOLFMT, "ardclient"), Color.WHITE);
			//For options window
			THEMES.set(huds.toArray(new String[0]));
		}

		//Audio
		TIMERVOLUME.ensure(1000);
		NOGOBAUDIO.ensure(false);

		//Session based globals
		PAUSED.ensure(false);
		SHOWGRID.ensure(false);



		//Piggy backing off this to init some other important settings
		final Optional<Storage> optint = Storage.create("jdbc:sqlite:static.sqlite");
		if(optint.isPresent()) {
			Movable.init(optint.get());
			Alerted.init(optint.get());
			Deleted.init();
			Hidden.init();
			HighlightData.init();
			OverlayData.init();
			//Internal lookups are no longer needed
			optint.get().close();
		} else {
			logger.atSevere().log("Failed to open static datastore");
			System.exit(0);
		}
	}

	/**
	 * Users may want to reset all graphics related settings for reasons...
	 */
	public static void resetgraphics() {
		//Custom Graphics
		SKIPLOADING.set(true);
		SHOWFLAVOBJS.set(false);
		SKIPLOADING.set(true);
		SHOWFLAVOBJS.set(true);
		SYMMETRICOUTLINES.set(false);
		SHADOWSQUALITY.set(4);
		SHADOWSIZE.set(750);
		MSAALEVEL.set(4);
		WIREFRAMEMODE.set(false);
		WEATHER.set(true);
		ANIMATIONS.set(true);
		SHOWMAP.set(true);
		SHOWGOBS.set(true);
		NIGHTVISION.set(false);
		FLATWORLD.set(false);
		SHOWTRANTILES.set(true);
	}
}


