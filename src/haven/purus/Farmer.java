package haven.purus;

import haven.Button;
import haven.*;
import haven.Window;
import haven.automation.AreaSelectCallback;
import haven.automation.GobSelectCallback;
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;

import java.awt.*;
import java.util.ArrayList;

public class Farmer extends Window implements AreaSelectCallback, GobSelectCallback {

	private Coord a, b;
	private boolean containeronly = false, replant = false, replantcontainer = true;
	private CheckBox replantChkbox, fillContainerChkbox, replantBarrelChkbox;
	private Gob barrel, chest;
	private ArrayList<Gob> containers = new ArrayList<>();

	public Farmer() {
		super(new Coord(180, 690), "Farming Bots");
		int y = 0;
		PBotUtils.sysMsg("Hold alt and left click containers to select them.",Color.white);
		Button carrotBtn = new Button(140, "Carrot") {
			@Override
			public void click() {
				if (a != null && b != null) {
					gameui().map.unregisterAreaSelect();
					// Start carrot farmer and close this window
					SeedCropFarmer SCF = new SeedCropFarmer(b, a, "gfx/terobjs/plants/carrot", "gfx/invobjs/carrot", 4, true, false, false, containers);
					gameui().add(SCF,
							new Coord(gameui().sz.x / 2 - SCF.sz.x / 2, gameui().sz.y / 2 - SCF.sz.y / 2 - 200));
					new Thread(SCF).start();
					this.parent.destroy();
				} else {
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(carrotBtn, new Coord(20, y));
		y += 35;

		Button carrotseedBtn = new Button(140, "Carrot Seeds") {
			@Override
			public void click() {
				if(replantcontainer && containers.size() == 0 || containeronly && containers.size() == 0) {
					PBotUtils.sysMsg("Please select a container by holding alt and clicking it before starting if using barrel or replantbarrel.", Color.white);
				}
				else if (a != null && b != null) {
					SeedCropFarmer bf =
							new SeedCropFarmer(a, b, "gfx/terobjs/plants/carrot", "gfx/invobjs/seed-carrot", 3, replant, containeronly, replantcontainer, containers);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else {
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(carrotseedBtn, new Coord(20, y));
		y += 35;

		Button onionBtn = new Button(140, "Yellow Onion") {
			@Override
			public void click() {
				if (a != null && b != null) {
					// Start yellow onion farmer and close this window
					SeedCropFarmer bf =
							new SeedCropFarmer(a, b, "gfx/terobjs/plants/yellowonion", "gfx/invobjs/yellowonion", 3, true, false, false, containers);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else {
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(onionBtn, new Coord(20, y));
		y += 35;

		Button redOnionBtn = new Button(140, "Red Onion") {
			@Override
			public void click() {
			if (a != null && b != null) {
					// Start yellow onion farmer and close this window
					SeedCropFarmer bf =
							new SeedCropFarmer(a, b, "gfx/terobjs/plants/redonion", "gfx/invobjs/redonion", 3,true, false, false, containers);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else {
				PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(redOnionBtn, new Coord(20, y));
		y += 35;

		Button beetBtn = new Button(140, "Beetroot") {
			@Override
			public void click() {
			if (a != null && b != null) {
					// Start beetroot onion farmer and close this window
					SeedCropFarmer bf = new SeedCropFarmer(a, b, "gfx/terobjs/plants/beet", "gfx/invobjs/beet", 3,true, false, false, containers);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else {
				PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(beetBtn, new Coord(20, y));
		y += 35;

		Button leekBtn = new Button(140, "Leeks") {
			@Override
			public void click() {
		if (a != null && b != null) {
					// Start beetroot onion farmer and close this window
					SeedCropFarmer bf = new SeedCropFarmer(a, b, "gfx/terobjs/plants/leek", "gfx/invobjs/leek", 4,true, false, false, containers);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else {
			PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(leekBtn, new Coord(20, y));
		y += 35;

		Button turnipBtn = new Button(140, "Turnip") {
			@Override
			public void click() {
				if (a != null && b != null) {
					// Start beetroot onion farmer and close this window
					SeedCropFarmer bf = new SeedCropFarmer(a, b, "gfx/terobjs/plants/turnip", "gfx/invobjs/turnip", 3, true, false, false, containers);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else {
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(turnipBtn, new Coord(20, y));
		y += 35;

		Button turnipseedBtn = new Button(140, "Turnip Seeds") {
			@Override
			public void click() {
				if(replantcontainer && containers.size() == 0 || containeronly && containers.size() == 0) {
					PBotUtils.sysMsg("Please select a container by holding alt and clicking it before starting if using barrel or replantbarrel.", Color.white);
				}
				else if (a != null && b != null) {
					SeedCropFarmer bf = new SeedCropFarmer(a, b, "gfx/terobjs/plants/turnip", "gfx/invobjs/seed-turnip", 1, replant, containeronly, replantcontainer, containers);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else {
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(turnipseedBtn, new Coord(20, y));
		y += 35;

		Button pumpkinBtn = new Button(140, "Pumpkin") {
			@Override
			public void click() {
				if (a != null && b != null) {
					SeedCropFarmer bf = new SeedCropFarmer(a, b, "gfx/terobjs/plants/pumpkin", "gfx/invobjs/seed-pumpkin", 4, true, false, false, containers);
					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else {
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(pumpkinBtn, new Coord(20, y));
		y += 35;

		Button barleyBtn = new Button(140, "Barley") {
			@Override
			public void click() {
				if(replantcontainer && containers.size() == 0 || containeronly && containers.size() == 0) {
					PBotUtils.sysMsg("Please select a container by holding alt and clicking it before starting if using barrel or replantbarrel.", Color.white);
				}
				else if (a != null && b != null) {
				//	System.out.println(a + "" + b);
					// Start barley farmer and close this window
					SeedCropFarmer bf =
							new SeedCropFarmer(a, b, "gfx/terobjs/plants/barley", "gfx/invobjs/seed-barley", 3, replant, containeronly, replantcontainer, containers);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else {
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(barleyBtn, new Coord(20, y));
		y += 35;

		Button wheatBtn = new Button(140, "Wheat") {
			@Override
			public void click() {
				if(replantcontainer && containers.size() == 0 || containeronly && containers.size() == 0) {
					PBotUtils.sysMsg("Please select a container by holding alt and clicking it before starting if using barrel or replantbarrel.", Color.white);
				}
				else if (a != null && b != null) {
					// Start yellow onion farmer and close this window
					SeedCropFarmer bf =
							new SeedCropFarmer(a, b, "gfx/terobjs/plants/wheat", "gfx/invobjs/seed-wheat", 3, replant, containeronly, replantcontainer, containers);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else {
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(wheatBtn, new Coord(20, y));
		y += 35;

		Button flaxBtn = new Button(140, "Flax") {
			@Override
			public void click() {
				if(replantcontainer && containers.size() == 0 || containeronly && containers.size() == 0) {
					PBotUtils.sysMsg("Please select a container by holding alt and clicking it before starting if using barrel or replantbarrel.", Color.white);
				}
				else if (a != null && b != null) {
					// Start flax farmer and close this window
					SeedCropFarmer bf = new SeedCropFarmer(a, b, "gfx/terobjs/plants/flax", "gfx/invobjs/seed-flax", 3, replant, containeronly, replantcontainer, containers);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else {
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(flaxBtn, new Coord(20, y));
		y += 35;

		Button poppyBtn = new Button(140, "Poppy") {
			@Override
			public void click() {
				if(replantcontainer && containers.size() == 0 || containeronly && containers.size() == 0) {
					PBotUtils.sysMsg("Please select a container by holding alt and clicking it before starting if using barrel or replantbarrel.", Color.white);
				}
				else if (a != null && b != null) {
					// Start poppy farmer and close this window
					SeedCropFarmer bf =
							new SeedCropFarmer(a, b, "gfx/terobjs/plants/poppy", "gfx/invobjs/seed-poppy", 4, replant, containeronly, replantcontainer, containers);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else {
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(poppyBtn, new Coord(20, y));
		y += 35;

		Button hempBtn = new Button(140, "Hemp") {
			@Override
			public void click() {
				if(replantcontainer && containers.size() == 0 || containeronly && containers.size() == 0) {
					PBotUtils.sysMsg("Please select a container by holding alt and clicking it before starting if using barrel or replantbarrel.", Color.white);
				}
				else if (a != null && b != null) {
					// Start hemp farmer and close this window
					SeedCropFarmer bf = new SeedCropFarmer(a, b, "gfx/terobjs/plants/hemp", "gfx/invobjs/seed-hemp", 4, replant, containeronly, replantcontainer, containers);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
			}
		};
		add(hempBtn, new Coord(20, y));
		y += 35;

		Button pipeBtn = new Button(140, "Pipeweed") {
			@Override
			public void click() {
				if(replantcontainer && containers.size() == 0 || containeronly && containers.size() == 0) {
					PBotUtils.sysMsg("Please select a container by holding alt and clicking it before starting if using barrel or replantbarrel.", Color.white);
				}
				else if (a != null && b != null) {
					// Start hemp farmer and close this window
					SeedCropFarmer bf = new SeedCropFarmer(a, b, "gfx/terobjs/plants/pipeweed", "gfx/invobjs/seed-pipeweed", 4, replant, containeronly, replantcontainer, containers);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
			}
		};
		add(pipeBtn, new Coord(20, y));
		y += 35;

		Button trelHarBtn = new Button(140, "Trellis harvest") {
			@Override
			public void click() {
				if (a != null && b != null) {
					// Start yellow onion farmer and close this window
					TrellisFarmer bf = new TrellisFarmer(a, b, true, false, false, chest);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else {
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(trelHarBtn, new Coord(20, y));
		y += 35;

		Button trelDesBtn = new Button(140, "Trellis destroy") {
			@Override
			public void click() {
				if (a != null && b != null) {
					// Start yellow onion farmer and close this window
					TrellisFarmer bf = new TrellisFarmer(a, b, false, true, false, chest);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else {
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(trelDesBtn, new Coord(20, y));
		y += 35;

		Button trelPlantBtn = new Button(140, "Trellis plant") {
			@Override
			public void click() {
				if (a != null && b != null) {
					// Start yellow onion farmer and close this window
					TrellisFarmer bf = new TrellisFarmer(a, b, false, false, true, chest);
					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else {
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(trelPlantBtn, new Coord(20, y));
		y += 35;

		Button areaSelBtn = new Button(140, "Select Area") {
			@Override
			public void click() {
				PBotUtils.sysMsg("Drag area over crops", Color.WHITE);
				gameui().map.farmSelect = true;
			}
		};
		add(areaSelBtn, new Coord(20, y));
		y += 35;
		replantChkbox = new CheckBox("Replant") {
			{
				a = replant;
			}

			public void set(boolean val) {
				a = val;
				replant = val;
				containeronly = !val;
				replantcontainer = !val;

				fillContainerChkbox.a = !val;
				replantBarrelChkbox.a = !val;
			}
		};
		add(replantChkbox, new Coord(20, y));
		replantBarrelChkbox = new CheckBox("Plant+Barrel") {
			{
				a = replantcontainer;
			}

			public void set(boolean val) {
				a = val;
				replantcontainer = val;
				replant = !val;
				containeronly = !val;

				replantChkbox.a = !val;
				fillContainerChkbox.a = !val;
			}
		};
		add(replantBarrelChkbox, new Coord(85, y));
		y += 15;
		fillContainerChkbox = new CheckBox("Barrel") {
			{
				a = containeronly;
			}

			public void set(boolean val) {
				a = val;
				containeronly = val;
				replant = !val;
				replantcontainer = !val;

				replantBarrelChkbox.a = !val;
				replantChkbox.a = !val;
			}
		};
		add(fillContainerChkbox, new Coord(65, y));

	}

	private void registerGobSelect() {
		synchronized (GobSelectCallback.class) {
			PBotAPI.gui.map.registerGobSelect(this);
		}
	}

	public void areaselect(Coord a, Coord b) {
		this.a = a.mul(MCache.tilesz2);
		this.b = b.mul(MCache.tilesz2).add(11, 11);
		PBotUtils.sysMsg("Area selected!", Color.WHITE);
		PBotAPI.gui.map.unregisterAreaSelect();
	}

	@Override
	public void wdgmsg(Widget sender, String msg, Object... args) {
		if (sender == cbtn) {
			PBotAPI.gui.map.unregisterGobSelect();
			this.destroy();
		}
		else
			super.wdgmsg(sender, msg, args);
	}

	@Override
	public void gobselect(Gob gob) {
		if (gob.getres().basename().contains("barrel") || gob.getres().basename().contains("trough") || gob.getres().basename().contains("cistern")) {
			if(!containers.contains(gob)) {
				containers.add(gob);
				PBotUtils.sysMsg("Barrel/Trough/Cistern added! Total : "+containers.size(), Color.WHITE);
			}
		} else if (gob.getres().basename().contains("chest")) {
			chest = gob;
			PBotUtils.sysMsg("Chest selected!", Color.WHITE);
		} else
			PBotUtils.sysMsg("Please select only a barrel, trough, or chest!", Color.WHITE);
		//gameui().map.unregisterGobSelect();
	}
}
