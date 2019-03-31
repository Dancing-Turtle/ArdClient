package haven.automation;

import haven.Button;
import haven.*;
import haven.Label;
import haven.Window;
import haven.purus.pbot.PBotAPI;
import haven.purus.pbot.PBotUtils;

import java.awt.*;
import java.util.ArrayList;
import java.util.Comparator;

public class PepperBotPro extends Window implements GobSelectCallback {

	private Coord a, b;
	private static final Text.Foundry infof = new Text.Foundry(Text.sans, 10).aa(true);
	private Gob barrel, hfire, water, cauldron, htable, grinder;
	private ArrayList<Gob> crops1, crops2, crops3, crops4 = new ArrayList<Gob>();
	private ArrayList<Gob> tables1, tables2, tables3, tables4 = new ArrayList<Gob>();

	private Thread selectingarea;
	private String cropName = "gfx/terobjs/plants/pepper";
	private int section = 1;
	private int direction = 1;
	public boolean allowrun;
	private final Label lblc4, lblc3, sec1, sec2, sec3, sec4;
	public PepperBotPro(GameUI gui) {

		super(new Coord(180, 430), "Pepper Bot");

		int y = 0;
		final Label lbl5 = new Label("Setup Selected", infof);
		add(lbl5, new Coord(20, y));
		y+=15;
		lblc4 = new Label("Tables West-->East", Text.num12boldFnd, Color.WHITE);
		add(lblc4, new Coord(20, y));
		y+=35;
		final Label lbl6 = new Label("Section Selected", infof);
		add(lbl6, new Coord(20, y));
		y+=15;
		lblc3 = new Label(section+"", Text.num12boldFnd, Color.WHITE);
		add(lblc3, new Coord(20, y));
		y+=25;

		final Label sec1lbl = new Label("Section 1 : ",infof);
		add(sec1lbl, new Coord(20,y));
		y+=15;
		sec1 = new Label("",Text.num12boldFnd,Color.white);
		add(sec1, new Coord(20, y));
		y+=25;

		final Label sec2lbl = new Label("Section 2 : ",infof);
		add(sec2lbl, new Coord(20,y));
		y+=15;
		sec2 = new Label("",Text.num12boldFnd,Color.white);
		add(sec2, new Coord(20, y));
		y+=25;

		final Label sec3lbl = new Label("Section 3 : ",infof);
		add(sec3lbl, new Coord(20,y));
		y+=15;
		sec3 = new Label("",Text.num12boldFnd,Color.white);
		add(sec3, new Coord(20, y));
		y+=25;

		final Label sec4lbl = new Label("Section 4 : ",infof);
		add(sec4lbl, new Coord(20,y));
		y+=15;
		sec4 = new Label("",Text.num12boldFnd,Color.white);
		add(sec4, new Coord(20, y));
		y+=25;

		if(PBotUtils.findObjectByNames(100,"gfx/terobjs/quern")!=null){
			grinder = PBotUtils.findObjectByNames(100,"gfx/terobjs/quern");
			if (!MapView.markedGobs.contains(grinder.id))
				MapView.markedGobs.add(grinder.id);
			PBotUtils.sysLogAppend("Auto added Quern","white");
		}
		if(PBotUtils.findObjectByNames(100,"gfx/terobjs/well")!=null){
			water = PBotUtils.findObjectByNames(100,"gfx/terobjs/well");
			if (!MapView.markedGobs.contains(water.id))
				MapView.markedGobs.add(water.id);
			PBotUtils.sysLogAppend("Auto added water source","white");
		}
		if(PBotUtils.findObjectByNames(100,"gfx/terobjs/cauldron")!=null){
			cauldron = PBotUtils.findObjectByNames(100,"gfx/terobjs/cauldron");
			if (!MapView.markedGobs.contains(cauldron.id))
				MapView.markedGobs.add(cauldron.id);
			PBotUtils.sysLogAppend("Auto added cauldron","white");
		}
		if(PBotUtils.findObjectByNames(100,"gfx/terobjs/pow")!=null){
			hfire = PBotUtils.findObjectByNames(100,"gfx/terobjs/pow");
			if (!MapView.markedGobs.contains(hfire.id))
				MapView.markedGobs.add(hfire.id);
			PBotUtils.sysLogAppend("Auto added hearthfire","white");
		}
		if(PBotUtils.findObjectByNames(100,"gfx/terobjs/barrel")!=null){
			barrel = PBotUtils.findObjectByNames(100,"gfx/terobjs/barrel");
			if (!MapView.markedGobs.contains(barrel.id))
				MapView.markedGobs.add(barrel.id);
			PBotUtils.sysLogAppend("Auto added barrel","white");
		}


		Button trelHarBtn = new Button(140, "Trellis harvest") {
			@Override
			public void click() {
			allowrun = true;
					if(hfire == null)
					{
						PBotUtils.sysMsg("No Hearthfire Selected.",Color.white);
						allowrun = false;
					}
					if(barrel == null)
					{
						PBotUtils.sysMsg("No barrel Selected.",Color.white);
						allowrun = false;
					}
					if(water == null)
					{
						PBotUtils.sysMsg("No water source Selected.",Color.white);
						allowrun = false;
					}
					if(cauldron == null)
					{
						PBotUtils.sysMsg("No cauldron Selected.",Color.white);
						allowrun = false;
					}


				if (a != null && b != null && allowrun) {
					PepperBotProRun bf = new PepperBotProRun(crops1, crops2, crops3, crops4, tables1, tables2, tables3, tables4, true, barrel, water, cauldron, section,hfire,direction);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else if(allowrun){
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(trelHarBtn, new Coord(20, y));
		y += 35;
		Button GrindBtn = new Button(140, "Grind Pepper") {
			@Override
			public void click() {
				allowrun = true;
				if(grinder == null)
				{
					PBotUtils.sysMsg("No grinder Selected.",Color.white);
					allowrun = false;
				}
				if (a != null && b != null && allowrun) {
					PepperGrinderRun bf = new PepperGrinderRun(a,b,grinder,section,direction);
					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else if(allowrun){
					PBotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(GrindBtn, new Coord(20, y));
		y += 35;
		Button areaSelBtn = new Button(140, "Select Area") {
			@Override
			public void click() {
				PBotUtils.sysMsg("Drag area over crops", Color.WHITE);
				selectingarea = new Thread(new PepperBotPro.selectingarea(), "Pepper Bot");
				selectingarea.start();
			}
		};
		add(areaSelBtn, new Coord(20, y));
		y += 35;
		Button sectionSelBtn = new Button(140, "Select Section") {
			@Override
			public void click() {
				section++;
				if (section>4)
					section = 1;
				PBotUtils.sysMsg("Section is now : "+section,Color.white);
				lblc3.settext(section+"");
			}
		};
		add(sectionSelBtn, new Coord(20, y));
		y += 35;
		Button sectionDirBtn = new Button(140, "Direction") {
			@Override
			public void click() {
				direction++;
				if (direction>4)
					direction = 1;
				if(direction == 1)
					lblc4.settext("Tables West --> East");
				if(direction == 2)
					lblc4.settext("Tables East --> West");
				if(direction == 3)
					lblc4.settext("Tables North --> South");
				if(direction == 4)
					lblc4.settext("Tables South --> North");
				//BotUtils.sysMsg("Section is now : "+section,Color.white);
			}
		};
		add(sectionDirBtn, new Coord(20, y));
	}

	public void gobselect(Gob gob) {
		if (gob.getres().basename().contains("barrel")) {
			barrel = gob;
			PBotUtils.sysMsg("Barrel selected!x : " + gob.rc.x + " y : " + gob.rc.y, Color.WHITE);
		} else if (gob.getres().basename().contains("well") || (gob.getres().basename().contains("cistern"))) {
			water = gob;
			PBotUtils.sysMsg("Well/Cistern selected! x : " + gob.rc.x + " y : " + gob.rc.y, Color.white);
		} else if (gob.getres().basename().contains("pow")) {
			hfire = gob;
			PBotUtils.sysMsg("Hearthfire selected!x : " + gob.rc.x + " y : " + gob.rc.y, Color.white);
		} else if (gob.getres().basename().contains("cauldron")) {
			cauldron = gob;
			PBotUtils.sysMsg("Cauldron Selected!x : " + gob.rc.x + " y : " + gob.rc.y, Color.white);
		} else if (gob.getres().basename().contains("htable")){
			htable = gob;
			int stage = gob.getStage();
			PBotUtils.sysMsg("table selected : "+gob.rc.x + " y : "+gob.rc.y+" stage : "+stage+" overlay : "+gob.ols.size(),Color.white);
	} else if (gob.getres().basename().contains("quern")){
		grinder = gob;
			PBotUtils.sysMsg("grinder selected : "+gob.rc.x + " y : "+gob.rc.y,Color.white);
	}
	}

	private class selectingarea implements Runnable {
		@Override
		public void run() {
			a = null;
			b = null;
			PBotUtils.selectArea();
			//gui.map.PBotAPISelect = true;
			// while(gui.map.PBotAPISelect)
			//BotUtils.sleep(100);
			// BotUtils.sysMsg("Adding", Color.WHITE);
				Coord selectedAreaA = PBotUtils.getSelectedAreaA();
				Coord selectedAreaB = PBotUtils.getSelectedAreaB();
				areaselect(selectedAreaA, selectedAreaB);
		}
	}



	public ArrayList<Gob> Crops(boolean checkStage, Coord rc1, Coord rc2) {
		// Initialises list of crops to harvest between selected coordinates
		ArrayList<Gob> gobs = new ArrayList<Gob>();
		double bigX = rc1.x > rc2.x ? rc1.x : rc2.x;
		double smallX = rc1.x < rc2.x ? rc1.x : rc2.x;
		double bigY = rc1.y > rc2.y ? rc1.y : rc2.y;
		double smallY = rc1.y < rc2.y ? rc1.y : rc2.y;
		synchronized (ui.sess.glob.oc) {
			for (Gob gob : ui.sess.glob.oc) {
				if (gob.rc.x <= bigX && gob.rc.x >= smallX && gob.getres() != null && gob.rc.y <= bigY
						&& gob.rc.y >= smallY && cropName.contains(gob.getres().name)) {
					// Add to list if its max stage
					if (checkStage) {
						int cropstgmaxval = 0;
						for (FastMesh.MeshRes layer : gob.getres().layers(FastMesh.MeshRes.class)) {
							int stg = layer.id / 10;
							if (stg > cropstgmaxval)
								cropstgmaxval = stg;
						}
						if (gob.getStage() == cropstgmaxval) {
							gobs.add(gob);
						}
					} else
						gobs.add(gob);
				}
			}
		}
		if(direction == 1)
			gobs.sort(new CoordSort1());

		else if (direction == 2)
			gobs.sort(new CoordSort2());

		else if (direction == 3)
			gobs.sort(new CoordSort3());

		else if (direction == 4)
			gobs.sort(new CoordSort4());

		return gobs;
	}
	public ArrayList<Gob> Tables(Coord rc1, Coord rc2) {
		// Initialises list of crops to harvest between selected coordinates
		ArrayList<Gob> gobs = new ArrayList<Gob>();
		double bigX = rc1.x > rc2.x ? rc1.x : rc2.x;
		double smallX = rc1.x < rc2.x ? rc1.x : rc2.x;
		double bigY = rc1.y > rc2.y ? rc1.y : rc2.y;
		double smallY = rc1.y < rc2.y ? rc1.y : rc2.y;
		synchronized (ui.sess.glob.oc) {
			for (Gob gob : ui.sess.glob.oc) {
				if (gob.rc.x <= bigX && gob.rc.x >= smallX && gob.getres() != null && gob.rc.y <= bigY
						&& gob.rc.y >= smallY && gob.getres().basename().contains("htable")) {
					gobs.add(gob);
				}
			}
		}
		if(direction == 1)
			gobs.sort(new CoordSort1());

		else if (direction == 2)
			gobs.sort(new CoordSort2());

		else if (direction == 3)
			gobs.sort(new CoordSort3());

		else if (direction == 4)
			gobs.sort(new CoordSort4());

		return gobs;
	}

	class CoordSort1 implements Comparator<Gob> {
		public int compare(Gob a, Gob b) {
			if (a.rc.x == b.rc.x) {
				if (a.rc.x % 2 == 0)
					return (a.rc.y < b.rc.y) ? 1 : (a.rc.y > b.rc.y) ? -1 : 0;
				else
					return (a.rc.y < b.rc.y) ? -1 : (a.rc.y > b.rc.y) ? 1 : 0;
			} else
				return (a.rc.x < b.rc.x) ? 1 : (a.rc.x > b.rc.x) ? -1 : 0;
		}
	}
	// Sorts coordinate array High Y to Low Y along Identical X Axis Tables East
	class CoordSort2 implements Comparator<Gob> {
		public int compare(Gob a, Gob b) {
			if (a.rc.x == b.rc.x) {
				if (a.rc.x % 2 == 0)
					return (a.rc.y < b.rc.y) ? 1 : (a.rc.y > b.rc.y) ? -1 : 0;
				else
					return (a.rc.y < b.rc.y) ? -1 : (a.rc.y > b.rc.y) ? 1 : 0;
			} else
				return (a.rc.x < b.rc.x) ? -1 : (a.rc.x > b.rc.x) ? 1 : 0;
		}
	}
	// Sorts coordinate array High Y to Low Y along Identical X Axis Tables North
	class CoordSort3 implements Comparator<Gob> {
		public int compare(Gob a, Gob b) {
			if (a.rc.y == b.rc.y) {
				if (a.rc.y % 2 == 0)
					return (a.rc.x < b.rc.x) ? 1 : (a.rc.x > b.rc.x) ? -1 : 0;
				else
					return (a.rc.x < b.rc.x) ? -1 : (a.rc.x > b.rc.x) ? 1 : 0;
			} else
				return (a.rc.y < b.rc.y) ? -1 : (a.rc.y > b.rc.y) ? 1 : 0;
		}
	}
	// Sorts coordinate array High Y to Low Y along Identical X Axis Tables South
	class CoordSort4 implements Comparator<Gob> {
		public int compare(Gob a, Gob b) {
			if (a.rc.y == b.rc.y) {
				if (a.rc.y % 2 == 0)
					return (a.rc.x < b.rc.x) ? 1 : (a.rc.x > b.rc.x) ? -1 : 0;
				else
					return (a.rc.x < b.rc.x) ? -1 : (a.rc.x > b.rc.x) ? 1 : 0;
			} else
				return (a.rc.y < b.rc.y) ? 1 : (a.rc.y > b.rc.y) ? -1 : 0;
		}
	}


	private void registerGobSelect() {
		synchronized (GobSelectCallback.class) {
			PBotAPI.gui.map.registerGobSelect(this);
		}
	}

	public void areaselect(Coord a, Coord b) {
		System.out.println("Area select triggered.");
		this.a = a;
		this.b = b;
		if(section ==1 ){
			crops1 = Crops(true, this.a,this.b);
			tables1 = Tables(this.a,this.b);
			sec1.settext("Crops : "+crops1.size()+" Tables : "+tables1.size());
		}else if(section == 2){
			crops2 = Crops(true, this.a,this.b);
			tables2 = Tables(this.a,this.b);
			sec2.settext("Crops : "+crops2.size()+" Tables : "+tables2.size());
		}else if(section == 3){
			crops3 = Crops(true, this.a,this.b);
			tables3 = Tables(this.a,this.b);
			sec3.settext("Crops : "+crops3.size()+" Tables : "+tables3.size());
		}else if(section == 4){
			crops4 = Crops(true, this.a,this.b);
			tables4 = Tables(this.a,this.b);
			sec4.settext("Crops : "+crops4.size()+" Tables : "+tables4.size());
		}
		PBotUtils.mapInteractLeftClick(0);
		section++;
		if (section>4)
			section = 1;
		PBotUtils.sysMsg("Ready to input next section.",Color.white);
		lblc3.settext(section+"");
	}

	@Override
	public void wdgmsg(Widget sender, String msg, Object... args) {
		if (sender == cbtn)
			reqdestroy();
		else
			super.wdgmsg(sender, msg, args);
	}
}
