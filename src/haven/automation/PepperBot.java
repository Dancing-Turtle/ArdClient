package haven.automation;

import haven.Button;
import haven.*;
import haven.Composite;
import haven.Label;
import haven.Window;
import haven.purus.BotUtils;
import haven.purus.SeedCropFarmer;
import haven.automation.PepperBotRun;
import haven.purus.pbot.PBotAPI;
import haven.res.ui.tt.q.qbuff.QBuff;
import net.dv8tion.jda.client.entities.Application;

import java.awt.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import static haven.OCache.posres;

public class PepperBot extends Window implements AreaSelectCallback, GobSelectCallback {

	private Coord a, b;
	private boolean containeronly = false, replant = true, replantcontainer = false;
	private static final Text.Foundry infof = new Text.Foundry(Text.sans, 10).aa(true);
	private CheckBox replantChkbox, fillContainerChkbox, replantBarrelChkbox;
	private Gob barrel, hfire, rowmarker, water, cauldron, htable, grinder;
	public Thread testthread;
	private final int rowgap = 4200;
	private final int northtravel = 20000;
	private int section = 1;
	private int direction = 1;
	List<WItem> ItemList;
	public boolean allowrun;
	private final Label lblc4, lblc3;
	private final int travel = 20000;
	public int xx, yy;

	public PepperBot(GameUI gui) {

		super(new Coord(180, 290), "Pepper Bot");

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

		if(BotUtils.findObjectByNames(100,"gfx/terobjs/quern")!=null){
			grinder = BotUtils.findObjectByNames(100,"gfx/terobjs/quern");
			if (!MapView.markedGobs.contains(grinder.id))
				MapView.markedGobs.add(grinder.id);
			BotUtils.sysLogAppend("Auto added Quern","white");
		}
		if(BotUtils.findObjectByNames(100,"gfx/terobjs/well")!=null){
			water = BotUtils.findObjectByNames(100,"gfx/terobjs/well");
			if (!MapView.markedGobs.contains(water.id))
				MapView.markedGobs.add(water.id);
			BotUtils.sysLogAppend("Auto added water source","white");
		}
		if(BotUtils.findObjectByNames(100,"gfx/terobjs/cauldron")!=null){
			cauldron = BotUtils.findObjectByNames(100,"gfx/terobjs/cauldron");
			if (!MapView.markedGobs.contains(cauldron.id))
				MapView.markedGobs.add(cauldron.id);
			BotUtils.sysLogAppend("Auto added cauldron","white");
		}
		if(BotUtils.findObjectByNames(100,"gfx/terobjs/pow")!=null){
			hfire = BotUtils.findObjectByNames(100,"gfx/terobjs/pow");
			if (!MapView.markedGobs.contains(hfire.id))
				MapView.markedGobs.add(hfire.id);
			BotUtils.sysLogAppend("Auto added hearthfire","white");
		}
		if(BotUtils.findObjectByNames(100,"gfx/terobjs/barrel")!=null){
			barrel = BotUtils.findObjectByNames(100,"gfx/terobjs/barrel");
			if (!MapView.markedGobs.contains(barrel.id))
				MapView.markedGobs.add(barrel.id);
			BotUtils.sysLogAppend("Auto added barrel","white");
		}


		Button trelHarBtn = new Button(140, "Trellis harvest") {
			@Override
			public void click() {
			allowrun = true;
					if(hfire == null)
					{
						BotUtils.sysMsg("No Hearthfire Selected.",Color.white);
						allowrun = false;
					}
					if(barrel == null)
					{
						BotUtils.sysMsg("No barrel Selected.",Color.white);
						allowrun = false;
					}
					if(water == null)
					{
						BotUtils.sysMsg("No water source Selected.",Color.white);
						allowrun = false;
					}
					if(cauldron == null)
					{
						BotUtils.sysMsg("No cauldron Selected.",Color.white);
						allowrun = false;
					}


				if (a != null && b != null && allowrun) {
					PepperBotRun bf = new PepperBotRun(a, b, true, false, false, barrel, water, cauldron, section,hfire,direction);

					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else if(allowrun){
					BotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(trelHarBtn, new Coord(20, y));
		y += 35;
		Button GrindBtn = new Button(140, "Grind Pepper") {
			@Override
			public void click() {
				allowrun = true;
				if(hfire == null)
				{
					BotUtils.sysMsg("No Hearthfire Selected.",Color.white);
					allowrun = false;
				}
				if(grinder == null)
				{
					BotUtils.sysMsg("No grinder Selected.",Color.white);
					allowrun = false;
				}
				if (a != null && b != null && allowrun) {
					PepperGrinderRun bf = new PepperGrinderRun(a,b,grinder,section,direction);
					gameui().add(bf, new Coord(gameui().sz.x / 2 - bf.sz.x / 2, gameui().sz.y / 2 - bf.sz.y / 2 - 200));
					new Thread(bf).start();
					this.parent.destroy();
				} else if(allowrun){
					BotUtils.sysMsg("Area not selected!", Color.WHITE);
				}
			}
		};
		add(GrindBtn, new Coord(20, y));
		y += 35;
		Button areaSelBtn = new Button(140, "Select Area") {
			@Override
			public void click() {
				BotUtils.sysMsg("Drag area over crops", Color.WHITE);
				gameui().map.farmSelect = true;
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
				BotUtils.sysMsg("Section is now : "+section,Color.white);
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
		y += 35;
		Button RunBtn = new Button(140, "Run Test") {
			@Override
			public void click() {
				testthread = new Thread(new PepperBot.testthread(),"Pepper Bot");
				testthread.start();
			}
		};
		add(RunBtn, new Coord(20, y));
	}

	public void gobselect(Gob gob) {
		if (gob.getres().basename().contains("barrel")) {
			barrel = gob;
			BotUtils.sysMsg("Barrel selected!x : " + gob.rc.x + " y : " + gob.rc.y, Color.WHITE);
		} else if (gob.getres().basename().contains("well") || (gob.getres().basename().contains("Cistern"))) {
			water = gob;
			BotUtils.sysMsg("Well selected! x : " + gob.rc.x + " y : " + gob.rc.y, Color.white);
		} else if (gob.getres().basename().contains("pow")) {
			hfire = gob;
			BotUtils.sysMsg("Hearthfire selected!x : " + gob.rc.x + " y : " + gob.rc.y, Color.white);
		} else if (gob.getres().basename().contains("cauldron")) {
			cauldron = gob;
			BotUtils.sysMsg("Cauldron Selected!x : " + gob.rc.x + " y : " + gob.rc.y, Color.white);
		} else if (gob.getres().basename().contains("htable")){
			htable = gob;
			int stage = gob.getStage();
			BotUtils.sysMsg("table selected : "+gob.rc.x + " y : "+gob.rc.y+" stage : "+stage+" overlay : "+gob.ols.size(),Color.white);
	} else if (gob.getres().basename().contains("quern")){
		grinder = gob;
		BotUtils.sysMsg("grinder selected : "+gob.rc.x + " y : "+gob.rc.y,Color.white);
	}
	}

	private void registerGobSelect() {
		synchronized (GobSelectCallback.class) {
			BotUtils.gui.map.registerGobSelect(this);
		}
	}

	public void areaselect(Coord a, Coord b) {
		this.a = a.mul(MCache.tilesz2);
		this.b = b.mul(MCache.tilesz2).add(11, 11);
		BotUtils.sysMsg("Area selected!", Color.WHITE);
		BotUtils.gui.map.unregisterAreaSelect();
	}

	@Override
	public void wdgmsg(Widget sender, String msg, Object... args) {
		if (sender == cbtn)
			reqdestroy();
		else
			super.wdgmsg(sender, msg, args);
	}

	public class testthread implements Runnable{
@Override
	public void run() {
	BotUtils.sysMsg("Started", Color.white);
	GameUI gui = gameui();
	if(gui.getwnd("Crafting")!=null)
		gui.getwnd("Crafting").close();

	gui.wdgmsg("act", "craft", "boiledpepper");
	BotUtils.waitForWindow("Crafting");
	for(Widget a = gui.lchild;a!=null;a=a.prev) {
		if (a instanceof CraftWindow) {
			for (Widget aa = a.lchild; aa != null; aa = aa.prev) {
				for (Widget aaa = aa.lchild; aaa != null; aaa = aaa.prev) {
					if (aaa instanceof Button) {
						if (((Button) aaa).text.text == "Craft All") {
							BotUtils.sysMsg("button found",Color.white);
							break;
						}
					}
				}
			}
		}
	}


	//List<WItem> idk = BotUtils.getInventoryContents(BotUtils.playerInventory());
	//for (WItem idkok : idk)
	//	System.out.println("res : " + idkok.item.resource().name);
}

	public double round(double value, int places) {
		if (places < 0) throw new IllegalArgumentException();

		long factor = (long) Math.pow(10, places);
		value = value * factor;
		long tmp = Math.round(value);
		return (double) tmp / factor;
	}
		public void sort(java.util.List<WItem> items){
			Collections.sort(items, (a, b) -> {
				QBuff aq = a.item.quality();
				QBuff bq = b.item.quality();
				if (aq == null || bq == null)
					return 0;
				else if (aq.q == bq.q)
					return 0;
				else if (aq.q > bq.q)
					return -1;
				else
					return 1;
			});
		}
		public java.util.List<WItem> getIdenticalItems(GItem item) {
			List<WItem> items = new ArrayList<WItem>();
			GSprite sprite = item.spr();
			if (sprite != null) {
				String name = sprite.getname();
				String resname = item.resource().name;
				for (Widget wdg = child; wdg != null; wdg = wdg.next) {
					if (wdg instanceof WItem) {
						sprite = ((WItem) wdg).item.spr();
						if (sprite != null) {
							Resource res = ((WItem) wdg).item.resource();
							if (res != null && res.name.equals(resname) && (name == null || name.equals(sprite.getname())))
								items.add((WItem) wdg);
						}
					}
				}
			}
			return items;
		}
		public WItem getItemPartial(String name) {
			for (Widget wdg = child; wdg != null; wdg = wdg.next) {
				if (wdg instanceof WItem) {
					String wdgname = ((WItem)wdg).item.getname();
					if (wdgname.contains(name))
						return (WItem) wdg;
				}
			}
			return null;
		}
}
}
