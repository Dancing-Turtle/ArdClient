package haven.purus;

import haven.*;

public class DrinkWater implements Runnable {

	GameUI gui;

	public DrinkWater(GameUI gui) {
		this.gui = gui;
	}

	@Override
	public void run() {
		drink();
	}

	private void drink() {
		// Don't attempt to drink if flower menu is already open or we are already drinking
		if(gui.ui.root.findchild(FlowerMenu.class) != null || gui.drinkingWater)
			return;
		gui.drinkingWater = true;
		WItem drinkFromThis = null;
		Equipory e = gui.getequipory();
		WItem l = e.quickslots[6];
		WItem r = e.quickslots[7];
		if (canDrinkFrom(l))
			drinkFromThis = l;
		if (canDrinkFrom(r))
			drinkFromThis = r;
		for (Widget w = gui.lchild; w != null; w = w.prev) {
			if (w instanceof Window) {
				Window wnd = (Window) w;
				for(Widget wdg = wnd.lchild; wdg != null; wdg = wdg.prev) {
					if(wdg instanceof Inventory) {
						Inventory inv = (Inventory) wdg;
						for(WItem item : inv.children(WItem.class)) {
							if(canDrinkFrom(item))
								drinkFromThis = item;
						}
					}
				}
			} else if(w instanceof AltBeltWnd) { // Alternate belt must be separately enabled

				AltBeltWnd invBelt = (AltBeltWnd)w;
				for(WItem item:invBelt.children(WItem.class)) {
					if(canDrinkFrom(item))
						drinkFromThis = item;
				}
			}
		}
		if(drinkFromThis != null) {
			drinkFromThis.item.wdgmsg("iact", Coord.z, 3);
			FlowerMenu menu = gui.ui.root.findchild(FlowerMenu.class);
			int retries = 0; // After 100 retries aka. 5 seconds, it will probably never appear
			while(menu == null) {
				if(retries++ > 100) {
					gui.drinkingWater = false;
					return;
				}
				sleep(50);
				menu = gui.ui.root.findchild(FlowerMenu.class);
			}
			for(FlowerMenu.Petal opt:menu.opts) {
				if(opt.name.equals("Drink")) {
					menu.choose(opt);
					menu.destroy();
				}
			}
			gui.lastDrinkingSucessful = true;
		} else {
			gui.lastDrinkingSucessful = false;
		}
		gui.drinkingWater = false;
	}

	private boolean canDrinkFrom(WItem item) {
		ItemInfo.Contents contents = getContents(item);
		if (contents != null && contents.sub != null) {
			synchronized(item.item.ui) {
				for(ItemInfo info : contents.sub) {
					if(info instanceof ItemInfo.Name) {
						ItemInfo.Name name = (ItemInfo.Name) info;
						if(name.str != null && name.str.text.contains("Water"))
							return true;
					}
				}
			}
		}
		return false;
	}

	private void sleep(int timeInMs) {
		try {
			Thread.sleep(timeInMs);
		} catch(InterruptedException e) {
			e.printStackTrace();
		}
	}

	private ItemInfo.Contents getContents(WItem item) {
		if(item == null)
			return null;
		synchronized(item.item.ui) {
			try {
				for(ItemInfo info : item.item.info())
					if(info != null && info instanceof ItemInfo.Contents)
						return (ItemInfo.Contents) info;
			} catch(Loading ignored) {
			}
		}
		return null;
	}
}
