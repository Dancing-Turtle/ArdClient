package haven.purus.pbot;

import haven.GameUI;

public class PBotAPI {
	
	public static GameUI gui;


/*	// Null if not found


	// Crafts item with this name, name must be recipe pagina name ie. "feathertrinket"
	// Opens the craft window, presses "craft" (one) and waits until the item has been crafted
	// Assumes that all required items are in the inventory
	public static void craftItem(String name) {
		// Close current window and wait for it to close
		openCraftingWnd(name);
		boolean stop = false;
		while(!stop) {
			stop = false;
			for(Widget w : gui.ui.widgets.values()) {
				if(w instanceof Makewindow) {
					w.wdgmsg("make", 0);
					stop = true;
					break;
				}
			}
			if(!stop)
				sleep(20);
		}
		waitForHourglass();
	}
	// Get list of input materials for current craft window null if there is no current crafting window
	public static LinkedList<CraftingInputs.CraftingInput> craftingInputs() {
		LinkedList<CraftingInputs.CraftingInput> ret = new LinkedList<>();
		for(Widget w:gui.ui.widgets.values()) {
			if(w instanceof Makewindow) {
				Makewindow mw = (Makewindow) w;
				for(Makewindow.Spec spec:mw.inputs) {
					for(ItemInfo ii:spec.info()) {
						if(ii instanceof ItemInfo.Name) {
							ret.add(new CraftingInputs.CraftingInput(spec.numInt, CraftingInputs.namesOfInput(((ItemInfo.Name) ii).str.text, spec.resource().name)));
						}
					}
				}
				break;
			}
		}
		return ret;
	}*/
}
