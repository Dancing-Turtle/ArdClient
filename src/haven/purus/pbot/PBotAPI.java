package haven.purus.pbot;

import haven.Charlist;
import haven.GameUI;
import haven.MainFrame;
import haven.UI;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class PBotAPI {

    public static GameUI gui;
    public static UI ui;
    public static Charlist charlist;

    public static Map<UI, GameUI> guis = new HashMap<>();

    public static GameUI gui(UI ui) {
        checkSessions();
        //return MainFrame.instance.p.ui.gui;
        return guis.get(ui);
    }

    public static UI ui() {
        return MainFrame.instance.p.ui;
    }

    public static void checkSessions() {
        synchronized (MainFrame.instance.p.sessions) {
            Iterator<UI> itr = MainFrame.instance.p.sessions.iterator();
            while (itr.hasNext()) {
                if (!guis.containsKey(itr.next())) guis.remove(itr.next());
            }
        }
    }


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
