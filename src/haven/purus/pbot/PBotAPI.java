package haven.purus.pbot;

import haven.GameUI;
import haven.MainFrame;
import haven.UI;
import modification.configuration;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class PBotAPI {

    public static GameUI gui;  //old gui init => just last new session
    public static UI ui;       //old new ui init => just last new session

    /**
     * Get the current UI in a multisession
     *
     * @return GameUI
     */
    public static UI ui() {
        return MainFrame.instance.p.ui;
    }

    /**
     * Get UI in a monosession
     * For lazy users who don't want to change scripts
     *
     * @return UI
     */
    public static UI modeui() {
        return configuration.pbotmode ? ui : MainFrame.instance.p.ui;
    }

    /**
     * Get GameUI in a monosession
     *
     * @return GameUI
     */
    public static GameUI gui() {
        return MainFrame.instance.p.ui.gui;
    }

    /**
     * Get List of all Sessions
     *
     * @return List<UI>
     */
    public static List<UI> uis() {
        List<UI> uiList = new ArrayList<UI>();
        synchronized (MainFrame.instance.p.sessions) {
            Iterator<UI> itr = MainFrame.instance.p.sessions.iterator();
            while (itr.hasNext()) uiList.add(itr.next());
        }
        return uiList;
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
