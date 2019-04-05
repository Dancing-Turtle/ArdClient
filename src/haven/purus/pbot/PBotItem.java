package haven.purus.pbot;

import haven.*;
import haven.res.ui.tt.q.qbuff.QBuff;

import java.util.regex.Pattern;
import static haven.OCache.posres;

public class PBotItem {

	public GItem gitem;
	public WItem witem;

	public PBotItem(GItem gitem, WItem witem) {
		this.gitem = gitem;
		this.witem = witem;
	}

	public PBotItem(WItem witem) {
		this.gitem = witem.item;
		this.witem = witem;
	}

	/**
	 * Returns contents of the item
	 * @return Contents of the item
	 */
	public ItemInfo.Contents getContents() {
		synchronized(gitem.ui) {
			try {
				for(ItemInfo info : gitem.info())
					if(info instanceof ItemInfo.Contents)
						return (ItemInfo.Contents) info;
			} catch(Loading ignored) {
			}
		}
		return null;
	}


	/**
	 * Take the item to hand and wait for it to appear in hand
	 */
	public void takeItem() {
		gitem.wdgmsg("take", Coord.z);
		while(PBotUtils.getItemAtHand() != null) {
			PBotUtils.sleep(25);
		}
	}


	/**
	 * Transfer an item to the active inventory, does not wait for item to transfer
	 */
	public void transferItem() {
		gitem.wdgmsg("transfer", Coord.z);
	}


	/**
	 * Right clicks the item in the inventory
	 */
	public void activateItem() {
		gitem.wdgmsg("iact", Coord.z, 3);
	}

	/**
	 * Itemact
	 * @param mod modifier for example 1 = shift etc.
	 */
	public void itemact(int mod) {
		gitem.wdgmsg("itemact",  mod);
	}

	/**
	 * Get an amount of something such as seeds in a stack
	 * @return Amount of something in the item
	 */
	public int getAmount() {
		int ret = -1;
		synchronized(gitem.ui) {
			for(ItemInfo o : gitem.info()) {
				if(o instanceof GItem.Amount)
					ret = ((GItem.Amount) o).itemnum();
			}
		}
		return ret;
	}

	/**
	 * Returns name of the item if it exists
	 * @return Name of item or null
	 */
	public String getName() {
		synchronized(gitem.ui) {
			try {
				for(Object o : gitem.info().toArray()) {
					if(o instanceof ItemInfo.Name)
						return ((ItemInfo.Name) o).str.text;
				}
			} catch(Loading l) {

			}
		}
		return null;
	}

	/**
	 * Returns resname of the item
	 * @return Resname of the item
	 */
	public String getResname() {
		try {
			return gitem.getres().name;
		} catch(Loading l) {
		}
		return null;
	}

	/**
	 * Returns quality of the item, or -1 if quality was not found
	 * @return Quality of the item
	 */
	public double getQuality() {
		QBuff buff = gitem.quality();
		if(buff == null)
			return -1;
		else
			return buff.q;
	}

	/**
	 * Checks if player can drink from the item
	 * Only checks water and tea
	 * @return True if player can drink, else false
	 */
	public boolean canDrinkFrom() {
		synchronized(gitem.ui) {
			Pattern liquidPattern = Pattern.compile(String.format("[0-9.]+ l of (%s)",
					String.join("|", new String[]{"Water", "Piping Hot Tea", "Tea"}), Pattern.CASE_INSENSITIVE));
			ItemInfo.Contents contents = getContents();
			if(contents != null && contents.sub != null) {
				for(ItemInfo info : contents.sub) {
					if(info instanceof ItemInfo.Name) {
						ItemInfo.Name name = (ItemInfo.Name) info;
						if(name.str != null && liquidPattern.matcher(name.str.text).matches())
							return true;
					}
				}
			}
		}
		return false;
	}

	/**
	 * Drops the item from the inventory to ground, does not wait for it to drop
	 */
	public void dropItemFromInventory() {
		gitem.wdgmsg("drop", Coord.z);
	}
}
