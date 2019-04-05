package haven.purus.pbot;

import haven.*;

import java.util.ArrayList;
import java.util.List;

public class PBotInventory {

	private Inventory inv;

	public PBotInventory(Inventory inv) {
		this.inv = inv;
	}

	/**
	 * Return all items that the inventory contains
	 * @return List of items in the inventory
	 */
	public List<PBotItem> getInventoryContents() {
		List<PBotItem> items = new ArrayList<>();
		for(Widget witm = inv.child; witm != null; witm = witm.next) {
			synchronized(witm) {
				if(witm instanceof WItem) {
					WItem witem = (WItem) witm;
					items.add(new PBotItem(witem));
				}
			}
		}
		return items;
	}

	/**
	 * Returns a list of items with specific name(s) from the inventory
	 * @return List of items found
	 */
	public List<PBotItem> getInventoryItemsByNames(List<String> names) {
		List<PBotItem> items = new ArrayList<>();
		for(PBotItem item : getInventoryContents()) {
			String name = item.getName();
			for(String s : names) {
				if(s.equals(name))
					items.add(item);
			}
		}
		return items;
	}

	/**
	 * Returns list of items with specific resfile name(s) from the inventory
	 * @return List of items found
	 */
	public List<PBotItem> getInventoryItemsByResnames(List<String> resnames) {
		List<PBotItem> items = new ArrayList<>();
		for(PBotItem item : getInventoryContents()) {
			String name = item.getResname();
			for(String s : resnames) {
				if(s.equals(name))
					items.add(item);
			}
		}
		return items;
	}

	/**
	 * Finds an item with certain location from the inventory
	 * @param xLoc x-coordinate of the item location in inventory
	 * @param yLoc y-coordinate of the item location in inventory
	 * @return Null if not found
	 */
	public PBotItem getItemFromInventoryAtLocation(int xLoc, int yLoc) {
		for(Widget w = inv.child; w != null; w = w.next) {
			if(w instanceof WItem) {
				WItem itm = (WItem)w;
				if(itm.c.div(33).x == xLoc && itm.c.div(33).y == yLoc)
					return new PBotItem(itm);
			}
		}
		return null;
	}


	// Returns coordinates for placement if the given inventory matrix has space for item, 1 = grid reserved, 0 = grid free O(n*m) where n and m dimensions of matrix, null if no space
	public Coord freeSpaceForItem(PBotItem itm) {
		int[][] inventoryMatrix = containerMatrix();
		WItem item = itm.witem;
		int[][] d = new int[inventoryMatrix.length][inventoryMatrix[0].length];

		int sizeX = item.c.div(33).x;
		int sizeY = item.c.div(33).y;
		for(int i=0; i<inventoryMatrix.length; i++) {
			for(int j=0; j<inventoryMatrix[0].length; j++) {
				if(inventoryMatrix[i][j] == 1)
					d[i][j] = 0;
				else
					d[i][j] = (j == 0 ? 1 : d[i][j-1]+1);
			}
		}

		for(int i=0; i<inventoryMatrix[0].length; i++) {
			int curLen = 0;
			System.out.println();
			for(int j=0; j<inventoryMatrix.length; j++) {
				System.out.print(d[j][i]);
				if(d[j][i] >= sizeY)
					curLen++;
				else
					curLen = 0;
				if(curLen >= sizeX)
					return new Coord(j,i);
			}
		}

		return null;
	}


	// Returns a matrix representing the container and items inside, 1 = item in this grid, 0 = free grid
	public int[][] containerMatrix() {
		int[][] ret = new int[inv.isz.x][inv.isz.y];
		for(PBotItem item:getInventoryContents()) {
			int xSize = item.gitem.size().x;
			int ySize = item.gitem.size().y;
			int xLoc = item.witem.c.div(33).x;
			int yLoc = item.witem.c.div(33).y;

			for(int i = 0; i < xSize; i++) {
				for(int j = 0; j < ySize; j++) {
					ret[i + xLoc][j + yLoc] = 1;
				}
			}
		}
		return ret;
	}

	/**
	 * Drop item from the hand to given slot in inventory
	 * @param coord Slot to drop the item to
	 */
	public void dropItemToInventory(Coord coord) {
		inv.wdgmsg("drop", coord);
	}

	/**
	 * Amount of free slots in the inventory
	 * @return Amount of free inventory slots
	 */
	public int freeSlotsInv() {
		int takenSlots = 0;
		for(Widget i = inv.child; i != null; i = i.next) {
			if(i instanceof WItem) {
				WItem buf = (WItem) i;
				takenSlots += buf.size().x * buf.size().y;
			}
		}
		int allSlots = inv.isz.x * inv.isz.y;
		return allSlots - takenSlots;
	}
}
