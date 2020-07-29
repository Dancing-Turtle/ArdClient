package haven.purus.pbot;

import haven.Coord;
import haven.GItem;
import haven.ItemInfo;
import haven.Loading;
import haven.UI;
import haven.WItem;
import haven.res.ui.tt.q.qbuff.QBuff;

import java.util.regex.Pattern;

public class PBotItem {

    public GItem gitem;
    public WItem witem;
    public final UI ui;

    public PBotItem(GItem gitem, WItem witem) {
        this.gitem = gitem;
        this.witem = witem;
        this.ui = witem.ui;
    }

    public PBotItem(WItem witem) {
        this.gitem = witem.item;
        this.witem = witem;
        this.ui = witem.ui;
    }

    /**
     * Returns contents of the item
     *
     * @return Contents of the item
     */
    public ItemInfo.Contents getContents() {
        synchronized (ui) {
            try {
                for (ItemInfo info : gitem.info())
                    if (info instanceof ItemInfo.Contents)
                        return (ItemInfo.Contents) info;
            } catch (Loading ignored) {
            }
        }
        return null;
    }


    /**
     * Take the item to hand
     *
     * @param wait Wait for item to appear on hand
     */
    public void takeItem(boolean wait) {
        gitem.wdgmsg("take", getInvLoc());
        if (wait) {
            while (PBotUtils.getItemAtHand(ui) == null) {
                PBotUtils.sleep(25);
            }
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
     *
     * @param mod modifier for example 1 = shift etc.
     */
    public void itemact(int mod) {
        gitem.wdgmsg("itemact", mod);
    }

    /**
     * Get an amount of something such as seeds in a stack
     *
     * @return Amount of something in the item
     */
    public int getAmount() {
        int ret = -1;
        synchronized (ui) {
            for (ItemInfo o : gitem.info()) {
                if (o instanceof GItem.Amount)
                    ret = ((GItem.Amount) o).itemnum();
            }
        }
        return ret;
    }

    /**
     * Get location of the item in inventory, ie. 5,4
     *
     * @return Coord-object, access x and y for coords
     */
    public Coord getInvLoc() {
        return witem.c.div(33);
    }

    /**
     * Returns name of the item if it exists
     *
     * @return Name of item or null
     */
    public String getName() {
        synchronized (ui) {
            try {
                for (Object o : gitem.info().toArray()) {
                    if (o instanceof ItemInfo.Name)
                        return ((ItemInfo.Name) o).str.text;
                }
            } catch (Loading l) {

            }
        }
        return null;
    }

    /**
     * Returns resname of the item
     *
     * @return Resname of the item
     */
    public String getResname() {
        try {
            return gitem.getres().name;
        } catch (Loading l) {
        }
        return null;
    }

    /**
     * Returns quality of the item, or -1 if quality was not found
     *
     * @return Quality of the item
     */
    public double getQuality() {
        QBuff buff = gitem.quality();
        if (buff == null)
            return -1;
        else
            return buff.q;
    }

    /**
     * Returns quality of the item, wait until the quality is calculated for the item
     *
     * @return Quality of the item
     */
    public double getQuality2() {
        while (gitem.quality() == null)
            PBotUtils.sleep(5);
        return gitem.quality().q;
    }

    /**
     * Checks if player can drink from the item
     * Only checks water and tea
     *
     * @return True if player can drink, else false
     */
    public boolean canDrinkFrom() {
        synchronized (gitem.ui) {
            Pattern liquidPattern = Pattern.compile(String.format("[0-9.]+ l of (%s)",
                    String.join("|", new String[]{"Water", "Piping Hot Tea", "Tea"}), Pattern.CASE_INSENSITIVE));
            ItemInfo.Contents contents = getContents();
            if (contents != null && contents.sub != null) {
                for (ItemInfo info : contents.sub) {
                    if (info instanceof ItemInfo.Name) {
                        ItemInfo.Name name = (ItemInfo.Name) info;
                        if (name.str != null && liquidPattern.matcher(name.str.text).matches())
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
