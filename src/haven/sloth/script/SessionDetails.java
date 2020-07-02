package haven.sloth.script;

import haven.*;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

//Sessions are account name & ui based
//Note: scripts are not tied to a single Session, but do start in some session.
@SuppressWarnings("unused")
public class SessionDetails {
    public enum InventoryType {
        MAIN, STUDY, BELT, SUPPLEMENTAL
    }

    public final Context context;
    public final WeakReference<Session> session;

    public int shp, hhp, mhp;
    public int stam;
    public int energy;

    public int north = -1;
    public int south = 1;
    public int west = -1;
    public int east = 1;

    private WeakReference<Charlist> chrlst = null;
    private final Object chrlstlock = new Object();

    private WeakReference<FlowerMenu> fm;
    private final Object fmlock = new Object();

    private WeakReference<Speedget> speed;
    private final Object speedlock =  new Object();

    private WeakReference<GItem> helditem;
    private final Object heldlock = new Object();

    private WeakReference<Inventory> maininv = null;
    private WeakReference<Inventory> studyinv = null;
    private WeakReference<Inventory> beltinv = null;
    //For every other inventory
    private final List<WeakReference<Inventory>> invs = new ArrayList<>();

    //For every stockpile
    private final List<WeakReference<ISBox>> stockpiles = new ArrayList<>();

    //For every VMeter
    private final List<WeakReference<VMeter>> vmeters = new ArrayList<>();

    //For every pointer
    private final List<WeakReference<PointerData>> pointers = new ArrayList<>();

    public SessionDetails(final Session sess) {
        this.context = new Context();
        this.session = new WeakReference<>(sess);
        shp = hhp = mhp = stam = energy = 0;
    }

    public boolean active() {
        final Session sess = session.get();
        return sess != null && sess.alive();
    }

    /*****************************************************************************************
     *  Basics
     *****************************************************************************************/
    public UI getUI() {
        final Session sess = session.get();
        return sess != null ? sess.glob.ui.get() : null;
    }

    public String username() {
        final Session sess = session.get();
        return sess != null ? sess.username : null;
    }

    public String chrname() {
        final UI ui = getUI();
        return ui != null ? ui.gui.chrid : null;
    }

    /*****************************************************************************************
     *  Pointers
     *****************************************************************************************/
    public void attachPointer(final PointerData pointer) {
        synchronized (pointers) {
            pointers.add(new WeakReference<>(pointer));
        }
    }

    public void removePointer(final PointerData pointer) {
        synchronized (pointers) {
            pointers.removeIf(ptr -> ptr.get() == null || ptr.get() == pointer);
        }
    }

    public PointerData[] getPointers() {
        final List<PointerData> ret = new ArrayList<>();
        synchronized (pointers) {
            Iterator<WeakReference<PointerData>> itr = pointers.iterator();
            while(itr.hasNext()) {
                final PointerData ptr = itr.next().get();
                if(ptr != null) {
                    ret.add(ptr);
                } else {
                    itr.remove();
                }
            }
        }
        return ret.toArray(new PointerData[0]);
    }

    /*****************************************************************************************
     *  VMeters
     *****************************************************************************************/
    public void attachVMeter(final VMeter vmeter) {
        synchronized (vmeters) {
            vmeters.add(new WeakReference<>(vmeter));
        }
    }

    public void removeVMeter(final VMeter vmeter) {
        synchronized (vmeters) {
            vmeters.removeIf((vm) -> vm.get() == null || vm.get() == vmeter);
        }
    }

    public VMeter[] getVMeters() {
        final List<VMeter> ret = new ArrayList<>();
        synchronized (vmeters) {
            Iterator<WeakReference<VMeter>> itr = vmeters.iterator();
            while(itr.hasNext()) {
                final VMeter meter = itr.next().get();
                if(meter != null) {
                    ret.add(meter);
                } else {
                    //remove broken/gc'd vmeters
                    itr.remove();
                }
            }
        }
        return ret.toArray(new VMeter[0]);
    }

    /*****************************************************************************************
     *  Stockpiles
     *****************************************************************************************/
    public void attachISBox(final ISBox isbox) {
        synchronized (stockpiles) {
            stockpiles.add(new WeakReference<>(isbox));
        }
    }

    public void removeISBox(final ISBox isbox) {
        synchronized (stockpiles) {
            stockpiles.removeIf((stockpile) -> stockpile.get() == null || stockpile.get() == isbox);
        }
    }

    public ISBox[] getISBoxes() {
        final List<ISBox> ret = new ArrayList<>();
        synchronized (stockpiles) {
            Iterator<WeakReference<ISBox>> itr = stockpiles.iterator();
            while(itr.hasNext()) {
                final ISBox box = itr.next().get();
                if(box != null) {
                    ret.add(box);
                } else {
                    //remove broken/gc'd isboxes
                    itr.remove();
                }
            }
        }
        return ret.toArray(new ISBox[0]);
    }

    /*****************************************************************************************
     *  FlowerMenu
     *****************************************************************************************/
    public void attachFlowermenu(final FlowerMenu menu) {
        synchronized (fmlock) {
            fm = new WeakReference<>(menu);
        }
    }

    public void removeFlowermenu() {
        synchronized (fmlock) {
            fm = null;
        }
    }

    public FlowerMenu getFlowermenu() {
        synchronized (fmlock) {
            return fm != null ? fm.get() : null;
        }
    }

    /*****************************************************************************************
     *  Speedget
     *****************************************************************************************/
    public void attachSpeedget(final Speedget speed) {
        synchronized (speedlock) {
            this.speed = new WeakReference<>(speed);
        }
    }

    public void removeSpeedget() {
        synchronized (speedlock) {
            this.speed = null;
        }
    }

    public Speedget getSpeedget() {
        synchronized (speedlock) {
            return speed != null ? speed.get() : null;
        }
    }

    /*****************************************************************************************
     *  Character Listing
     *****************************************************************************************/
    public void attachCharlist(final Charlist cl) {
        synchronized (chrlstlock) {
            chrlst = new WeakReference<>(cl);
        }
    }

    public void removeCharlist() {
        synchronized (chrlstlock) {
            chrlst = null;
        }
    }

    public Charlist getCharlist() {
        synchronized (chrlstlock) {
            return chrlst != null ? chrlst.get() : null;
        }
    }

    /*****************************************************************************************
     *  Inventory
     *****************************************************************************************/
    public void attachHeldItem(final GItem item) {
        synchronized (heldlock) {
            helditem = new WeakReference<>(item);
        }
    }

    public void removeHeldItem() {
        synchronized (heldlock) {
            helditem = null;
        }
    }

    public GItem getHeldItem() {
        synchronized (heldlock) {
            return helditem != null ? helditem.get() : null;
        }
    }

    /*****************************************************************************************
     *  Inventory
     *****************************************************************************************/
    public Inventory getMainInventory() {
        synchronized (invs) {
            return maininv != null ? maininv.get() : null;
        }
    }

    public Inventory getStudyInventory() {
        synchronized (invs) {
            return studyinv != null ? studyinv.get() : null;
        }
    }

    public Inventory getBeltInventory() {
        synchronized (invs) {
            return beltinv != null ? beltinv.get() : null;
        }
    }

    //Supplemental only
    public int numberOfInventories() {
        synchronized (invs) {
            return invs.size();
        }
    }

    //Supplemental Only
    public Inventory getInventory(final int i) {
        synchronized (invs) {
            if(i < invs.size())
                return invs.get(i).get();
            else
                return null;
        }
    }

    public void attachInventory(final Inventory inv, final InventoryType type) {
        synchronized (invs) {
            switch (type) {
                case MAIN:
                    maininv = new WeakReference<>(inv);
                    break;
                case BELT:
                    beltinv = new WeakReference<>(inv);
                    break;
                case STUDY:
                    studyinv = new WeakReference<>(inv);
                    break;
                case SUPPLEMENTAL:
                    invs.add(new WeakReference<>(inv));
                    break;
            }
        }
    }

    public void removeInventory(final Inventory inv, final InventoryType type) {
        synchronized (invs) {
            switch (type) {
                case MAIN:
                    maininv = null;
                    break;
                case BELT:
                    beltinv = null;
                    break;
                case STUDY:
                    studyinv = null;
                    break;
                case SUPPLEMENTAL:
                    //This will also clean up any references that are no longer valid
                    invs.removeIf(i -> i.get() == null || i.get() == inv);
                    break;
            }
        }
    }

    //Includes Main and Belt, but not study
    public Inventory[] inventories() {
        final List<Inventory> ret = new ArrayList<>();
        synchronized (invs) {
            if (maininv != null) {
                final Inventory main = maininv.get();
                if (main != null) {
                    ret.add(main);
                } else {
                    maininv = null;
                }
            }

            if (beltinv != null) {
                final Inventory belt = beltinv.get();
                if (belt != null) {
                    ret.add(belt);
                } else {
                    beltinv = null;
                }
            }

            final Iterator<WeakReference<Inventory>> itr = invs.iterator();
            while (itr.hasNext()) {
                final Inventory inv = itr.next().get();
                if (inv != null)
                    ret.add(inv);
                else// cleanup bad references
                    itr.remove();
            }
        }
        return ret.toArray(new Inventory[0]);
    }


}