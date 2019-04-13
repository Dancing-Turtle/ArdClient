package haven.automation;


import static haven.OCache.posres;

import java.util.HashSet;
import java.util.Set;

import haven.Coord2d;
import haven.FastMesh;
import haven.FlowerMenu;
import haven.GameUI;
import haven.Gob;
import haven.Loading;
import haven.ResDrawable;
import haven.Resource;

public class TrellisHarvest implements Runnable {
    private GameUI gui;
    private Set<String> plants = new HashSet<>(5);

    public TrellisHarvest(GameUI gui) {
        this.gui = gui;
        plants.add("gfx/terobjs/plants/wine");
        plants.add("gfx/terobjs/plants/pepper");
        plants.add("gfx/terobjs/plants/hops");
        plants.add("gfx/terobjs/plants/peas");
        plants.add("gfx/terobjs/plants/cucumber");
    }

    @Override
    public void run() {
        Gob plant = null;
        synchronized (gui.map.glob.oc) {
            for (Gob gob : gui.map.glob.oc) {
                try {
                    Resource res = gob.getres();
                    if (res != null && plants.contains(res.name)) {
                        if (gob.cropstgmaxval == 0) {
                            if (gob.cropstgmaxval == 0) {
                                for (FastMesh.MeshRes layer : gob.getres().layers(FastMesh.MeshRes.class)) {
                                    int stg = layer.id / 10;
                                    if (stg > gob.cropstgmaxval)
                                        gob.cropstgmaxval = stg;
                                }
                            }
                        }
                        int stage = gob.sdt();
                        if (stage == gob.cropstgmaxval) {
                            Coord2d plc = gui.map.player().rc;
                            if ((plant == null || gob.rc.dist(plc) < plant.rc.dist(plc)))
                                plant = gob;
                        }
                    }
                } catch (Loading l) {
                }
            }
        }

        if (plant == null)
            return;

        FlowerMenu.setNextSelection("Harvest");
        gui.map.wdgmsg("click", plant.sc, plant.rc.floor(posres), 3, 0, 0, (int) plant.id, plant.rc.floor(posres), 0, -1);
    }
}
