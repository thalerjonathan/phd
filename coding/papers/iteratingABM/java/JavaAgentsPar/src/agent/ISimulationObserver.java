package agent;

import java.util.LinkedHashMap;

/**
 * Created by jonathan on 05/12/16.
 */
public interface ISimulationObserver<A extends Agent> {
    double startSimulation();
    double getDt();
    boolean simulationStep(LinkedHashMap<Integer, A> as);
}
