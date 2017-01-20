package agent;

import java.util.List;

/**
 * Created by jonathan on 05/12/16.
 */
public interface ISimulationObserver<A extends Agent> {
    double startSimulation();
    double getDt();
    boolean simulationStep(List<A> as);
}
