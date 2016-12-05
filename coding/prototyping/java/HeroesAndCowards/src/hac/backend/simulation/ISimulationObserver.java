package hac.backend.simulation;

import hac.backend.agent.Agent;

import java.util.List;

/**
 * Created by jonathan on 05/12/16.
 */
public interface ISimulationObserver {
    double startSimulation();
    double getDt();
    boolean simulationStep(List<Agent> as, WorldType wt);
}
