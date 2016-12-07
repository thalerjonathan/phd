package hac.backend.simulation;

/**
 * Created by jonathan on 07/12/16.
 */
public class SimulationConfig {

    public SimulationConfig() {}

    public SimulationConfig( SimulationConfig cpy ) {
        this.agentCount = cpy.agentCount;
        this.simulationRandomSeed = cpy.simulationRandomSeed;
        this.heroesDistribution = cpy.heroesDistribution;
        this.dt = cpy.dt;
        this.randomTraversal = cpy.randomTraversal;
        this.simultaneousUpdates = cpy.simultaneousUpdates;
        this.steps = cpy.steps;
        this.worldType = cpy.worldType;
    }

    public int agentCount;
    public long simulationRandomSeed;
    public double heroesDistribution;
    public double dt;
    public boolean randomTraversal;
    public boolean simultaneousUpdates;
    public int steps;
    public WorldType worldType;
}
