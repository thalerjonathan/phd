import hac.backend.agent.Agent;
import hac.backend.simulation.Simulator;
import hac.backend.simulation.WorldType;
import hac.gui.HACFrontend;

import java.io.IOException;
import java.util.List;

/**
 * Created by jonathan on 05/12/16.
 */
public class Main {
    public static void main(String[] args) throws InterruptedException, IOException {
        int agentCount = 1_000_000;
        double heroesDistribution = 0.01;

        boolean randomTraversal = true;
        boolean newStateVisible = true;

        int steps = 100;
        double dt = 0.5;

        WorldType wt = WorldType.BORDER;

        Simulator hac = new Simulator();
        HACFrontend fe = new HACFrontend();
        List<Agent> asInit = hac.createRandomAgents(agentCount, heroesDistribution);

        // TODO: define random-seed to be reproducible

        // NOTE: use this code to calculate a number of steps and then display the final result
        /*
        List<List<Agent>> allAsSteps = hac.simulate(randomTraversal,
                newStateVisible, wt, asInit, steps, dt);
        List<Agent> finalIteration = allAsSteps.get(allAsSteps.size() -1);
        fe.simulationStep( finalIteration, wt);
        */

        // NOTE: use this code to view simulation interactively
        hac.simulateWithObserver( randomTraversal, newStateVisible, wt, asInit, fe );
        fe.dispose();
    }
}
