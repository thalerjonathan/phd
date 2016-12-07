import hac.backend.Utils;
import hac.backend.agent.Agent;
import hac.backend.simulation.ISimulationObserver;
import hac.backend.simulation.SimulationConfig;
import hac.backend.simulation.Simulator;
import hac.backend.simulation.WorldType;
import hac.gui.HACFrontend;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * Created by jonathan on 05/12/16.
 */
public class Main {
    public static void main(String[] args) throws InterruptedException, IOException {
        int agentSize = 5;
        double epsilon = 0.1;

        SimulationConfig simCfg1 = new SimulationConfig();
        simCfg1.agentCount = 6000;
        simCfg1.simulationRandomSeed = 42;
        simCfg1.heroesDistribution = 0.25;
        simCfg1.dt = 0.01;
        simCfg1.randomTraversal = false;
        simCfg1.simultaneousUpdates = false;
        simCfg1.worldType = WorldType.BORDER;
        simCfg1.steps = 0; //1000_000;

        SimulationConfig simCfg2 = new SimulationConfig( simCfg1 );
        simCfg2.randomTraversal = true;

        //observeInfiniteSimulation( simCfg1, agentSize );
        //observeAllSteps( simCfg1, agentSize );
        //observeFinalStep( simCfg1, agentSize );
        //observeDualLockstepSimulation( simCfg1, simCfg2, epsilon, agentSize );

        int differenceAfterSteps = compareDualLockstepSimulation( simCfg1, simCfg2, epsilon, agentSize );
        if ( differenceAfterSteps > 0 ) {
            System.out.println("Found differences in Simulations after " + differenceAfterSteps + " steps using epsilon of " + epsilon );
        } else {
            System.out.println("No differences in Simulations found using epsilon of " + epsilon);
        }
    }

    public static void observeInfiniteSimulation( SimulationConfig simCfg, int agentSize ) {
        Simulator hac = new Simulator( simCfg.simulationRandomSeed );
        HACFrontend fe = new HACFrontend( agentSize, true );
        List<Agent> asInit = hac.createRandomAgents( simCfg.agentCount, simCfg.heroesDistribution );

        /* NOTE: use this code to view simulation interactively.
           NOTE: to have REPRODUCEABLE runs we MUST NOT ALLOW the GUI/RENDERING to drive our dt because GUI/RENDERING
                 depends strongly on OS and scheduling and could and IS subject to non-deterministic changes and will
                 NEVER be constant although the scenery may be of constant complexity!
                 Thus introduce indirection and return constant dt and feed output to rendering => result in reproducible runs
        */
        hac.simulateWithObserver( simCfg.randomTraversal,
                simCfg.simultaneousUpdates,
                simCfg.worldType, asInit, new ISimulationObserver() {
            @Override
            public double startSimulation() {
                return 0.0;
            }

            @Override
            public double getDt() {
                return simCfg.dt;
            }

            @Override
            public boolean simulationStep(List<Agent> as, WorldType wt) {
                return fe.simulationStep(as, wt);
            }
        });

        fe.dispose();
    }

    public static void observeAllSteps( SimulationConfig simCfg, int agentSize ) {
        Simulator hac = new Simulator( simCfg.simulationRandomSeed );
        HACFrontend fe = new HACFrontend( agentSize, true );
        List<Agent> as = hac.createRandomAgents( simCfg.agentCount, simCfg.heroesDistribution );

        for (int i = 0; i < simCfg.steps; ++i) {
            // NOTE: use this code to calculate a number of steps and then display the final result
            List<List<Agent>> allAsSteps = hac.simulate(simCfg.randomTraversal,
                    simCfg.simultaneousUpdates, simCfg.worldType, as, 1, simCfg.dt);
            as = allAsSteps.get(1);

            fe.simulationStep(as, simCfg.worldType);
        }
    }

    public static void observeFinalStep( SimulationConfig simCfg, int agentSize ) {
        Simulator hac = new Simulator( simCfg.simulationRandomSeed );
        HACFrontend fe = new HACFrontend( agentSize, true );
        List<Agent> asInit = hac.createRandomAgents( simCfg.agentCount, simCfg.heroesDistribution );

        // NOTE: use this code to calculate a number of steps and then display the final result
        List<List<Agent>> allAsSteps = hac.simulate( simCfg.randomTraversal,
                simCfg.simultaneousUpdates, simCfg.worldType, asInit, simCfg.steps, simCfg.dt );
        List<Agent> finalIteration = allAsSteps.get(allAsSteps.size() -1);

        fe.simulationStep(finalIteration, simCfg.worldType);
    }

    public static void observeDualLockstepSimulation( SimulationConfig simCfg1, SimulationConfig simCfg2, double eps, int agentSize ) {
       Simulator hac1 = new Simulator( simCfg1.simulationRandomSeed );
        HACFrontend fe1 = new HACFrontend( agentSize, true );
        List<Agent> as1 = hac1.createRandomAgents( simCfg1.agentCount, simCfg1.heroesDistribution );

        Simulator hac2 = new Simulator( simCfg2.simulationRandomSeed );
        HACFrontend fe2 = new HACFrontend( agentSize, true );
        List<Agent> as2 = hac2.createRandomAgents( simCfg2.agentCount, simCfg2.heroesDistribution );

        boolean cont1 = true;
        boolean cont2 = true;

        while ( cont1 && cont2 ) {
            List<List<Agent>> allAsSteps1 = hac1.simulate( simCfg1.randomTraversal,
                    simCfg1.simultaneousUpdates, simCfg1.worldType, as1, 1, simCfg1.dt);

            List<List<Agent>> allAsSteps2 = hac2.simulate( simCfg2.randomTraversal,
                    simCfg2.simultaneousUpdates, simCfg2.worldType, as2, 1, simCfg2.dt);

            as1 = allAsSteps1.get(1);
            as2 = allAsSteps2.get(1);

            cont1 = fe1.simulationStep(as1, simCfg1.worldType);
            cont2 = fe2.simulationStep(as2, simCfg2.worldType);
        }
    }

    public static int compareDualLockstepSimulation( SimulationConfig simCfg1, SimulationConfig simCfg2, double eps, int agentSize ) {
        if ( simCfg1.agentCount != simCfg2.agentCount ) {
            throw new RuntimeException("To compare simulations, both need same number of agents");
        }

        int steps = Math.max( simCfg1.steps, simCfg2.steps );

        Simulator hac1 = new Simulator( simCfg1.simulationRandomSeed );
        HACFrontend fe1 = new HACFrontend( agentSize, false );
        List<Agent> as1 = hac1.createRandomAgents( simCfg1.agentCount, simCfg1.heroesDistribution );

        Simulator hac2 = new Simulator( simCfg2.simulationRandomSeed );
        List<Agent> as2 = hac2.createRandomAgents( simCfg2.agentCount, simCfg2.heroesDistribution );
        HACFrontend fe2 = new HACFrontend( agentSize, false );

        double error = 0.0;

        // NOTE: infinite loop when both are configured to 0
        int s = 1;
        while( true ) {
            List<List<Agent>> allAsSteps1 = hac1.simulate( simCfg1.randomTraversal,
                    simCfg1.simultaneousUpdates, simCfg1.worldType, as1, 1, simCfg1.dt);

            List<List<Agent>> allAsSteps2 = hac2.simulate( simCfg2.randomTraversal,
                    simCfg2.simultaneousUpdates, simCfg2.worldType, as2, 1, simCfg2.dt);

            as1 = allAsSteps1.get(1);
            as2 = allAsSteps2.get(1);

            boolean errorChanged = false;

            for (int i = 0; i < as1.size(); ++i) {
                Agent a1 = as1.get( i );
                Agent a2 = as2.get( i );

                double xDelta = Math.abs( a1.getPos().getX() - a2.getPos().getX() );
                double yDelta = Math.abs( a1.getPos().getY() - a2.getPos().getY() );
                double maxDelta = Math.max( xDelta, yDelta );
                if ( maxDelta > error ) {
                    error = maxDelta;
                    errorChanged = true;
                }

                if ( (xDelta > eps) || (yDelta > eps) ) {
                    if ( errorChanged )
                        System.out.println("Error increase at " + s + " steps: " + error);

                    fe1.setVisible( true );
                    fe2.setVisible( true );

                    fe1.simulationStep( as1, simCfg1.worldType );
                    fe2.simulationStep( as2, simCfg2.worldType );

                    return s;
                }
            }

            if ( errorChanged )
                System.out.println("Error increase at " + s + " steps: " + error);

            if ( s++ == steps ) {
                break;
            }
        }

        return 0;
    }
}
