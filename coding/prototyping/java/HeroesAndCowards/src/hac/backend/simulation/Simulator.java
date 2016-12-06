package hac.backend.simulation;

import hac.backend.Utils;
import hac.backend.agent.Agent;
import hac.backend.agent.Vector;

import java.util.*;

/**
 * Created by jonathan on 05/12/16.
 */
public class Simulator {

    private Random r;

    public Simulator(long seed) {
        this.r = new Random(seed);
    }

    public List<Agent> createRandomAgents(int count, double heroDistribution) {
        List<Agent> as = new ArrayList<>();

        // NOTE: need to create them first and then set their enemies and friends because only then all available
        for (int i = 0; i < count; ++i) {
            double x = this.r.nextDouble();
            double y = this.r.nextDouble();

            Agent a = new Agent( i );
            a.setHero( this.r.nextDouble() <= heroDistribution );
            a.setPos(new Vector(x,y));

            as.add(a);
        }

        for (int i = 0; i < count; ++i) {
            Agent a = as.get( i );

            Agent friend = Utils.drawRandomIgnoring(as, new Agent[] { a }, this.r);
            Agent enemy = Utils.drawRandomIgnoring(as, new Agent[] { a, friend }, this.r);

            a.setFriend( friend );
            a.setEnemy( enemy );
        }

        return as;
    }

    public List<List<Agent>> simulate(boolean randomTraversal,
                                      boolean simultaneousUpdates,
                                      WorldType wt,
                                      List<Agent> as,
                                      int steps,
                                      double dt) {
        List<List<Agent>> allAgentSteps = new ArrayList<>();
        allAgentSteps.add( as );

        for (int i = 0; i < steps; ++i) {
            as = this.internalIteration(randomTraversal, simultaneousUpdates, wt, as, dt);
            allAgentSteps.add( as );
        }

        return allAgentSteps;
    }

    public List<Agent> simulateWithObserver(boolean randomTraversal,
                                            boolean simultaneousUpdates,
                                            WorldType wt,
                                            List<Agent> as,
                                            ISimulationObserver o) {
        double dt = o.startSimulation();

        while(o.simulationStep(as, wt)) {
            as = this.internalIteration(randomTraversal, simultaneousUpdates, wt, as, dt);
            dt = o.getDt();
        }

        return as;
    }

    private List<Agent> internalIteration(boolean randomTraversal,
                                          boolean simultaneousUpdates,
                                          WorldType wt,
                                          List<Agent> as,
                                          double dt) {
        if (randomTraversal)
            Collections.shuffle( as, this.r );

        if (simultaneousUpdates)
            as = this.nextStepSimultaneous(as, dt, wt);
        else
            as = this.nextStepConsecutive(as, dt, wt);

        return as;
    }

    // NOTE: this creates updates without freezing
    private List<Agent> nextStepConsecutive(List<Agent> as, double dt, WorldType wt) {
        for (Agent a : as) {
            a.step(dt, wt);
        }

        return as;
    }

    // NOTE: all agents update simultaneous by 'freezing' the state and working on the frozen states thus looking like
    //       all agents moved at the same time.
    private List<Agent> nextStepSimultaneous(List<Agent> as, double dt, WorldType wt) {
        List<Agent> nextAgents = new ArrayList<>();
        Map<Integer, Agent> agentIdMapping = new HashMap<>();

        for (Agent a : as) {
            // NOTE: to 'freeze' the states we work on copies of agents which will prevent the referenced friends and enemies to be updated indirectly in this step
            // NOTE: this is both the strength and the weakness of java and using references (aliasing). We can never
            // guarantee that no update to a reference happens
            Agent an = new Agent( a );
            an.step( dt, wt );

            nextAgents.add( an );
            agentIdMapping.put(an.getId(), an);
        }

        // NOTE: replace the old instances of enemy/friend with new ones - id stays the same
        for (Agent a : nextAgents) {
            Agent enemy = nextAgents.get( a.getEnemy().getId() );
            Agent friend = nextAgents.get( a.getFriend().getId() );

            a.setEnemy( enemy );
            a.setFriend( friend );
        }

        return nextAgents;
    }
}
