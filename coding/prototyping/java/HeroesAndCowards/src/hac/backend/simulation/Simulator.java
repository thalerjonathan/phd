package hac.backend.simulation;

import hac.backend.Utils;
import hac.backend.agent.Agent;
import hac.backend.agent.Vector;

import java.util.*;

/**
 * Created by jonathan on 05/12/16.
 */
public class Simulator {

    public Simulator() {
    }

    public List<Agent> createRandomAgents(int count, double heroDistribution) {
        List<Agent> as = new ArrayList<>();

        // NOTE: need to create them first and then set their enemies and friends because only then all available
        for (int i = 0; i < count; ++i) {
            double x = Math.random();
            double y = Math.random();
            Agent a = new Agent( i );
            a.setHero( Math.random() <= heroDistribution );
            a.setPos( new Vector(x,y));

            as.add(a);
        }

        for (int i = 0; i < count; ++i) {
            Agent a = as.get( i );

            Agent friend = Utils.drawRandomIgnoring(as, new Agent[] { a });
            Agent enemy = Utils.drawRandomIgnoring(as, new Agent[] { a, friend });

            a.setFriend( friend );
            a.setEnemy( enemy );
        }

        return as;
    }

    public List<List<Agent>> simulate(boolean randomTraversal,
                                      boolean newStateVisible,
                                      WorldType wt,
                                      List<Agent> as,
                                      int steps,
                                      double dt) {
        List<List<Agent>> allAgentSteps = new ArrayList<>();
        allAgentSteps.add( as );

        for (int i = 0; i < steps; ++i) {
            if (randomTraversal)
                Collections.shuffle( as );

            if (newStateVisible)
                as = this.nextStep(as, dt, wt);
            else
                as = this.nextStepCopy(as, dt, wt);

            allAgentSteps.add( as );
        }

        return allAgentSteps;
    }

    public List<Agent> simulateWithObserver(boolean randomTraversal,
                                            boolean newStateVisible,
                                            WorldType wt,
                                            List<Agent> as,
                                            ISimulationObserver o) {
        double dt = o.startSimulation();

        while(o.simulationStep(as, wt)) {
            if (randomTraversal)
                Collections.shuffle( as );

            if (newStateVisible)
                as = this.nextStep(as, dt, wt);
            else
                as = this.nextStepCopy(as, dt, wt);

            dt = o.getDt();
        }

        return as;
    }

    // NOTE: this creates updates without freezing
    private List<Agent> nextStep(List<Agent> as, double dt, WorldType wt) {
        for (Agent a : as) {
            a.step(dt, wt);
        }

        return as;
    }

    // NOTE: all agents update on 'frozen' (old) states and result will be the updated one
    private List<Agent> nextStepCopy(List<Agent> as, double dt, WorldType wt) {
        List<Agent> nextAgents = new ArrayList<>();
        Map<Integer, Agent> agentIdMapping = new HashMap<>();

        //Collections.shuffle( as );

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
