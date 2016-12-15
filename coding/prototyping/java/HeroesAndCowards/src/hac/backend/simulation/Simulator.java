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

    public List<List<Agent>> simulate(SimulationConfig cfg,
                                      List<Agent> as) {
        List<Integer> iterationIndices = new ArrayList<>();
        for (int i = 0; i < as.size(); ++i) {
            iterationIndices.add( i );
        }

        List<List<Agent>> allAgentSteps = new ArrayList<>();
        allAgentSteps.add( as );

        for (int i = 0; i < cfg.steps; ++i) {
            as = this.internalIteration(cfg, iterationIndices, as);
            allAgentSteps.add( as );
        }

        return allAgentSteps;
    }

    public List<Agent> simulateWithObserver(SimulationConfig cfg,
                                            List<Agent> as,
                                            ISimulationObserver o) {
        List<Integer> iterationIndices = new ArrayList<>();
        for (int i = 0; i < as.size(); ++i) {
            iterationIndices.add( i );
        }

        double dt = o.startSimulation();

        while(o.simulationStep(as)) {
            as = this.internalIteration(cfg, iterationIndices, as);
            dt = o.getDt();
        }

        return as;
    }

    private List<Agent> internalIteration(SimulationConfig cfg,
                                          List<Integer> iterationIndices,
                                          List<Agent> as) {
        if (cfg.randomTraversal)
            Collections.shuffle( iterationIndices, this.r );

        if (cfg.simultaneousUpdates)
            as = this.nextStepSimultaneous(as, iterationIndices, cfg);
        else
            as = this.nextStepConsecutive(as, iterationIndices, cfg);

        return as;
    }

    // NOTE: this creates updates without freezing
    private List<Agent> nextStepConsecutive(List<Agent> as, List<Integer> iterationIndices, SimulationConfig cfg) {
        for (Integer i : iterationIndices) {
            Agent a = as.get( i );
            a.step(cfg.dt, cfg.worldType, cfg.noisyDirection, cfg.noisyStepWidth, this.r);
        }

        return as;
    }

    // NOTE: all agents update simultaneous by 'freezing' the state and working on the frozen states thus looking like
    //       all agents moved at the same time.
    private List<Agent> nextStepSimultaneous(List<Agent> as, List<Integer> iterationIndices, SimulationConfig cfg) {
        List<Agent> nextAgents = new ArrayList<>( as );
        Map<Integer, Agent> agentIdMapping = new HashMap<>();

        for (Integer i : iterationIndices) {
            Agent a = as.get( i );
            // NOTE: to 'freeze' the states we work on copies of agents which will prevent the referenced friends and enemies to be updated indirectly in this step
            // NOTE: this is both the strength and the weakness of java and using references (aliasing). We can never
            // guarantee that no update to a reference happens
            Agent an = a.stepImmutable( cfg.dt, cfg.worldType, cfg.noisyDirection, cfg.noisyStepWidth, this.r );

            nextAgents.set( i, an );
            agentIdMapping.put(an.getId(), an);
        }

        // NOTE: replace the old instances of enemy/friend with new ones - id stays the same
        for (Integer i : iterationIndices) {
            Agent a = nextAgents.get( i );
            Agent enemy = agentIdMapping.get( a.getEnemy().getId() );
            Agent friend = agentIdMapping.get( a.getFriend().getId() );

            a.setEnemy( enemy );
            a.setFriend( friend );
        }

        return nextAgents;
    }
}
