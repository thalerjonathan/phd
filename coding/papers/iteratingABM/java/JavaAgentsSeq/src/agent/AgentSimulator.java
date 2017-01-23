package agent;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

/**
 * Created by jonathan on 20/01/17.
 */
public class AgentSimulator<A extends Agent, E> {

    private double time;

    public List<List<A>> simulate(List<A> as,
                                  E env,
                                  int stepCount,
                                  double dt,
                                  Random rng) {
        List<Integer> iterationIndices = createIterationIndices(as.size());
        List<List<A>> allAgentSteps = new ArrayList<>();
        allAgentSteps.add( as );

        for (int i = 0; i < stepCount; ++i) {
            as = this.internalIteration(as, env, iterationIndices, rng, dt);
            allAgentSteps.add( as );
        }

        return allAgentSteps;
    }

    public List<A> simulateWithObserver(List<A> as,
                                        E env,
                                        double dt,
                                        Random rng,
                                        ISimulationObserver<A> o) {
        List<Integer> iterationIndices = createIterationIndices(as.size());

        while(o.simulationStep(as)) {
            as = this.internalIteration(as, env, iterationIndices, rng, dt);
        }

        return as;
    }

    private List<A> internalIteration(List<A> as,
                                      E env,
                                      List<Integer> iterationIndices,
                                      Random rng,
                                      double delta) {
        if (null != rng)
            Collections.shuffle(iterationIndices, rng);

        this.time = this.time + delta;

        for (Integer i : iterationIndices) {
            A a = as.get( i );
            a.step(this.time, delta, env);
        }

        return as;
    }

    private static List<Integer> createIterationIndices(int count) {
        List<Integer> iterationIndices = new ArrayList<>();
        for (int i = 0; i < count; ++i) {
            iterationIndices.add( i );
        }

        return iterationIndices;
    }
}
