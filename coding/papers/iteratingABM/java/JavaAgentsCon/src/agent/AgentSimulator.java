package agent;

import java.util.*;
import java.util.concurrent.*;

/**
 * Created by jonathan on 20/01/17.
 */
public class AgentSimulator<A extends Agent, E> {

    private double time;
    private ExecutorService executor;

    public AgentSimulator() {
        executor = Executors.newFixedThreadPool(3);
    }

    public LinkedHashMap<Integer, A> simulateWithObserver(List<A> as,
                                        E env,
                                        double dt,
                                        ISimulationObserver<A> o) throws InterruptedException, CloneNotSupportedException, ExecutionException {
        LinkedHashMap<Integer, A> om = createOrderedMap(as);

        while(o.simulationStep(om)) {
            om = this.nextStepConcurrent(om, env, dt);
        }

        return om;
    }

    private LinkedHashMap<Integer, A> nextStepConcurrent(LinkedHashMap<Integer, A> om,
                                                         E env,
                                                         double delta) throws CloneNotSupportedException, ExecutionException, InterruptedException {
        this.time = this.time + delta;

        List<Future<Void>> agentFutures = new ArrayList<>( om.size() );

        // start agent-computations
        Iterator<Map.Entry<Integer, A>> iter = om.entrySet().iterator();
        while ( iter.hasNext() ) {
            Map.Entry<Integer, A> e = iter.next();
            A a = e.getValue();

            Future<Void> af = executor.submit(new Callable<Void>() {
                @Override
                public Void call() throws Exception {
                    a.step(time, delta, env);

                    return null;
                }
            });

            agentFutures.add(af);
        }

        // wait for the results
        for (Future<Void> f : agentFutures ) {
            f.get();
        }

        return om;
    }

    private LinkedHashMap<Integer, A> createOrderedMap(List<A> as) {
        LinkedHashMap<Integer, A> om = new LinkedHashMap<>();

        for (A a : as) {
            om.put(a.getId(), a);
        }

        return om;
    }
}
