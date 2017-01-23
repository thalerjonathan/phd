package agent;

import java.util.*;
import java.util.concurrent.*;

/**
 * Created by jonathan on 20/01/17.
 */
public class AgentSimulator<A extends Agent> {

    private double time;
    private ExecutorService executor;

    public AgentSimulator() {
        executor = Executors.newFixedThreadPool(3);
    }

    public List<A> simulateWithObserver(List<A> as,
                                        ISimulationObserver<A> o) throws InterruptedException, CloneNotSupportedException, ExecutionException {
        double dt = o.startSimulation();
        LinkedHashMap<Integer, A> om = createOrderedMap(as);

        while(o.simulationStep(om)) {
            om = this.nextStepSimultaneous(om, dt);
            dt = o.getDt();
        }

        return as;
    }

    private LinkedHashMap<Integer, A> nextStepSimultaneous(LinkedHashMap<Integer, A> om,
                                                           double delta) throws CloneNotSupportedException, ExecutionException, InterruptedException {
        this.time = this.time + delta;

        List<Future<Void>> agentFutures = new ArrayList<>( om.size() );

        // distribute messages
        Iterator<Map.Entry<Integer, A>> iter = om.entrySet().iterator();
        while ( iter.hasNext() ) {
            Map.Entry<Integer, A> e = iter.next();
            A a = e.getValue();

            Iterator<MsgPair> msgIter = a.getOutBox().iterator();
            while ( msgIter.hasNext() ) {
                MsgPair p = msgIter.next();

                om.get(p.agent.getId()).getInBox().add(new MsgPair(a, p.msg));
            }

            a.getOutBox().clear();
        }

        // start agent-computations
        iter = om.entrySet().iterator();
        while ( iter.hasNext() ) {
            Map.Entry<Integer, A> e = iter.next();
            A a = e.getValue();

            Future<Void> af = executor.submit(new Callable<Void>() {
                @Override
                public Void call() throws Exception {
                    a.step(time, delta);

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
