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

    // WARNING: this is EXTREMELY slow (but its more functional-style ;))
    private LinkedHashMap<Integer, A> nextStepSimultaneousCopy(LinkedHashMap<Integer, A> omOld,
                                                               double delta) throws CloneNotSupportedException, ExecutionException, InterruptedException {
        this.time = this.time + delta;

        LinkedHashMap<Integer, A> omNew = new LinkedHashMap<>( omOld.size() );
        List<Future<Void>> agentFutures = new ArrayList<>( omOld.size() );

        // clone agents
        Iterator<Map.Entry<Integer, A>> iter = omOld.entrySet().iterator();
        while ( iter.hasNext() ) {
            Map.Entry<Integer, A> e = iter.next();
            A oldAgent = e.getValue();
            A clonedAgent = (A) oldAgent.clone();

            omNew.put( clonedAgent.getId(), clonedAgent );
        }

        // start agent-computations
        iter = omNew.entrySet().iterator();
        while ( iter.hasNext() ) {
            Map.Entry<Integer, A> e = iter.next();
            A a = e.getValue();

            Future<Void> af = executor.submit(new Callable<Void>() {
                @Override
                public Void call() throws Exception {
                    Iterator<Map.Entry<Integer, A>> iter = omOld.entrySet().iterator();
                    while ( iter.hasNext() ) {
                        Map.Entry<Integer, A> e = iter.next();
                        Agent sendingAgent = e.getValue();

                        Iterator<MsgPair> msgIter = sendingAgent.getOutBox().iterator();
                        while ( msgIter.hasNext() ) {
                            MsgPair p = msgIter.next();

                            if ( p.agentId == a.getId() ) {
                                a.getInBox().add(new MsgPair(sendingAgent.getId(), p.msg));
                            }
                        }
                    }

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

        return omNew;
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

                om.get(p.agentId).getInBox().add(new MsgPair(a.getId(), p.msg));
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
