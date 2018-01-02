package agent;

import java.util.*;
import java.util.concurrent.*;

/**
 * Created by jonathan on 20/01/17.
 */
public class AgentSimulator<A extends Agent> implements Runnable {

    private List<A> as;

    private ExecutorService executor;

    private boolean runObserver;
    private Thread observerThread;
    private ISimulationObserver<A> o;

    public AgentSimulator() {
        executor = Executors.newFixedThreadPool(3);
    }


    public List<Future<Void>> simulateWithObserver(List<A> as,
                                                   double dt,
                                                   ISimulationObserver<A> o) throws InterruptedException, CloneNotSupportedException, ExecutionException {

        List<Future<Void>> agentFutures;
        List<Callable<Void>> agentCallables = new ArrayList<>( as.size() );

        // start agent-computations
        for (A a : as ) {
            agentCallables.add( new AgentTask(a, dt));
        }

        agentFutures = executor.invokeAll(agentCallables);

        this.o = o;
        this.as = as;
        this.runObserver = true;
        this.observerThread = new Thread(this);
        this.observerThread.start();

        return agentFutures;
    }

    /*
        public List<AgentThread> simulateWithObserver(List<A> as,
                                                       double dt,
                                                       ISimulationObserver<A> o) throws InterruptedException, CloneNotSupportedException, ExecutionException {

            List<AgentThread> agentThreads = new ArrayList<>( as.size() );

            // start agent-computations
            for (A a : as ) {
                AgentThread at = new AgentThread(a, dt);
                at.start();
                agentThreads.add(at);
            }

            this.o = o;
            this.as = as;
            this.runObserver = true;
            this.observerThread = new Thread(this);
            this.observerThread.start();

            return agentThreads;
        }

        public class AgentThread extends Thread {
            private A a;
            private double dt;

            public AgentThread(A a, double dt) {
                this.a = a;
                this.dt = dt;
            }

            @Override
            public void run() {
                a.run(dt);
            }
        }
    */
    private class AgentTask implements Callable<Void> {
        private A a;
        private double dt;

        public AgentTask(A a, double dt) {
            this.a = a;
            this.dt = dt;
        }

        @Override
        public Void call() throws Exception {
            a.run(dt);
            return null;
        }
    }

    @Override
    public void run() {
        while (this.runObserver) {
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            o.simulationStep(as);
        }
    }
}
