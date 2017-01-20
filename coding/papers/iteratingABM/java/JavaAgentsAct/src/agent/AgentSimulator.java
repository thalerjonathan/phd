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
        executor = Executors.newFixedThreadPool(1);
    }

    public List<Future<Void>> simulateWithObserver(List<A> as,
                                                   double dt,
                                                   ISimulationObserver<A> o) throws InterruptedException, CloneNotSupportedException, ExecutionException {

        List<Future<Void>> agentFutures = new ArrayList<>( as.size() );

        // start agent-computations
        for (A a : as ) {
            Future<Void> af = executor.submit(new Callable<Void>() {
                @Override
                public Void call() throws Exception {
                    a.run(dt);

                    return null;
                }
            });

            agentFutures.add(af);
        }

        this.o = o;
        this.as = as;
        this.runObserver = true;
        this.observerThread = new Thread(this);
        this.observerThread.start();

        return agentFutures;
    }

    @Override
    public void run() {
        while (this.runObserver) {
            try {
                Thread.sleep(20);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            o.simulationStep(as);
        }
    }
}
