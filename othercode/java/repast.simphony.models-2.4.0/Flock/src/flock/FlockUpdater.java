package flock;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.engine.schedule.ScheduleParameters;

/**
 * FlockUpdater uses a thread pool to run batches of Prey.update() on
 *   multiple available CPU cores. 
 * 
 * @author Eric Tatara 
 *
 */
public class FlockUpdater {

	ExecutorService executor;
	ExecutorCompletionService ecs;
	List<Callable> runners;
	
	public FlockUpdater(List<Prey> preyList){
		int cores = Runtime.getRuntime().availableProcessors();
		executor = Executors.newFixedThreadPool(cores+1);  // +1 is optimal
		ecs = new ExecutorCompletionService<>(executor);
		runners = new ArrayList<Callable>();

		// Divide the list containing all Prey into sublists that will be
		//  assigned to individual runners.
		
		int stride =  (int)Math.max(1, Math.round((double)preyList.size() / (double)cores));

		while(!preyList.isEmpty()){
			List subList = new ArrayList();
			for (int i=0; i<stride; i++){
				if (preyList.size() > 0)
					subList.add(preyList.remove(0));
			}
			runners.add(new FlockRunner(subList));
		}
		
		// Schedule the update method
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		ScheduleParameters sp = ScheduleParameters.createRepeating(1, 1); 
		schedule.schedule(sp, this, "update");
		
		// Schedule the shutdown method
		sp = ScheduleParameters.createAtEnd(ScheduleParameters.LAST_PRIORITY); 
		schedule.schedule(sp, this, "shutdown");
	}

	/**
	 *  Each update, submit each runner to the executor service and start them. 
	 */
	public void update(){
		for (Callable runner : runners){
			ecs.submit(runner);
		}
		
		for (Callable runner : runners){
			try {
				ecs.take();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Shut down the executor service
	 */
	public void shutdown(){
		System.out.println("ExecutorService.shutdown()");
		executor.shutdown();
	}
	
	/**
	 * FlockRunner just loops through each Prey that's stored in its 
	 *   list and runs the Prey update method.
	 * 
	 * @author tatara
	 *
	 */
	private class FlockRunner implements Callable{

        private List<Prey> preyList;
        
        public FlockRunner(List<Prey> preyList){
            this.preyList = preyList;
        }
        
        @Override
        public Object call() {
           for (Prey prey : preyList){
        	   prey.update();
           }
           
           return null;  // no result
        }
    }
}