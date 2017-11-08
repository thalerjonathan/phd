package flock;

import java.util.ArrayList;
import java.util.List;

import repast.simphony.context.Context;
import repast.simphony.context.space.continuous.ContinuousSpaceFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.engine.schedule.ScheduleParameters;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.continuous.AbstractPointTranslator;
import repast.simphony.space.continuous.InfiniteBorders;
import repast.simphony.space.continuous.StickyBorders;
import repast.simphony.space.continuous.WrapAroundBorders;

/**
 * Flocking model that simulates a large flock of smaller prey birds
 *   that attempt to avoid larger predator birds.
 * 
 * Adopted from the C# Swarm model by Daniel Greenheck: 
 *   http://greenhecktech.com/2014/04/03/throwback-thursday-swarm-intelligence/
 * 
 * @author Eric Tatara
 *
 */
@SuppressWarnings({ "rawtypes", "unchecked" })
public class SwarmBuilder implements ContextBuilder {
	
	@Override
	public Context build(Context context) {
		Parameters param = RunEnvironment.getInstance().getParameters();
		
		int initialNumPrey = (Integer)param.getValue("initialNumPrey");
		int initialNumPred = (Integer)param.getValue("initialNumPred");
		
		String borderType = param.getValueAsString("borderType");
		AbstractPointTranslator border = null;
		
		if ("Wrap_Around".equals(borderType))
			border = new WrapAroundBorders();
		else if ("Bouncy".equals(borderType))
			border = new repast.simphony.space.continuous.BouncyBorders();
		else if ("Infinite".equals(borderType))
			border = new InfiniteBorders<>();
		else if ("Sticky".equals(borderType))
			border = new StickyBorders();  
			
		ContinuousSpaceFactoryFinder.createContinuousSpaceFactory(null)
				.createContinuousSpace("Space", context, 
						new Adder(4),
						border, 
						500, 500, 500);
		
		// Add the prey
		List<Prey> preyList = new ArrayList<Prey>();
		for(int i=0; i<initialNumPrey; i++){
			preyList.add(new Prey());
		}
		context.addAll(preyList);

		// Add the predators
		for(int i=0; i<initialNumPred; i++){            	
			context.add(new Predator());
		}
		
		// If the model is running in multi-threaded mode, the Prey will 
		//   be updated in batches on each available CPU thread.
		String threadType = param.getValueAsString("threadType");
		if ("Multi-thread".equals(threadType)){
			context.add(new FlockUpdater(preyList));
		}
		// Otherwise, schedule all the Prey to update in the single simulation thread.
		else { 
			ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
			for (Prey prey : preyList){
				ScheduleParameters sp1 = ScheduleParameters.createRepeating(1, 1); 
				schedule.schedule(sp1, prey, "update");
			}
		}
		return context;
	}
}