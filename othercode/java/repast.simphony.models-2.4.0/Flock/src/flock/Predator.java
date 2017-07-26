package flock;

import javax.vecmath.Vector3d;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.space.continuous.NdPoint;
import repast.simphony.util.ContextUtils;

/**
 * Predator grabs a random target prey and attempts to eat it.
 * 
 * Adopted from the C# Swarm model by Daniel Greenheck: 
 *   http://greenhecktech.com/2014/04/03/throwback-thursday-swarm-intelligence/
 * 
 * @author Eric Tatara
 * 
 */
public class Predator extends Boid {
	
	private Prey target;         // The current target
	private Vector3d attackVector = new Vector3d();    // Vector to the prey
  private ContinuousSpace space;
	
	/**
	 * Predator initialization.
	 */
	@ScheduledMethod(start=0)
	public void init(){
		Context context = ContextUtils.getContext(this);
		space = (ContinuousSpace)context.getProjection("Space");
	
		NdPoint q = space.getLocation(this);
		lastPosition = new Vector3d(q.getX(), q.getY(), q.getZ());
	}
	
	@ScheduledMethod(start=1, interval=1)
	public void update(){
		Parameters param = RunEnvironment.getInstance().getParameters();
		Context context = ContextUtils.getContext(this);
		
		// A smaller time scale results in smoother movement, but over a shorter
		//  distance.  Reduce to speed up simluation speed.
		double timeScale = (Double)param.getValue("timeScale");
		
		// Vector which will modify the boids velocity vector
		Vector3d velocityUpdate = new Vector3d();     

		// If we have no target or we killed this one, acquire a new one
		
		// How close the predator has to be to the prey to kill it
		double killRadius = (Double)param.getValue("killRadius");
		
		if( target == null || attackVector.lengthSquared() < killRadius * killRadius){
			target = (Prey)context.getRandomObjects(Prey.class, 1).iterator().next();
		}
		
		if(target != null){
			attackVector.sub(target.getLastPosition(), lastPosition);
			velocityUpdate.add(attackVector);
		}

		// Update the velocity of the boid
		// Modify our velocity update vector to take into account acceleration over time
		double preyAcceleration = (Double)param.getValue("preyAcceleration");
		double predAcceleration = (Double)param.getValue("predAcceleration");
		double predMaxSpeed = (Double)param.getValue("predMaxSpeed");
		
		velocityUpdate.scale(predAcceleration * preyAcceleration * timeScale);

		// Apply the update to the velocity
		velocity.add(velocityUpdate);

		// If our velocity vector exceeds the max speed, throttle it back to the MAX_SPEED
		if (velocity.length() > predMaxSpeed ){
			velocity.normalize();
			velocity.scale(predMaxSpeed);
		}

		// Update the position of the boid
		velocity.scale(timeScale);
		lastPosition.add(velocity);
		space.moveByDisplacement(this, velocity.x, velocity.y, velocity.z);
	}
}