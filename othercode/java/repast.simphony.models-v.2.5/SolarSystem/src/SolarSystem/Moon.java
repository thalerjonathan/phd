package SolarSystem;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.space.continuous.NdPoint;
import repast.simphony.util.ContextUtils;

public class Moon {

	private String name;
  private float[] rot = {0,0,0,0};
	private Planet planet;
	private double orbitDistance = 4.0;
  
	public Moon(String name, Planet planet){
		this.name = name;
		this.planet = planet;
	}
	
	@ScheduledMethod(start = 0)
	public void establishOrbit(){
	  orbit(true);
	}
	
	@ScheduledMethod(start = 1, interval = 1)
	public void step() {
		rotate();
		orbit(false);
	}
	
	private void orbit(boolean init){
		Context context = ContextUtils.getContext(this);
		ContinuousSpace space = (ContinuousSpace) context.getProjection("Universe");
		NdPoint point = space.getLocation(planet);
		
		double angle = 1;
    double tick = RunEnvironment.getInstance().getCurrentSchedule().getTickCount();
		double[] loc = {0,0,0};
    
		double x0 = point.getX();			// The x coordinate on the 2D continuous space
		double y0 = point.getY();      // The y coordinate on the 2D continuous space
		double z0 = point.getZ();
		
		loc[0] = orbitDistance * Math.sin(angle * tick/1000) + x0;
		loc[1] = y0; //
		loc[2] = orbitDistance * Math.cos(angle * tick/1000) + z0;
		
		space.moveTo(this, loc);
		
	}
	
	private void rotate(){
//	 rot[0] = rot[0] + .01f;
		 rot[1] = rot[1] + .01f;
//		 rot[2] = rot[2] + .01f;
		 rot[3] = rot[3] + .001f;		
	}
	
	public String getName() {
		return name;
	}

	public float[] getRot() {
		return rot;
	}
	
}
