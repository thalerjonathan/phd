package SolarSystem;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.space.continuous.NdPoint;
import repast.simphony.util.ContextUtils;

public class Planet {

	private String name;
  private float[] rot = {0,0,0,0};
  private Star star;
	private double orbitDistance = 40.0;
	
	public Planet(String name, Star star){
		this.name = name;
		this.star = star;
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
		NdPoint point = space.getLocation(star);
		
		double angle = 0.01;
    double tick = RunEnvironment.getInstance().getCurrentSchedule().getTickCount();
		double[] loc = {0,0,0};
    
		double x0 = point.getX();			// The x coordinate on the 2D continuous space
		double y0 = point.getY();      // The y coordinate on the 2D continuous space
		double z0 = point.getZ();
		
		loc[0] = orbitDistance * Math.sin(angle * tick/100) + x0;
		loc[1] = y0; //
		loc[2] = orbitDistance * Math.cos(angle * tick/100) + z0;
		
		space.moveTo(this, loc);
		
	}

	public void rotate() {
//	 rot[0] = rot[0] + .01f;
	 rot[1] = rot[1] + .01f;
//	 rot[2] = rot[2] + .01f;
	 rot[3] = rot[3] + .001f;
	}
	
	public String getName() {
		return name;
	}

	public float[] getRot() {
		return rot;
	}
	
}
