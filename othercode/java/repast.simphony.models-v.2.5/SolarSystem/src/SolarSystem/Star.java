package SolarSystem;

import repast.simphony.engine.schedule.ScheduledMethod;

public class Star {

	private String name;
  private float[] rot = {0,0,0,0};
	
	public Star(String name){
		this.name = name;
	}
	
	@ScheduledMethod(start = 1, interval = 1)
	public void step() {
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
