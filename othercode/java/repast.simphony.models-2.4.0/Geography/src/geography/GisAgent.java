package geography;

import repast.simphony.context.Context;
import repast.simphony.engine.schedule.ScheduleParameters;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.random.RandomHelper;
import repast.simphony.space.gis.Geography;
import repast.simphony.util.ContextUtils;

/**
 * A geolocated agent with a point location.
 * 
 * @author Eric Tatara
 *
 */
public class GisAgent {

	private String name;
	private boolean water = false;
	private double waterRate;

	public GisAgent(String name) {
		this.name = name;  
	}

	@ScheduledMethod(start = 1, interval = 1, priority = ScheduleParameters.FIRST_PRIORITY)
	public void step() {  	
		randomWalk();
		
		// The agent drinks and gets thirsy.
		water = false;
		waterRate = 0;
	}
	
	/**
	 * Random walk the agent around.
	 */
	private void randomWalk(){
		Context context = ContextUtils.getContext(this);
		Geography<GisAgent> geography = (Geography)context.getProjection("Geography");

		geography.moveByDisplacement(this, RandomHelper.nextDoubleFromTo(-0.0005, 0.0005), 
				RandomHelper.nextDoubleFromTo(-0.0005, 0.0005));
	}

	public String getName() {
		return name;
	}

	@Override
	public String toString(){
		return name;
	}

	public boolean isWater() {
		return water;
	}

	public void setWater(boolean water) {
		this.water = water;
	}

	public double getWaterRate() {
		return waterRate;
	}

	public void setWaterRate(double waterRate) {
		this.waterRate = waterRate;
	}
}