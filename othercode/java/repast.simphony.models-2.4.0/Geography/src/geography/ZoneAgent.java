package geography;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.parameter.Parameters;
import repast.simphony.query.space.gis.GeographyWithin;
import repast.simphony.query.space.gis.IntersectsQuery;
import repast.simphony.space.gis.Geography;
import repast.simphony.util.ContextUtils;

/**
 * The Zone agent defines a specific region in the geography that is 
 * represented by a polygon feature.  Zone agents will update the water status
 * of any GisAgent located within a certain proximity to the Zone.
 * 
 * @author Eric Tatara
 *
 */
public class ZoneAgent {

	private String name;
	private double taxRate;
	private double waterFlowRate;

	public ZoneAgent(String name, double taxRate){
		this.name = name;
		this.taxRate = taxRate;
	}

	@ScheduledMethod(start = 1, interval = 1)
	public void step() {  	
		checkWaterSupply();
	}

	/**
	 * Checks first if the zone intersects a water line to determine if the zone
	 * has water.  Next the zone queries all agents within a specified distance
	 * from the zone border and sets the water status of the agent based on the
	 * water status of the zone.
	 */
	private void checkWaterSupply(){
		Context context = ContextUtils.getContext(this);
		Geography geography = (Geography)context.getProjection("Geography"); 

		// Find all features that intersect the zone feature
		IntersectsQuery query = new IntersectsQuery(geography, this);	
		waterFlowRate = 0;
		for (Object obj : query.query()) {
			// If the zone finds a water line, then set the zone water rate from the line
			if (obj instanceof WaterLine){
				WaterLine line = (WaterLine)obj;

				waterFlowRate = line.getFlowRate();
			}
		}
		
		/**
		 * Checks if GisAgents are within a certain distance from the zone and 
		 * sets the water status of the agent based on the zone's water status.
		 */
		Parameters parm = RunEnvironment.getInstance().getParameters();
		double zoneDistance = (Double)parm.getValue("zoneDistance");  // meters
		
		GeographyWithin within = new GeographyWithin(geography, zoneDistance, this);
		for (Object obj : within.query()) {
			if (obj instanceof GisAgent){
				// If the zone finds a GisAgent, set the agent water rate from the zone
				GisAgent agent = (GisAgent)obj;

				if (getWaterFlowRate() > 0){
					agent.setWater(true);
					agent.setWaterRate(getWaterFlowRate());
				}
			}
		}
		
		// An alternative approach to using GeographyWithin would be to do an 
		//  IntersectsQuery on the BufferZoneAgents and check for GisAgents that
		//  intersect the BufferZone.  That would be computationally faster since
		//  the GeographyWithin internally creates a buffer based on the distance 
		//  each time.
	}


	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public double getTaxRate() {
		return taxRate;
	}

	public void setTaxRate(double taxRate) {
		this.taxRate = taxRate;
	}

	public double getWaterFlowRate() {
		return waterFlowRate;
	}

	public void setWaterFlowRate(double waterFlowRate) {
		this.waterFlowRate = waterFlowRate;
	}
}