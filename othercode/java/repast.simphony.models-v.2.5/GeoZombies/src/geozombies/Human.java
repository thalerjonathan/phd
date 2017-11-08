
package geozombies;

import java.util.ArrayList;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.gis.Geography;
import repast.simphony.util.ContextUtils;

/**
 * Human Agents flee from nearby Zombies.  If infected, Humans become Zombies.
 * 
 * @author Eric Tatara
 * @author Nick Collier
 * @author Jonathan Ozik
 *
 */
public class Human {

	// Search zone agent list that defines the search radius area for this Human
	protected List<ZoneAgent> searchZoneAgents;
	
	// The current active search zone agent
	protected ZoneAgent activeSearchZone;
	
	protected boolean running = false;
	
	public Human(int energy) {
	}
	
	/**
	 * Initialization occurs once at tick 0.
	 */
	@ScheduledMethod(start = 0)
	public void init(){
		Context context = ContextUtils.getContext(this);
		Geography geography = (Geography)context.getProjection("Geography");
		
		// Generate a list of search zone agents that will be used by the Human agent
		// to detect nearby Zombies.

		// The number of search zone polygons around this agent.  More zones will 
		// result in more precise sensing, but the model will run slower.
		int numSearchZones = 4; 
		
		Coordinate center = geography.getGeometry(this).getCoordinate();
		List<Geometry> searchZones = ZombieUtils.generateSearchZones(geography, 
				center, numSearchZones);
		searchZoneAgents = new ArrayList<ZoneAgent>();
		
		// Create the ZoneAgents and add to geography and context
		for (Geometry geom : searchZones){
			ZoneAgent zoneAgent = new ZoneAgent();
			
			// Don't show search zones in display for Human agents
			zoneAgent.setVisible(false);
			searchZoneAgents.add(zoneAgent);
			context.add(zoneAgent);
			geography.move(zoneAgent, geom);
		}
	}
	
	/**
	 * The Step method.  Humans will search the area within the search zones and
	 * detect zombies and if zombies are present will flee to the zone with the 
	 * least number of zombies.
	 */
	@ScheduledMethod(start = 1, interval = 1)
	public void step() {
		Context context = ContextUtils.getContext(this);
		Geography geography = (Geography)context.getProjection("Geography");
		
		// Get the list of all zombies within the search distance
		double searchDistance = 5000; // meters
		List nearZombieList = ZombieUtils.getObjectsWithinDistance(this, 
				Zombie.class, searchDistance);
		
		// Find the search zone with the least zombies
		ZoneAgent zoneWithLeastZombies = null;
		int minCount = Integer.MAX_VALUE;
	
		if (activeSearchZone != null){
			minCount = activeSearchZone.lookForObjects(nearZombieList).size();
			zoneWithLeastZombies = activeSearchZone;
		}
		
		running = false;
		for (ZoneAgent zone : searchZoneAgents){	
			zone.setActive(false);
			
			// Get the list of zombies in each individual zone polygon
			List foundZombies = zone.lookForObjects(nearZombieList);
			
			if (foundZombies.size() > 0) 
				running = true;
			
			if(foundZombies.size() < minCount){
				zoneWithLeastZombies = zone;
				minCount = foundZombies.size();
				activeSearchZone = zoneWithLeastZombies;
			}
		}
		
		// Move to the active search zone if other zones have some zombies
		if (running){
			moveTowards(activeSearchZone);
		}
		else{
		
		}
	
		activeSearchZone.setActive(true);  // Active search zone is styled in display
	}

	/**
	 * Move the Human towards the specified zone.
	 * 
	 * @param zone
	 */
	public void moveTowards(ZoneAgent zone) {
		Context context = ContextUtils.getContext(this);
		Geography geography = (Geography)context.getProjection("Geography");
		
		// Move towards the zone center point
		Point zoneCenter = geography.getGeometry(zone).getCentroid();
		Coordinate currentPosisition = geography.getGeometry(this).getCoordinate();
		
		Parameters params = RunEnvironment.getInstance().getParameters();
		int speed = (Integer) params.getValue("humanSpeed");
		
		// The rate of movement.
		double moveSlowDown = 100 / speed;
		
		double x = (zoneCenter.getX() - currentPosisition.x) / moveSlowDown;
		double y = (zoneCenter.getY() - currentPosisition.y) / moveSlowDown;
		
		// Move towards the center by some displacement amount from the current position.
		geography.moveByDisplacement(this, x, y);
		
		// Update the surrounding search zone locations to reflect the new position
		updateSearchZone(currentPosisition);
		
		// Play some terrifying sound effects of people screaming
//		ZombieUtils.playScream();
		SpecialEffects.getInstance().playEnragedZombies();
	}

	/**
	 * Move the search zone areas around the agent based on the agent's current
	 * location.
	 * 
	 * @param lastPosition
	 */
	protected void updateSearchZone(Coordinate lastPosition){
		Context context = ContextUtils.getContext(this);
		Geography geography = (Geography)context.getProjection("Geography");
		
		Coordinate currentPosisition = geography.getGeometry(this).getCoordinate();
		
		double x = (currentPosisition.x - lastPosition.x);
		double y = (currentPosisition.y - lastPosition.y);
		
		for (ZoneAgent zone : searchZoneAgents){
			geography.moveByDisplacement(zone, x, y);	
		}
	}

	public boolean isRunning() {
		return running;
	}

	public void setRunning(boolean running) {
		this.running = running;
	}
}
