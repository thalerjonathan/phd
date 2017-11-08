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
import repast.simphony.random.RandomHelper;
import repast.simphony.space.gis.Geography;
import repast.simphony.space.graph.Network;
import repast.simphony.util.ContextUtils;

/**
 * Zombie agents move toward groups of Human agents and try to infect them.
 * 
 * @author Eric Tatara
 * @author Nick Collier
 * @author Jonathan Ozik
 * 
 */
public class Zombie {

	// Search zone agent list that defines the search radius area for this Zombie
	protected List<ZoneAgent> searchZoneAgents;
	
	// The current active search zone agent
	protected ZoneAgent activeSearchZone;
	
		
	/**
	 * Initialization occurs once at tick 0.
	 */
	@ScheduledMethod(start = 0)
	public void init(){
		Context context = ContextUtils.getContext(this);
		Geography geography = (Geography)context.getProjection("Geography");

		// Generate a list of search zone agents that will be used by the Zombie agent
		// to detect nearby Humans.

		// The number of search zone polygons around this agent.  More zones will 
		// result in more precise sensing, but the model will run slower.
		int numSearchZones = 6; 

		Coordinate center = geography.getGeometry(this).getCoordinate();
		List<Geometry> searchZones = ZombieUtils.generateSearchZones(geography, 
				center, numSearchZones);
		searchZoneAgents = new ArrayList<ZoneAgent>();
		
		// Create the ZoneAgents and add to geography and context
		for (Geometry geom : searchZones){
			ZoneAgent zoneAgent = new ZoneAgent();
			
			// Don't initially show search zones in display for zombie agents
			zoneAgent.setVisible(false);
			searchZoneAgents.add(zoneAgent);
			context.add(zoneAgent);
			geography.move(zoneAgent, geom);
		}
	}
	
	/**
	 * The step method.  Zombies will search the area within the search zones and
	 * detect humans and if humans are present will pursue them and try to infect
	 * humans.
	 */
	@ScheduledMethod(start = 1, interval = 1)
	public void step() {
		Context context = ContextUtils.getContext(this);
		Geography geography = (Geography)context.getProjection("Geography");
		
		// Get the list of all humans within the search distance
		double searchDistance = 5000; // meters
		List nearHumanList = ZombieUtils.getObjectsWithinDistance(this, 
				Human.class, searchDistance);
		
		// Find the zone with most humans
		ZoneAgent zoneWithMostHumans = null;
		int maxCount = -1;
		
		if (activeSearchZone != null){
			maxCount = activeSearchZone.lookForObjects(nearHumanList).size();
			zoneWithMostHumans = activeSearchZone;
		}
		
		for (ZoneAgent zone : searchZoneAgents){	
			zone.setActive(false);
			zone.setVisible(false);
			
			List foundHumans = zone.lookForObjects(nearHumanList);
			
			if(foundHumans.size() > maxCount){
				zoneWithMostHumans = zone;
				maxCount = foundHumans.size();
				activeSearchZone = zoneWithMostHumans;
			}
		}
		
	  // Move to the active search zone if it has some humans
		if (maxCount > 0){
			// Set the zone visibility only when this Zombie is actively tracking Humans
			for (ZoneAgent zone : searchZoneAgents){
				zone.setVisible(true);
			}
			moveTowards(activeSearchZone);
		}
		else{
//			randomWalk();			
		}
		activeSearchZone.setActive(true); // Active search zone is styled in display
		infect();
	}


	/**
	 * Move the Zombie towards the specified zone.
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
		int zombieSpeed = (Integer) params.getValue("zombieSpeed");
		
		// The rate of movement.
		double moveSlowDown = 100 / zombieSpeed;
		
		double x = (zoneCenter.getX() - currentPosisition.x) / moveSlowDown;
		double y = (zoneCenter.getY() - currentPosisition.y) / moveSlowDown;
		
		// Move towards the center by some displacement amount from the current position.
		geography.moveByDisplacement(this, x, y);
		
		// Update the surrounding search zone locations to reflect the new position
		updateSearchZone(currentPosisition);
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

	/**
	 * Infect nearby humans.
	 */
	public void infect() {
		Context context = ContextUtils.getContext(this);
		Geography geography = (Geography)context.getProjection("Geography");
		
		Coordinate currentPosisition = geography.getGeometry(this).getCoordinate();
		
		Parameters params = RunEnvironment.getInstance().getParameters();

		// The infection radius is smaller than the search radius
	  int infectRadius = (Integer) params.getValue("infectRadius"); // meters	
	  
	  // Find humans within the infection distance
	  List humans = ZombieUtils.getObjectsWithinDistance(this, Human.class, 
	  		infectRadius);
			  
	  // If humans are near, bite a random human and turn that human into a zombie!
		if (humans.size() > 0) {
			int index = RandomHelper.nextIntFromTo(0, humans.size() - 1);
			Human human = (Human)humans.get(index);

			// Get the human geometry to re-use it as the spawned zombie location
			Geometry humanloc = geography.getGeometry(human);
			
			// Remove the human from the context, thereby killing it.
			context.remove(human);
			
			// Create a new child zombie
			Zombie childZombie = new Zombie();
			context.add(childZombie);
			geography.move(childZombie, humanloc);
			childZombie.init();
			
			// Create an infection network edge between the parent and child zombie
			Network net = (Network)context.getProjection("infection network");
			net.addEdge(new InfectionNetworkEdge(this, childZombie));
			
			// Play creepy zombie sound
			SpecialEffects.getInstance().playZombieMoan();
		}
	}
	
	protected void randomWalk(){
		Context context = ContextUtils.getContext(this);
		Geography geography = (Geography)context.getProjection("Geography");

		Coordinate currentPosisition = geography.getGeometry(this).getCoordinate();
		
		geography.moveByDisplacement(this, RandomHelper.nextDoubleFromTo(-0.0001, 0.0001), 
				RandomHelper.nextDoubleFromTo(-0.0001, 0.0001));
		
		updateSearchZone(currentPosisition);
	}
	
	/**
	 * Use a single zombie to track whether all humans are dead or no humans are
	 * moving to end the run.
	 */
	@ScheduledMethod(start=10, interval=1, pick=1)
	public void checkHumans(){
		Context context = ContextUtils.getContext(this);
		
		double tick = RunEnvironment.getInstance().getCurrentSchedule().getTickCount();
		
		// If all humans dead
		if (!context.getAgentLayer(Human.class).iterator().hasNext()){
			RunEnvironment.getInstance().endAt(tick + 1);
			SpecialEffects.getInstance().surprise();
		}
		
		// If humans exist but none are moving
		else{
			boolean humansRunning = false;
			for(Object o : context.getAgentLayer(Human.class)){
				if ( ((Human)o).isRunning() ){
					humansRunning = true;
					break;
				}
			}
			if (!humansRunning){
				RunEnvironment.getInstance().endAt(tick + 1);
			}
		}
	}
}
