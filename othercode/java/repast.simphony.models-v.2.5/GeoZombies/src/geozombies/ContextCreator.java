package geozombies;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

import repast.simphony.context.Context;
import repast.simphony.context.space.gis.GeographyFactoryFinder;
import repast.simphony.context.space.graph.NetworkBuilder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.parameter.Parameters;
import repast.simphony.random.RandomHelper;
import repast.simphony.space.gis.Geography;
import repast.simphony.space.gis.GeographyParameters;
import repast.simphony.space.graph.Network;

/**
 * ContextBuilder for the GeoZombies model.  This demo model has similar
 * behavior to the other Repast Zombies models but operates in a geospatial
 * projection.
 * 
 * @author Eric Tatara
 * @author Nick Collier
 * @author Jonathan Ozik
 *
 */
public class ContextCreator implements ContextBuilder {

  static {
    SpecialEffects.setUIEffects();
  }
  
	int numAgents;
	double zoneDistance;
	
	public Context build(Context context) {		
		
		// Create the Geography projection that is used to store geographic locations
		// of agents in the model.
		GeographyParameters geoParams = new GeographyParameters();
		Geography geography = GeographyFactoryFinder.createGeographyFactory(null)
				.createGeography("Geography", context, geoParams);

		// Create the Network projection that is used to create the infection network.
		NetworkBuilder<Object> netBuilder = new NetworkBuilder<Object>(
				"infection network", context, true);
		Network net = netBuilder.buildNetwork();
		
		// Geometry factory is used to create geometries
		GeometryFactory fac = new GeometryFactory();
		
		// Create Zombie agents
		Parameters params = RunEnvironment.getInstance().getParameters();
		int zombieCount = (Integer) params.getValue("zombie_count");
		for (int i = 0; i < zombieCount; i++) {
			Zombie zombie = new Zombie();
			context.add(zombie);
			
			// Create coordinates in the Chicago area
			Coordinate coord = new Coordinate(-88 + 0.5* Math.random(), 41.5 + 0.5 * Math.random());
			Point geom = fac.createPoint(coord);
			
			geography.move(zombie, geom);
		}

		// Create Human agents
		int humanCount = (Integer) params.getValue("human_count");
		for (int i = 0; i < humanCount; i++) {
			int energy = RandomHelper.nextIntFromTo(4, 10);
			
			Human human = new Human(energy);
			
			context.add(human);
			
			// Create coordinates in the Chicago area
			Coordinate coord = new Coordinate(-88 + 0.5* Math.random(), 41.5 + 0.5 * Math.random());
			Point geom = fac.createPoint(coord);
			
			geography.move(human, geom);
		}
		
		if (RunEnvironment.getInstance().isBatch()) {
			RunEnvironment.getInstance().endAt(100);
		}
		
		return context;
	}
}