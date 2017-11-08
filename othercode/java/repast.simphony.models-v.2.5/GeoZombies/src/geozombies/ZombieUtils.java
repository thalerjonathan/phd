package geozombies;

import java.util.ArrayList;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.util.GeometricShapeFactory;

import repast.simphony.context.Context;
import repast.simphony.gis.util.GeometryUtil;
import repast.simphony.space.gis.Geography;
import repast.simphony.util.ContextUtils;

/**
 * Utilities for the GeoZombies model.
 * 
 * @author Eric Tatara
 *
 */
public class ZombieUtils {

	public static GeometryFactory fac = new GeometryFactory();

	/**
	 * Generates a list of polygon geometries that defines the search area around
	 * a Zombie.  This implementation creates a set of "pie slices" within a 
	 * circle around the Zombie. 
	 * 
	 * @param center the current Zombie location coordinate
	 * @return a list of polygon geometries
	 */
	public static List<Geometry> generateSearchZones(Geography geography, 
			Coordinate center, int numZones){
		List<Geometry> geomList = new ArrayList<Geometry>();

		// Radius of the search circle area
		double radius = 0.1;  // in degree lat/lon

		// The radian interval used to divide the search circle area
		double interval = 2 * Math.PI / numZones;

		GeometricShapeFactory gsf = new GeometricShapeFactory();

		int numArcPoints = 10; // Num points in the arc (precision)

		gsf.setCentre(center);
		gsf.setSize(radius); 
		gsf.setNumPoints(numArcPoints); 

		// Generate a polygon geometry for each circle pie slice
		for (int i=0; i < numZones; i++){
			LineString arc = gsf.createArc(i*interval, interval);

			Coordinate[] coords = new Coordinate[numArcPoints + 2];

			coords[0] = center;
			coords [numArcPoints + 2 - 1] = center;

			for (int j=0; j<numArcPoints; j++){
				coords[j+1] = arc.getCoordinateN(j);
			}

			geomList.add(fac.createPolygon(coords));
		}
		return geomList;
	}

	/**
	 * Returns a list of objects in the geography based on the source object, class
	 * type of objects to search for, and distance.  This approach is faster than
	 * using the Repast GeograpyWithin query since it uses the geography's internal
	 * spatial index to limit search results based on distance.  The GeoraphyWithin
	 * query in contrast compares the distance of all objects in the geography which
	 * will be slow when there are lots of objects.
	 * 
	 * This approach uses the reference envelope around a distance buffer around
	 * the source object's geometry, which for a single agent should be a point
	 * source with a circular buffer.  The envelope around the buffer is 
	 * rectangular so this should be used as a rough within distance result and
	 * refined further to check if the list of return objects fall within a more
	 * specific region contained within the reference envelope.
	 * 
	 * @param source the agent performing the search
	 * @param clazz the class of objects the agent is searching for
	 * @param searchDistance the search distance around the agent
	 * @return
	 */
	public static List<?> getObjectsWithinDistance(Object source, Class clazz,
			double searchDistance){
		Context context = ContextUtils.getContext(source);
		Geography geography = (Geography)context.getProjection("Geography");
		
		Geometry searchArea =  GeometryUtil.generateBuffer(geography, 
				geography.getGeometry(source), searchDistance);
		
		Envelope searchEnvelope = searchArea.getEnvelopeInternal();
		Iterable<?> nearObjects = geography.getObjectsWithin(searchEnvelope, clazz);	
		List nearObjectList = new ArrayList();
		
		for (Object o : nearObjects){
			nearObjectList.add(o);
		}	
		return nearObjectList;
	}
	
}
