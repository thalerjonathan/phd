package geography;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.geotools.data.shapefile.ShapefileDataStore;
import org.geotools.data.simple.SimpleFeatureIterator;
import org.opengis.feature.simple.SimpleFeature;

import repast.simphony.context.Context;
import repast.simphony.context.space.gis.GeographyFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.gis.Geography;
import repast.simphony.space.gis.GeographyParameters;
import repast.simphony.space.gis.GeometryUtils;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * ContextBuilder for the GIS demo.  In this model, mobile GisAgents move around
 * the Geography with a random motion and are represented by point locations.  
 * ZoneAgents are polygons that represent certain geographic areas.  WaterLine
 * agents represent water supply lines from Lake Michigan that supply the 
 * Chicago area.  When a ZoneAgent intersects a WaterLine, the ZoneAgent will 
 * have access to fresh drinking water.  GisAgents that are within a certain 
 * distance from the ZoneAgent boundary will also have access to water.  Agents
 * that are not in proximity to a Zone with a water supply will not have access
 * to water (they will be thirsty).  BufferZoneAgents are for visualization 
 * to illustrate the extend of the boundary around a ZoneAgent.  
 * 
 * GisAgents may be generated programmatically depending on the value for 
 * number of agents.  GisAgents, ZoneAgents, and WaterLine agents are also
 * loaded from ESRI shapefiles.
 * 
 * @author Eric Tatara
 *
 */
public class ContextCreator implements ContextBuilder {

	int numAgents;
	double zoneDistance;
	
	public Context build(Context context) {

		System.out.println("Geography Demo ContextBuilder.build()");
		
		Parameters parm = RunEnvironment.getInstance().getParameters();
		numAgents = (Integer)parm.getValue("numAgents");
		zoneDistance = (Double)parm.getValue("zoneDistance");

		GeographyParameters geoParams = new GeographyParameters();
		Geography geography = GeographyFactoryFinder.createGeographyFactory(null)
				.createGeography("Geography", context, geoParams);

		GeometryFactory fac = new GeometryFactory();

		// Generate some points
		for (int i = 0; i < numAgents; i++) {
			GisAgent agent = new GisAgent("Site " + i);
			context.add(agent);

			Coordinate coord = new Coordinate(-88 + 0.5* Math.random(), 41.5 + 0.5 * Math.random());
			Point geom = fac.createPoint(coord);
			geography.move(agent, geom);
		}

		// TODO GIS: use an example of ShapefileLoader
		
		// Load Features from shapefiles
		loadFeatures( "data/Zones2.shp", context, geography);
		loadFeatures( "data/Agents2.shp", context, geography);
		loadFeatures( "data/WaterLines.shp", context, geography);
		
		return context;
	}

	/**
	 * Loads features from the specified shapefile.  The appropriate type of agents
	 * will be created depending on the geometry type in the shapefile (point, 
	 * line, polygon).
	 * 
	 * @param filename the name of the shapefile from which to load agents
	 * @param context the context
	 * @param geography the geography
	 */
	private void loadFeatures (String filename, Context context, Geography geography){
		URL url = null;
		try {
			url = new File(filename).toURL();
		} catch (MalformedURLException e1) {
			e1.printStackTrace();
		}

		List<SimpleFeature> features = new ArrayList<SimpleFeature>();
		
		// Try to load the shapefile
		SimpleFeatureIterator fiter = null;
		ShapefileDataStore store = null;
		store = new ShapefileDataStore(url);

		try {
			fiter = store.getFeatureSource().getFeatures().features();

			while(fiter.hasNext()){
				features.add(fiter.next());
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		finally{
			fiter.close();
			store.dispose();
		}
		
		// For each feature in the file
		for (SimpleFeature feature : features){
			Geometry geom = (Geometry)feature.getDefaultGeometry();
			Object agent = null;

			// For Polygons, create ZoneAgents
			if (geom instanceof MultiPolygon){
				MultiPolygon mp = (MultiPolygon)feature.getDefaultGeometry();
				geom = (Polygon)mp.getGeometryN(0);

				// Read the feature attributes and assign to the ZoneAgent
				String name = (String)feature.getAttribute("name");
				double taxRate = (double)feature.getAttribute("Tax_Rate");

				agent = new ZoneAgent(name,taxRate);

				// Create a BufferZoneAgent around the zone, just for visualization
				Geometry buffer = GeometryUtils.generateBuffer(geography, geom, zoneDistance);
				BufferZoneAgent bufferZone = new BufferZoneAgent("Buffer: " + name, 
						zoneDistance, (ZoneAgent)agent);
				context.add(bufferZone);
				geography.move(bufferZone, buffer);
			}

			// For Points, create RadioTower agents
			else if (geom instanceof Point){
				geom = (Point)feature.getDefaultGeometry();				

				// Read the feature attributes and assign to the ZoneAgent
				String name = (String)feature.getAttribute("Name");
				agent = new RadioTower(name);
			}

			// For Lines, create WaterLines
			else if (geom instanceof MultiLineString){
				MultiLineString line = (MultiLineString)feature.getDefaultGeometry();
				geom = (LineString)line.getGeometryN(0);

				// Read the feature attributes and assign to the ZoneAgent
				String name = (String)feature.getAttribute("Name");
				double flowRate = (Long)feature.getAttribute("Flow_Rate");
				agent = new WaterLine(name, flowRate);
			}

			if (agent != null){
				context.add(agent);
				geography.move(agent, geom);
			}
			else{
				System.out.println("Error creating agent for  " + geom);
			}
		}				
	}
}