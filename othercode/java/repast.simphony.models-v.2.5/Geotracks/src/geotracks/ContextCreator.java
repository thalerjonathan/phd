package geotracks;

import java.awt.Color;

import org.geotools.coverage.Category;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.geotools.util.NumberRange;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

import repast.simphony.context.Context;
import repast.simphony.context.space.gis.GeographyFactoryFinder;
import repast.simphony.context.space.graph.NetworkBuilder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.gis.Geography;
import repast.simphony.space.gis.GeographyParameters;
import repast.simphony.space.gis.RepastCoverageFactory;
import repast.simphony.space.gis.WritableGridCoverage2D;
import repast.simphony.space.graph.Network;

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
		
	public Context build(Context context) {
	
		GeographyParameters geoParams = new GeographyParameters();
		Geography geography = GeographyFactoryFinder.createGeographyFactory(null)
				.createGeography("Geography", context, geoParams);

		GeometryFactory fac = new GeometryFactory();

		NetworkBuilder<?> netBuilder = new NetworkBuilder<Object>("Network", context, true);
		Network net = netBuilder.buildNetwork();
				
		
		GisAgent agent = new GisAgent("Traveler");
		context.add(agent);

		Point geom = fac.createPoint(new Coordinate(-88.8570, 41.7432));
		geography.move(agent, geom);

		// envelope around northing Illinois
		ReferencedEnvelope env = new ReferencedEnvelope(-89.4, -87.7236, 41.50, 42.1681, DefaultGeographicCRS.WGS84);

		// Create a coverage to act as a heat map that becomes more intense with
		//  the number of times the agent has visited a point.
		int maxColorIndex = 10; //RepastCoverageFactory.MAX_BYTE_COLOR_INDEX;
		Color[] whiteRedColorScale = new Color[maxColorIndex];

		// white to red color scale
		for (int i=0; i<whiteRedColorScale.length; i++) {
			int blueGreen = (255/maxColorIndex*(maxColorIndex-i));			
			whiteRedColorScale[i] = new Color(255, blueGreen, blueGreen); 
		}

		// Color scale coverage with no-data
		Category[] categories	= new Category[] {	
				new Category("No data", new Color(0,0,0,0), 0),  // transparent
				new Category("Level", whiteRedColorScale, NumberRange.create(1, maxColorIndex))
		};

		WritableGridCoverage2D coverage2 = RepastCoverageFactory.createWritableByteIndexedCoverage(
				"My data indexed", 20, 20, env, categories, null, 0);

		geography.addCoverage("My indexed coverage", coverage2);
				
		return context;
	}
}