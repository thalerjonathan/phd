package geotracks;

import org.geotools.geometry.DirectPosition2D;
import org.opengis.geometry.DirectPosition;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

import repast.simphony.context.Context;
import repast.simphony.engine.schedule.ScheduleParameters;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.random.RandomHelper;
import repast.simphony.space.gis.Geography;
import repast.simphony.space.gis.WritableGridCoverage2D;
import repast.simphony.space.graph.Network;
import repast.simphony.util.ContextUtils;

/**
 * A geolocated agent with a point location.
 * 
 * @author Eric Tatara
 *
 */
public class GisAgent {

	private String name;
	private int waypointCount = 1;
	private Waypoint lastWayPoint = null;

	public GisAgent(String name) {
		this.name = name;  
	}

	@ScheduledMethod(start = 1, interval = 1, priority = ScheduleParameters.FIRST_PRIORITY)
	public void step() {  	
		randomWalk();
		trackInCoverage();
		dropWaypoint();
		
		// This is a synthetic way to slow down the agent step() method.  Since the 
		//   agent doesnt perform any complex activity, it would otherwise step the 
		//   model too fast to observe the behavior.  The try block with Thread.sleep()
		//   should not be used in an actual simulation model.
		try {
			Thread.sleep(50);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	public void dropWaypoint() {
		Context context = ContextUtils.getContext(this);
		Geography geography = (Geography)context.getProjection("Geography");
		Network net = (Network)context.getProjection("Network");
		
		Coordinate loc = geography.getGeometry(this).getCoordinate();
		
		Waypoint wp = new Waypoint(waypointCount);
		waypointCount++;
		
		context.add(wp);
		geography.move(wp, fac.createPoint(new Coordinate(loc)));
		
		if (lastWayPoint != null)
			net.addEdge(lastWayPoint, wp);
		
		lastWayPoint = wp;
	}
	
	private GeometryFactory fac = new GeometryFactory();
	
	public void trackInCoverage() {
		Context context = ContextUtils.getContext(this);
		Geography geography = (Geography)context.getProjection("Geography");
	
		WritableGridCoverage2D coverage = (WritableGridCoverage2D)geography.getCoverage("My indexed coverage");
		
		if (coverage == null) return;
		
		Coordinate loc = geography.getGeometry(this).getCoordinate();

		double val = 0;

		DirectPosition pos = new DirectPosition2D(loc.x, loc.y);
		
		// Nothing to do if agent position not over coverage, and would throw a
		//   PointOutsideCoverageException if accessed
		if (!coverage.getEnvelope2D().contains(pos)) 
			return;
		
		double[] gridVal = null;
		gridVal = coverage.evaluate(pos, gridVal);
		
		int max = 6;  // max coverage index value
		
		if (gridVal != null) {
			val = gridVal[0] + 1;
		}
		if (val > max) val = max;  // dont exceed max
		
		coverage.setValue(pos, val);
	}

	/**
	 * Random walk the agent around.
	 */
	private void randomWalk(){
		Context context = ContextUtils.getContext(this);
		Geography<GisAgent> geography = (Geography)context.getProjection("Geography");

		geography.moveByDisplacement(this, RandomHelper.nextDoubleFromTo(-0.05, 0.05), 
				RandomHelper.nextDoubleFromTo(-0.05, 0.05));
	}

	public String getName() {
		return name;
	}

	@Override
	public String toString(){
		return name;
	}

}