package geography;

import org.geotools.coverage.grid.GridCoordinates2D;
import org.geotools.geometry.DirectPosition2D;
import org.opengis.geometry.DirectPosition;

import com.vividsolutions.jts.geom.Coordinate;

import repast.simphony.context.Context;
import repast.simphony.engine.schedule.ScheduleParameters;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.random.RandomHelper;
import repast.simphony.space.gis.Geography;
import repast.simphony.space.gis.WritableGridCoverage2D;
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
	
	@ScheduledMethod(start=1, interval=1, pick=1)
	/**
	 * Writes random data to the "My Coverage" coverage.
	 */
	public void testCoverageWrite() {
		Context context = ContextUtils.getContext(this);
		Geography<GisAgent> geography = (Geography)context.getProjection("Geography");
	
		WritableGridCoverage2D coverage = (WritableGridCoverage2D)geography.getCoverage("My coverage");
			
		int x = RandomHelper.nextIntFromTo(0,9);
		int y = RandomHelper.nextIntFromTo(0,9);

		double val = RandomHelper.nextDoubleFromTo(0, 10);
		
		GridCoordinates2D coord = new GridCoordinates2D(x, y);
		
		coverage.setValue(coord, val);
		double[] read = null;
		
		// Test read some random part of the coverage
		double xx = RandomHelper.nextDoubleFromTo(coverage.getEnvelope2D().getMinX(),
				coverage.getEnvelope2D().getMaxX());
		double yy = RandomHelper.nextDoubleFromTo(coverage.getEnvelope2D().getMinY(),
				coverage.getEnvelope2D().getMaxY());
		
		DirectPosition pos = new DirectPosition2D(xx,yy);
		read = coverage.evaluate(pos, read);
	}
	
//	@ScheduledMethod(start=100, pick=1)
	public void testdelete() {
		Context context = ContextUtils.getContext(this);
		Geography<GisAgent> geography = (Geography)context.getProjection("Geography");
	
		WritableGridCoverage2D coverage = (WritableGridCoverage2D)geography.getCoverage("My indexed coverage");
		
		geography.removeCoverage("My indexed coverage");
		
	}
	
	@ScheduledMethod(start=1, interval=1)
	public void testCoverageReadWrite2() {
		Context context = ContextUtils.getContext(this);
		Geography<GisAgent> geography = (Geography)context.getProjection("Geography");
	
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
		
		int max = 3;  // max coverage index value
		
		if (gridVal != null) {
			val = gridVal[0] + 1;
		}
		if (val > max) val = max;  // dont exceed max
		
		coverage.setValue(pos, val);
	}
	
	@ScheduledMethod(start=1, interval=1)
	public void testCoverageReadWrite3() {
		Context context = ContextUtils.getContext(this);
		Geography<GisAgent> geography = (Geography)context.getProjection("Geography");
	
		WritableGridCoverage2D coverage = (WritableGridCoverage2D)geography.getCoverage("My indexed coverage 2");
		
		Coordinate loc = geography.getGeometry(this).getCoordinate();

		int val = 0;

		DirectPosition pos = new DirectPosition2D(loc.x, loc.y);
		
		// Nothing to do if agent position not over coverage, and would throw a
		//   PointOutsideCoverageException if accessed
		if (!coverage.getEnvelope2D().contains(pos)) 
			return;
		
		int[] gridVal = null;
		gridVal = coverage.evaluate(pos, gridVal);
		
		if (gridVal != null) {
			val = gridVal[0] + 1;
		}
		if (val > 10) 
			val = 10;  // dont exceed 
		
		coverage.setValue(pos, val);
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