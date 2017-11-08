package geography;

import com.vividsolutions.jts.geom.Geometry;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.gis.util.GeometryUtil;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.gis.Geography;
import repast.simphony.util.ContextUtils;


/**
 * BufferZoneAagent is a geolocated agent indicated by a polygon feature that
 *  represents a buffer around a ZoneAgetn.
 * 
 * @author Eric Tatara
 *
 */
public class BufferZoneAgent {

	private double size;  // size of the current buffer zone.
	private String name; 
  private ZoneAgent zone;  // Zone that's associated with this bufferzone
	
	public BufferZoneAgent(String name, double size, ZoneAgent zone){
		this.name = name;
		this.size = size;
		this.zone = zone;
	}
	
	@ScheduledMethod(start = 1, interval = 1)
	public void step(){
		Parameters parm = RunEnvironment.getInstance().getParameters();
		double zoneDistance = (Double)parm.getValue("zoneDistance");  // meters
		
		// If the size of the bufferzone changes during the simulation, then update
		//  its geometry with the new size based on the associated zone agent.
		if (zoneDistance != size){
			Context context = ContextUtils.getContext(this);
			Geography geography = (Geography)context.getProjection("Geography"); 
			
			
			Geometry buffer = GeometryUtil.generateBuffer(geography, 
					geography.getGeometry(zone), zoneDistance);
			
			// Sets the new geometry for this agent in the geography
			geography.move(this, buffer);
			
			size = zoneDistance;
		}
		
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}