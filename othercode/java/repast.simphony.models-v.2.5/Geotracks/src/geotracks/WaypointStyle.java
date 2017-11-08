package geotracks;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.Map;

import gov.nasa.worldwind.WorldWind;
import gov.nasa.worldwind.render.BasicWWTexture;
import gov.nasa.worldwind.render.Material;
import gov.nasa.worldwind.render.Offset;
import gov.nasa.worldwind.render.PatternFactory;
import gov.nasa.worldwind.render.WWTexture;
import repast.simphony.visualization.gis3D.PlaceMark;
import repast.simphony.visualization.gis3D.style.MarkStyle;

/**
 * Style for GisAgents.  This demo style changes the appearance of the GisAgents
 *   based on their water value.
 * 
 * @author Eric Tatara
 *
 */
public class WaypointStyle implements MarkStyle<Waypoint>{
	
	private Map<String, WWTexture> textureMap;
	
	public WaypointStyle(){
		
		/**
		 * Use of a map to store textures significantly reduces CPU and memory use
		 * since the same texture can be reused.  Textures can be created for different
		 * agent states and re-used when needed.
		 */
		textureMap = new HashMap<String, WWTexture>();
		
		BufferedImage image = PatternFactory.createPattern(PatternFactory.PATTERN_CIRCLE, 
				new Dimension(50, 50), 0.7f,  Color.BLUE);
		
		textureMap.put("blue circle", new BasicWWTexture(image));
		
		image = PatternFactory.createPattern(PatternFactory.PATTERN_CIRCLE, 
				new Dimension(50, 50), 0.7f,  Color.YELLOW);
		
		textureMap.put("yellow circle", new BasicWWTexture(image));
	}
	
	/**
	 * The PlaceMark is a WWJ PointPlacemark implementation with a different 
	 *   texture handling mechanism.  All other standard WWJ PointPlacemark 
	 *   attributes can be changed here.  PointPlacemark label attributes could be
	 *   set here, but are also available through the MarkStyle interface.
	 *   
	 *   @see gov.nasa.worldwind.render.PointPlacemark for more info.
	 */
	@Override
	public PlaceMark getPlaceMark(Waypoint agent, PlaceMark mark) {
		
		// PlaceMark is null on first call.
		if (mark == null)
			mark = new PlaceMark();
		
		/**
		 * The Altitude mode determines how the mark appears using the elevation.
		 *   WorldWind.ABSOLUTE places the mark at elevation relative to sea level
		 *   WorldWind.RELATIVE_TO_GROUND places the mark at elevation relative to ground elevation
		 *   WorldWind.CLAMP_TO_GROUND places the mark at ground elevation
		 */
		mark.setAltitudeMode(WorldWind.RELATIVE_TO_GROUND);
		mark.setLineEnabled(false);
		
		return mark;
	}
	
	/**
	 * Get the mark elevation in meters.  The elevation is used to visually offset 
	 *   the mark from the surface and is not an inherent property of the agent's 
	 *   location in the geography.
	 */
	@Override
	public double getElevation(Waypoint agent) {
			return 0;
	}
	
	/**
	 * Here we set the appearance of the GisAgent.  In this style implementation,
	 *   the style class creates a new BufferedImage each time getTexture is 
	 *   called.  If the texture never changes, the texture argument can just be 
	 *   checked for null value, created once, and then just returned every time 
	 *   thereafter.  If there is a small set of possible values for the texture,
	 *   eg. blue circle, and yellow circle, those BufferedImages could 
	 *   be stored here and re-used by returning the appropriate image based on 
	 *   the agent properties. 
	 */
	@Override
	public WWTexture getTexture(Waypoint agent, WWTexture texture) {
	
		// WWTexture is null on first call.
	
		return textureMap.get("blue circle");
	}
	
	/**
	 * Scale factor for the mark size.
	 */
	@Override
	public double getScale(Waypoint agent) {
		return 0.2;
	}

	@Override
	public double getHeading(Waypoint agent) {
		return 0;
	}
	
	/**
	 * The agent on-screen label.  Return null instead of empty string "" for better
	 *   performance.
	 */
	@Override
	public String getLabel(Waypoint agent) {
		return null;
	}

	@Override
	public Color getLabelColor(Waypoint agent) {
		return null;
	}
	
	/**
	 * Return an Offset that determines the label position relative to the mark 
	 * position.  @see gov.nasa.worldwind.render.Offset
	 * 
	 */
	@Override
	public Offset getLabelOffset(Waypoint agent) {
		return null;
	}

	@Override
	public Font getLabelFont(Waypoint obj) {
		return null;
	}

	/** Width of the line that connects an elevated mark with the surface.  Use
	 *    a value of 0 to disable line drawing.
	 *   
	 */
	@Override
	public double getLineWidth(Waypoint agent) {
		return 0;
	}

	@Override
	public Material getLineMaterial(Waypoint obj, Material lineMaterial) {
		return null;
	}

	@Override
	public Offset getIconOffset(Waypoint obj) {
		return Offset.CENTER;
	}
}