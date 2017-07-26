package geography;

import gov.nasa.worldwind.WorldWind;
import gov.nasa.worldwind.avlist.AVKey;
import gov.nasa.worldwind.render.Material;
import gov.nasa.worldwind.render.Offset;
import gov.nasa.worldwind.render.PatternFactory;
import gov.nasa.worldwind.render.WWTexture;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.image.BufferedImage;

import repast.simphony.visualization.gis3D.BufferedImageTexture;
import repast.simphony.visualization.gis3D.PlaceMark;
import repast.simphony.visualization.gis3D.style.MarkStyle;

/**
 * Style for GisAgents.  This demo style changes the appearance of the GisAgents
 *   based on their water value.
 * 
 * @author Eric Tatara
 *
 */
public class GisAgentStyle implements MarkStyle<GisAgent>{
	
	private Offset labelOffset;
	
	public GisAgentStyle(){
		
		/**
		 * The gov.nasa.worldwind.render.Offset is used to position the label from 
		 *   the mark point location.  The first two arguments in the Offset 
		 *   constructor are the x and y offset values.  The third and fourth 
		 *   arguments are the x and y units for the offset. AVKey.FRACTION 
		 *   represents units of the image texture size, with 1.0 being one image 
		 *   width/height.  AVKey.PIXELS can be used to specify the offset in pixels. 
		 */
		labelOffset = new Offset(1.2d, 0.6d, AVKey.FRACTION, AVKey.FRACTION);
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
	public PlaceMark getPlaceMark(GisAgent agent, PlaceMark mark) {
		
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
	public double getElevation(GisAgent agent) {
		if (agent.isWater()){
			return 1000;  // meters
		}
		else{
			return 0;
		}
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
	public WWTexture getTexture(GisAgent agent, WWTexture texture) {
	
		// WWTexture is null on first call.
		
		Color color = null;
	
		if (agent.isWater()){
			color = Color.BLUE;
		}
		else{
			color = Color.YELLOW;
		}
		
		BufferedImage image = PatternFactory.createPattern(PatternFactory.PATTERN_CIRCLE, 
				new Dimension(10, 10), 0.7f,  color);

		return new BufferedImageTexture(image);	
	}
	
	/**
	 * Scale factor for the mark size.
	 */
	@Override
	public double getScale(GisAgent agent) {
		if (agent.isWater()){
			return 2;
		}
		else{
			return 1;
		}
	}

	@Override
	public double getHeading(GisAgent agent) {
		return 0;
	}
	
	/**
	 * The agent on-screen label.  Return null instead of empty string "" for better
	 *   performance.
	 */
	@Override
	public String getLabel(GisAgent agent) {
		return "" + agent.getWaterRate();
	}

	@Override
	public Color getLabelColor(GisAgent agent) {
		if (agent.isWater()){
			return Color.BLUE;
		}
		else{
			return Color.YELLOW;
		}
	}
	
	/**
	 * Return an Offset that determines the label position relative to the mark 
	 * position.  @see gov.nasa.worldwind.render.Offset
	 * 
	 */
	@Override
	public Offset getLabelOffset(GisAgent agent) {
		return labelOffset;
	}

	@Override
	public Font getLabelFont(GisAgent obj) {
		return null;
	}

	/** Width of the line that connects an elevated mark with the surface.  Use
	 *    a value of 0 to disable line drawing.
	 *   
	 */
	@Override
	public double getLineWidth(GisAgent agent) {
		if (agent.isWater()){
			return 2;
		}
		else{
			return 0;
		}
	}

	@Override
	public Material getLineMaterial(GisAgent obj, Material lineMaterial) {
		if (lineMaterial == null){
			lineMaterial = new Material(Color.RED);
		}
		
		return lineMaterial;
	}

	@Override
	public Offset getIconOffset(GisAgent obj) {
		// TODO Auto-generated method stub
		return null;
	}
}