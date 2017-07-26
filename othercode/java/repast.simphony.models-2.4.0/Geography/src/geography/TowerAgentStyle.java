package geography;

import gov.nasa.worldwind.WorldWind;
import gov.nasa.worldwind.avlist.AVKey;
import gov.nasa.worldwind.render.BasicWWTexture;
import gov.nasa.worldwind.render.Offset;
import gov.nasa.worldwind.render.WWTexture;

import java.net.URL;

import repast.simphony.visualization.gis3D.style.DefaultMarkStyle;

/**
 * Style for Tower Agents.
 * 
 * @author Eric Tatara
 *
 */
public class TowerAgentStyle extends DefaultMarkStyle<RadioTower>{

	/**
	 * The gov.nasa.worldwind.render.Offset is used to position the icon from 
	 *   the mark point location.  If no offset is provided, the lower left corner
	 *   of the icon is located at the point (lat lon) position.  Using values of
	 *   (0.5,0.5) will position the icon center over the lat lon location.
	 *   The first two arguments in the Offset constructor are the x and y 
	 *   offset values.  The third and fourth arguments are the x and y units 
	 *   for the offset. AVKey.FRACTION represents units of the image texture 
	 *   size, with 1.0 being one image width/height.  AVKey.PIXELS can be used 
	 *   to specify the offset in pixels. 
	 */
	Offset iconOffset = new Offset(0.5d, 0.5d, AVKey.FRACTION, AVKey.FRACTION);
	
	/**
	 * Here we set the appearance of the TowerAgent using a non-changing icon.
	 */
	@Override
	public WWTexture getTexture(RadioTower agent, WWTexture texture) {
			
		// If the texture is already defined, then just return the same texture since
		//  we don't want to update the tower agent appearance.  The only time the 
		//  below code will actually be used is on the initialization of the display
		//  when the icons are created.
		if (texture != null)
			return texture;
		
		// BasicWWTexture is useful when the texture is a non-changing image.
		URL localUrl = WorldWind.getDataFileStore().requestFile("icons/radio.png");
		if (localUrl != null)	{
			return new BasicWWTexture(localUrl, false);
		}
		
		return null;
	}
	
	@Override
	public Offset getIconOffset(RadioTower agent){
		return iconOffset;
	}
}