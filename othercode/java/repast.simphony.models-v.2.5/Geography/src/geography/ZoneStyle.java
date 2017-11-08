package geography;

import gov.nasa.worldwind.render.SurfacePolygon;
import gov.nasa.worldwind.render.SurfaceShape;

import java.awt.Color;

import repast.simphony.visualization.gis3D.style.SurfaceShapeStyle;

/**
 * Style for ZoneAgents.
 * 
 * @author Eric Tatara
 *
 */
public class ZoneStyle implements SurfaceShapeStyle<ZoneAgent>{

	@Override
	public SurfaceShape getSurfaceShape(ZoneAgent object, SurfaceShape shape) {
		return new SurfacePolygon();
	}

	@Override
	public Color getFillColor(ZoneAgent zone) {
		return Color.CYAN;
	}

	@Override
	public double getFillOpacity(ZoneAgent obj) {
		return 0.25;
	}

	/**
	 * If the zone has water then indicate with a BLUE outline.
	 */
	@Override
	public Color getLineColor(ZoneAgent zone) {
		if (zone.getWaterFlowRate() > 0)
			return Color.blue;
		else
			return Color.black;
	}

	@Override
	public double getLineOpacity(ZoneAgent obj) {
		return 1.0;
	}

	@Override
	public double getLineWidth(ZoneAgent obj) {
		return 3;
	}
}