package geography;

import gov.nasa.worldwind.render.SurfacePolyline;
import gov.nasa.worldwind.render.SurfaceShape;

import java.awt.Color;

import repast.simphony.visualization.gis3D.style.SurfaceShapeStyle;

/**
 * Style for WaterLines.
 * 
 * @author Eric Tatara
 *
 */
public class WaterLineStyle implements SurfaceShapeStyle<WaterLine>{

	@Override
	public SurfaceShape getSurfaceShape(WaterLine object, SurfaceShape shape) {
	  return new SurfacePolyline();
	}

	@Override
	public Color getFillColor(WaterLine obj) {
		return null;
	}

	@Override
	public double getFillOpacity(WaterLine obj) {
		return 0;
	}

	@Override
	public Color getLineColor(WaterLine obj) {
		return Color.BLUE;
	}

	@Override
	public double getLineOpacity(WaterLine obj) {
		return 1.0;
	}

	@Override
	public double getLineWidth(WaterLine obj) {
		return 2;
	}
}