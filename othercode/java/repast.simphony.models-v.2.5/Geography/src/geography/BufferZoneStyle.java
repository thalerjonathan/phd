package geography;

import gov.nasa.worldwind.render.SurfacePolygon;
import gov.nasa.worldwind.render.SurfaceShape;

import java.awt.Color;

import repast.simphony.visualization.gis3D.style.SurfaceShapeStyle;

/**
 * Style for BufferZoneAgent.
 * 
 * @author Eric Tatara
 *
 */
public class BufferZoneStyle implements SurfaceShapeStyle<BufferZoneAgent>{

	@Override
	public SurfaceShape getSurfaceShape(BufferZoneAgent object, SurfaceShape shape) {
		return new SurfacePolygon();
	}

	@Override
	public Color getFillColor(BufferZoneAgent zone) {
		return Color.YELLOW;
	}

	@Override
	public double getFillOpacity(BufferZoneAgent obj) {
		return 0.25;
	}

	@Override
	public Color getLineColor(BufferZoneAgent zone) {
		return Color.BLACK;
	}

	@Override
	public double getLineOpacity(BufferZoneAgent obj) {
		return 1.0;
	}

	@Override
	public double getLineWidth(BufferZoneAgent obj) {
		return 3;
	}
}