package geozombies;

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
		if (zone.isActive()) 
			return Color.RED;

		return Color.CYAN;
	}

	@Override
	public double getFillOpacity(ZoneAgent zone) {
		if(!zone.isVisible()) return 0;
		
		return 0.15;
	}

	@Override
	public Color getLineColor(ZoneAgent zone) {
		return Color.GREEN;
	}

	@Override
	public double getLineOpacity(ZoneAgent zone) {
		if(!zone.isVisible()) return 0;
		
		return 0.15;
	}

	@Override
	public double getLineWidth(ZoneAgent obj) {
		return 3;
	}
}