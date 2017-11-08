package geography;

import java.awt.Color;

import gov.nasa.worldwind.render.SurfacePolyline;
import repast.simphony.space.graph.RepastEdge;
import repast.simphony.visualization.gis3D.style.NetworkStyleGIS;

/**
 * Simple network style class example.
 * 
 * @author Eric Tatara
 *
 */
public class MyNetworkStyle implements NetworkStyleGIS {

	@Override
	public SurfacePolyline getSurfaceShape(RepastEdge edge, SurfacePolyline shape) {
		return new SurfacePolyline();
	}

	@Override
	public Color getLineColor(RepastEdge edge) {
		return Color.GREEN;
	}

	@Override
	public double getLineOpacity(RepastEdge edge) {
		return 0.5;
	}

	@Override
	public double getLineWidth(RepastEdge edge) {
		return edge.getWeight();
	}
}
