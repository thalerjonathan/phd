package Mousetrap;

import java.awt.Color;
import java.awt.Paint;

import repast.simphony.valueLayer.ValueLayer;
import repast.simphony.visualization.visualization3D.style.ValueLayerStyle3D;

/**
 * @author Nick Collier
 * @author Eric Tatara
 */
public class MouseTrapValueLayerStyle implements ValueLayerStyle3D {

	private ValueLayer layer;

	public float getY(double... coordinates) {
		return layer.get(coordinates) == 0 ? -0.8f : -.4f;
	}

	public void addValueLayer(ValueLayer layer) {
		this.layer = layer;
	}

	public int getBlue(double... coordinates) {
		return 0;
	}

	public float getCellSize() {
		return .06f;
	}

	public int getGreen(double... coordinates) {
    return 0;
	}

	public int getRed(double... coordinates) {
		return 0;
	}

	public Paint getPaint(double... coordinates) {
		double v = layer.get(coordinates);

		if (v == 0)
			return Color.GREEN;
		else
			return Color.RED;
	}
}