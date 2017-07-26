package PredatorPrey;

import java.awt.Color;

import repast.simphony.valueLayer.ValueLayer;
import repast.simphony.visualizationOGL2D.ValueLayerStyleOGL;

/**
 * Style for Grass value layer in 2D display.
 * 
 * @author Eric Tatara
 */
public class GrassStyle2D implements ValueLayerStyleOGL {

	protected ValueLayer layer;
	private Color tan = new Color(205, 133, 63);

	public void init(ValueLayer layer) {
		this.layer = layer;
	}

	public float getCellSize() {
		return 15.0f;
	}

	/**
	 * Return the color based on the value at given coordinates.
	 */
	public Color getColor(double... coordinates) {
		double v = layer.get(coordinates);
		
		if (v == 1)
			return Color.GREEN;
		else 
			return tan;
	}
}