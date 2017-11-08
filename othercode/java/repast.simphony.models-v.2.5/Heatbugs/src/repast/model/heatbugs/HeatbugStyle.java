package repast.model.heatbugs;

import java.awt.Color;
import java.awt.Font;

import javax.swing.JLabel;

import repast.simphony.visualizationOGL2D.StyleOGL2D;
import saf.v3d.ShapeFactory2D;
import saf.v3d.scene.Position;
import saf.v3d.scene.VSpatial;

public class HeatbugStyle implements StyleOGL2D<Heatbug> {

	private Font font1, font2;
	private ShapeFactory2D shapeFactory;

	public HeatbugStyle() {
		font1 = new JLabel().getFont();
		font2 = new Font("SansSerif", Font.BOLD, 20);
	}

	public void init(ShapeFactory2D shapeFactory) {
		this.shapeFactory = shapeFactory;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * repast.simphony.visualizationOGL2D.StyleOGL2D#getBorderColor(java.lang
	 * .Object)
	 */
	public Color getBorderColor(Heatbug object) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * repast.simphony.visualizationOGL2D.StyleOGL2D#getBorderSize(java.lang
	 * .Object)
	 */
	public int getBorderSize(Heatbug object) {
		return 0;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * repast.simphony.visualizationOGL2D.StyleOGL2D#getColor(java.lang.Object)
	 */
	public Color getColor(Heatbug object) {
		return Color.green;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * repast.simphony.visualizationOGL2D.StyleOGL2D#getRotation(java.lang.Object
	 * )
	 */
	public float getRotation(Heatbug object) {
		return 0;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * repast.simphony.visualizationOGL2D.StyleOGL2D#getScale(java.lang.Object)
	 */
	public float getScale(Heatbug object) {
		return 1;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * repast.simphony.visualizationOGL2D.StyleOGL2D#getVSpatial(java.lang.Object
	 * , saf.v3d.scene.VSpatial)
	 */
	public VSpatial getVSpatial(Heatbug object, VSpatial spatial) {
		if (spatial == null)
			return shapeFactory.createCircle(4, 16);
		return spatial;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * repast.simphony.visualizationOGL2D.StyleOGL2D#getLabel(java.lang.Object)
	 */
	public String getLabel(Heatbug object) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * repast.simphony.visualizationOGL2D.StyleOGL2D#getLabelColor(java.lang
	 * .Object)
	 */
	public Color getLabelColor(Heatbug object) {
		return Color.WHITE;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * repast.simphony.visualizationOGL2D.StyleOGL2D#getLabelFont(java.lang.
	 * Object)
	 */
	public Font getLabelFont(Heatbug object) {
		if (Math.random() < .5) {
			return font1;
		} else {
			return font2;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * repast.simphony.visualizationOGL2D.StyleOGL2D#getLabelPosition(java.lang
	 * .Object)
	 */
	public Position getLabelPosition(Heatbug object) {
		return Position.SOUTH;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * repast.simphony.visualizationOGL2D.StyleOGL2D#getLabelXOffset(java.lang
	 * .Object)
	 */
	public float getLabelXOffset(Heatbug object) {
		// TODO Auto-generated method stub
		return 0;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * repast.simphony.visualizationOGL2D.StyleOGL2D#getLabelYOffset(java.lang
	 * .Object)
	 */
	public float getLabelYOffset(Heatbug object) {
		// TODO Auto-generated method stub
		return 0;
	}
}
