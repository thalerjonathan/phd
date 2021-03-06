package socialForce.scenario.scene.rendering;

import java.awt.Color;
import java.awt.Font;
import java.awt.geom.Rectangle2D;

import repast.simphony.visualizationOGL2D.StyleOGL2D;
import saf.v3d.ShapeFactory2D;
import saf.v3d.scene.Position;
import saf.v3d.scene.VSpatial;
import socialForce.markup.impl.Point;
import socialForce.misc.Utils;
import socialForce.scene.museum.Screen;

public class ScreenRendering implements StyleOGL2D<Screen> {

	private ShapeFactory2D shapeFactory;
	
	@Override
	public void init(ShapeFactory2D factory) {
		this.shapeFactory = factory;
	}

	@Override
	public VSpatial getVSpatial(Screen s, VSpatial spatial) {
		if (spatial == null) {
			// NOTE: this rendering works in GLOBAL coordinate system and constructs a shape with GLOBAL COORDINATES (rotation would not work properly)!!!
			Point ref = Utils.anylogicToRePast(new Point(s.x, s.y));
			
			Rectangle2D.Double screenRect = new Rectangle2D.Double(ref.getX(), ref.getY(), 50, s.max - s.y);
			
			return shapeFactory.createShape(screenRect);
		}
		
		return spatial;
	}

	@Override
	public Color getColor(Screen object) {
		return Color.BLUE;
	}

	@Override
	public int getBorderSize(Screen object) {
		return 0;
	}

	@Override
	public Color getBorderColor(Screen object) {
		return null;
	}

	@Override
	public float getRotation(Screen object) {
		return 0;
	}

	@Override
	public float getScale(Screen object) {
		return 1;
	}

	@Override
	public String getLabel(Screen object) {
		return null;
	}

	@Override
	public Font getLabelFont(Screen object) {
		return null;
	}

	@Override
	public float getLabelXOffset(Screen object) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public float getLabelYOffset(Screen object) {
		return 0;
	}

	@Override
	public Position getLabelPosition(Screen object) {
		return null;
	}

	@Override
	public Color getLabelColor(Screen object) {
		return null;
	}

}
