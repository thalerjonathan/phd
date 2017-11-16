package socialForce.scenario.pillarHall.rendering;

import java.awt.Color;
import java.awt.Font;
import java.awt.geom.Path2D;

import repast.simphony.visualizationOGL2D.StyleOGL2D;
import saf.v3d.ShapeFactory2D;
import saf.v3d.scene.Position;
import saf.v3d.scene.VSpatial;
import socialForce.markup.Rect;

public class RectRendering implements StyleOGL2D<Rect> {

	private Color PERU_COLOR = new Color(205, 133, 63);
	private ShapeFactory2D shapeFactory;
	
	public void init(ShapeFactory2D shapeFactory) {
		this.shapeFactory = shapeFactory;
	}

	public Color getBorderColor(Rect object) {
		return null;
	}

	public int getBorderSize(Rect object) {
		return 0;
	}

	public Color getColor(Rect object) {
		return PERU_COLOR;
	}

	public float getRotation(Rect object) {
		return 0;
	}

	public float getScale(Rect object) {
		return 1;
	}

	public VSpatial getVSpatial(Rect r, VSpatial spatial) {
		if (spatial == null) {
			// NOTE: we are working in LOCAL space
			
			Path2D.Double wallPath = new Path2D.Double();
			wallPath.moveTo(0, 0);
			wallPath.lineTo(0, r.getWidth() * 25);
			wallPath.lineTo(r.getHeight() * 25, r.getWidth() * 25);
			wallPath.lineTo(r.getHeight() * 25, 0);
			wallPath.lineTo(0, 0);
			
			return shapeFactory.createShape(wallPath);
		}
		
		return spatial;
	}

	public String getLabel(Rect object) {
		return null;
	}

	public Color getLabelColor(Rect object) {
		return Color.WHITE;
	}

	public Font getLabelFont(Rect object) {
		return null;
	}

	public Position getLabelPosition(Rect object) {
		return Position.SOUTH;
	}
	
	public float getLabelXOffset(Rect object) {
		// TODO Auto-generated method stub
		return 0;
	}

	public float getLabelYOffset(Rect object) {
		// TODO Auto-generated method stub
		return 0;
	}
}
