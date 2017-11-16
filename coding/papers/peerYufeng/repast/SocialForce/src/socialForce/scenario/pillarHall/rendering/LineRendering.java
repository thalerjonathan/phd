package socialForce.scenario.pillarHall.rendering;

import java.awt.Color;
import java.awt.Font;
import java.awt.geom.Path2D;

import repast.simphony.visualizationOGL2D.StyleOGL2D;
import saf.v3d.ShapeFactory2D;
import saf.v3d.scene.Position;
import saf.v3d.scene.VSpatial;
import socialForce.markup.Line;

public class LineRendering implements StyleOGL2D<Line> {
	
	private Color PERU_COLOR = new Color(205, 133, 63);
	private ShapeFactory2D shapeFactory;
	
	public void init(ShapeFactory2D shapeFactory) {
		this.shapeFactory = shapeFactory;
	}

	public Color getBorderColor(Line object) {
		return null;
	}

	public int getBorderSize(Line object) {
		return 0;
	}

	public Color getColor(Line object) {
		return PERU_COLOR;
	}

	public float getRotation(Line object) {
		return 0;
	}

	public float getScale(Line object) {
		return 1;
	}

	public VSpatial getVSpatial(Line w, VSpatial spatial) {
		if (spatial == null) {
			// NOTE: we are working in LOCAL space
			
			Path2D.Double wallPath = new Path2D.Double();
			wallPath.moveTo(0, 0);
			wallPath.lineTo(-w.getFromToDiffX() * 25, -w.getFromToDiffY() * 25);
			
			return shapeFactory.createShape(wallPath);
		}
		
		return spatial;
	}

	public String getLabel(Line object) {
		return null;
	}

	public Color getLabelColor(Line object) {
		return Color.WHITE;
	}

	public Font getLabelFont(Line object) {
		return null;
	}

	public Position getLabelPosition(Line object) {
		return Position.SOUTH;
	}

	
	public float getLabelXOffset(Line object) {
		// TODO Auto-generated method stub
		return 0;
	}

	public float getLabelYOffset(Line object) {
		// TODO Auto-generated method stub
		return 0;
	}

}