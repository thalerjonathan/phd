package socialForce.rendering;

import java.awt.Color;
import java.awt.Font;
import java.awt.geom.Path2D;

import repast.simphony.visualizationOGL2D.StyleOGL2D;
import saf.v3d.ShapeFactory2D;
import saf.v3d.scene.Position;
import saf.v3d.scene.VSpatial;
import socialForce.Utils;
import socialForce.geom.Point;
import socialForce.markup.Wall;

public class WallRendering implements StyleOGL2D<Wall> {
	
	private Color PERU_COLOR = new Color(205, 133, 63);
	private ShapeFactory2D shapeFactory;
	
	public void init(ShapeFactory2D shapeFactory) {
		this.shapeFactory = shapeFactory;
	}

	public Color getBorderColor(Wall object) {
		return null;
	}

	public int getBorderSize(Wall object) {
		return 0;
	}

	public Color getColor(Wall object) {
		return PERU_COLOR;
	}

	public float getRotation(Wall object) {
		return 0;
	}

	public float getScale(Wall object) {
		return 1;
	}

	public VSpatial getVSpatial(Wall w, VSpatial spatial) {
		if (spatial == null) {
			// NOTE: this rendering works in GLOBAL coordinate system and constructs a shape with GLOBAL COORDINATES (rotation would not work properly)!!!
			Point[] points = w.getPoints();
			Point pRef = Utils.anylogicToRePast(w.getRefPoint());
			
			Path2D.Double wallPath = new Path2D.Double();
			wallPath.moveTo(pRef.x, pRef.y);
			
			for (int i = 1; i < points.length; ++i) {
				Point p = Utils.anylogicToRePast(points[i]);
				
				wallPath.lineTo(p.x, p.y);
			}
			
			return shapeFactory.createShape(wallPath);
		}
		
		return spatial;
	}

	public String getLabel(Wall object) {
		return null;
	}

	public Color getLabelColor(Wall object) {
		return Color.WHITE;
	}

	public Font getLabelFont(Wall object) {
		return null;
	}

	public Position getLabelPosition(Wall object) {
		return Position.SOUTH;
	}

	
	public float getLabelXOffset(Wall object) {
		// TODO Auto-generated method stub
		return 0;
	}

	public float getLabelYOffset(Wall object) {
		// TODO Auto-generated method stub
		return 0;
	}

}
