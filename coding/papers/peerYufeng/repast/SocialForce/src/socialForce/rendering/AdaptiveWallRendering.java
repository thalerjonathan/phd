package socialForce.rendering;

import java.awt.Color;
import java.awt.Font;
import java.awt.geom.Line2D;

import repast.simphony.visualizationOGL2D.StyleOGL2D;
import saf.v3d.ShapeFactory2D;
import saf.v3d.scene.Position;
import saf.v3d.scene.VSpatial;
import socialForce.AdaptiveWall;
import socialForce.Utils;
import socialForce.geom.Point;

public class AdaptiveWallRendering implements StyleOGL2D<AdaptiveWall> {

	private ShapeFactory2D shapeFactory;
	
	@Override
	public void init(ShapeFactory2D factory) {
		this.shapeFactory = factory;
	}

	@Override
	public VSpatial getVSpatial(AdaptiveWall w, VSpatial spatial) {
		if (spatial == null) {
			// note: the positioning of the AdaptiveWall sets the x and y to the center of the wall
			
			double halfHeight = w.height * 0.5;
			
			Point from = Utils.anylogicToRePast(new Point(w.x, w.y - halfHeight));
			Point to = Utils.anylogicToRePast(new Point(w.x, w.y + halfHeight));
			
			Line2D.Double adaptWallLine = new Line2D.Double(from.x, from.y, to.x, to.y);
			
			return shapeFactory.createShape(adaptWallLine);
		}
		
		return spatial;
	}

	@Override
	public Color getColor(AdaptiveWall object) {
		return Color.GRAY;
	}

	@Override
	public int getBorderSize(AdaptiveWall object) {
		return 0;
	}

	@Override
	public Color getBorderColor(AdaptiveWall object) {
		return null;
	}

	@Override
	public float getRotation(AdaptiveWall object) {
		return 0;
	}

	@Override
	public float getScale(AdaptiveWall object) {
		return 1;
	}

	@Override
	public String getLabel(AdaptiveWall object) {
		return null;
	}

	@Override
	public Font getLabelFont(AdaptiveWall object) {
		return null;
	}

	@Override
	public float getLabelXOffset(AdaptiveWall object) {
		return 0;
	}

	@Override
	public float getLabelYOffset(AdaptiveWall object) {
		return 0;
	}

	@Override
	public Position getLabelPosition(AdaptiveWall object) {
		return null;
	}

	@Override
	public Color getLabelColor(AdaptiveWall object) {
		return null;
	}

}
