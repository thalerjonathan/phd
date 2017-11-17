package socialForce.scenario.pillarHall.rendering;

import java.awt.Color;
import java.awt.Font;
import java.awt.geom.Path2D;

import repast.simphony.visualizationOGL2D.StyleOGL2D;
import saf.v3d.ShapeFactory2D;
import saf.v3d.scene.Position;
import saf.v3d.scene.VSpatial;
import socialForce.markup.Point;
import socialForce.markup.Rect;
import socialForce.scenario.pillarHall.SocialForceToRePastTranslator;

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
			Point topLeft = SocialForceToRePastTranslator.transformSocialForceMeterToRePastPixel(new Point(0, 0));
			Point topRight = SocialForceToRePastTranslator.transformSocialForceMeterToRePastPixel(new Point(r.getWidth(), 0));
			Point bottomRight = SocialForceToRePastTranslator.transformSocialForceMeterToRePastPixel(new Point(r.getWidth(), r.getHeight()));
			Point bottomLeft = SocialForceToRePastTranslator.transformSocialForceMeterToRePastPixel(new Point(0, r.getHeight()));
				
			Path2D.Double wallPath = new Path2D.Double();
			wallPath.moveTo(topLeft.getX(), topLeft.getY());
			wallPath.lineTo(topRight.getX(), topRight.getY());
			wallPath.lineTo(bottomRight.getX(), bottomRight.getY());
			wallPath.lineTo(bottomLeft.getX(), bottomLeft.getY());
			wallPath.lineTo(topLeft.getX(), topLeft.getY());
			
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
		return 0;
	}

	public float getLabelYOffset(Rect object) {
		return 0;
	}
}
