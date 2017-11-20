package socialForce.rendering;


import java.awt.Color;
import java.awt.Font;
import java.awt.geom.Ellipse2D;

import repast.simphony.visualizationOGL2D.StyleOGL2D;
import saf.v3d.ShapeFactory2D;
import saf.v3d.scene.Position;
import saf.v3d.scene.VSpatial;
import socialForce.markup.impl.Circle;

public class CircleRendering implements StyleOGL2D<Circle> {

	private ShapeFactory2D shapeFactory;
	
	@Override
	public void init(ShapeFactory2D factory) {
		this.shapeFactory = factory;
	}

	@Override
	public VSpatial getVSpatial(Circle c, VSpatial spatial) {
		if (spatial == null) {
			// NOTE: we are working in LOCAL space
			double radius = SocialForceToRePastTranslator.scaleSocialForceMeterToRePastPixel(c.getRadius());
			Ellipse2D.Double circle = new Ellipse2D.Double(0, 0, radius, radius);

			return shapeFactory.createShape(circle);
		}
		
		return spatial;
	}

	@Override
	public Color getColor(Circle object) {
		return RenderingUtils.PERU_COLOR;
	}

	@Override
	public int getBorderSize(Circle object) {
		return 0;
	}

	@Override
	public Color getBorderColor(Circle object) {
		return null;
	}

	@Override
	public float getRotation(Circle object) {
		return 0;
	}

	@Override
	public float getScale(Circle object) {
		return 1;
	}

	@Override
	public String getLabel(Circle object) {
		return null;
	}

	@Override
	public Font getLabelFont(Circle object) {
		return null;
	}

	@Override
	public float getLabelXOffset(Circle object) {
		return 0;
	}

	@Override
	public float getLabelYOffset(Circle object) {
		return 0;
	}

	@Override
	public Position getLabelPosition(Circle object) {
		return null;
	}

	@Override
	public Color getLabelColor(Circle object) {
		return null;
	}
}
