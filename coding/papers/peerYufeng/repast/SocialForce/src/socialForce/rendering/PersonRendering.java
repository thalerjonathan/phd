package socialForce.rendering;

import java.awt.Color;
import java.awt.Font;
import java.awt.geom.Path2D;

import repast.simphony.visualizationOGL2D.StyleOGL2D;
import saf.v3d.ShapeFactory2D;
import saf.v3d.scene.Position;
import saf.v3d.scene.VSpatial;
import socialForce.Person;
import socialForce.Utils;
import socialForce.geom.Point;

public class PersonRendering implements StyleOGL2D<Person> {

	private final static double PERSON_SIZE = 10;
	private ShapeFactory2D shapeFactory;
	
	@Override
	public void init(ShapeFactory2D factory) {
		this.shapeFactory = factory;
	}

	@Override
	public VSpatial getVSpatial(Person person, VSpatial spatial) {
		if (spatial == null) {
			// NOTE: this constructs a shape in GLOBAL coordinate system (rotation would not work properly)!!!
			/*
			Point p = Utils.anylogicToRePast(new Point(person.pxX, person.pxY));
			
			Point left = Utils.anylogicToRePast(new Point(p.x - PERSON_SIZE, p.y));
			Point right = Utils.anylogicToRePast(new Point(p.x + PERSON_SIZE, p.y));
			Point top = Utils.anylogicToRePast(new Point(p.x, p.y + PERSON_SIZE));
			*/
			
			// NOTE: this constructs a shape in LOCAL coordinate system 
			Point left = new Point(-PERSON_SIZE, 0);
			Point right = new Point(PERSON_SIZE, 0);
			Point top = new Point(0, PERSON_SIZE);
			
			Path2D.Double personShape = new Path2D.Double();
			personShape.moveTo(left.x, left.y);
			personShape.lineTo(right.x, right.y);
			personShape.lineTo(top.x, top.y);
			personShape.lineTo(left.x, left.y);
			
			spatial = shapeFactory.createShape(personShape);

			return spatial;
		}
		
		return spatial;
	}

	@Override
	public Color getColor(Person p) {
		return Color.BLACK; //;p.color;
	}

	@Override
	public int getBorderSize(Person object) {
		return 1;
	}

	@Override
	public Color getBorderColor(Person object) {
		return Color.BLACK;
	}

	@Override
	public float getRotation(Person p) {
		double headinDegree = p.heading * (180.0/Math.PI); // NOTE: heading is in radians, need to translate it into degrees
		return (float)headinDegree; 
	}

	@Override
	public float getScale(Person object) {
		return 1;
	}

	@Override
	public String getLabel(Person object) {
		return null;
	}

	@Override
	public Font getLabelFont(Person object) {
		return null;
	}

	@Override
	public float getLabelXOffset(Person object) {
		return 0;
	}

	@Override
	public float getLabelYOffset(Person object) {
		return 0;
	}

	@Override
	public Position getLabelPosition(Person object) {
		return null;
	}

	@Override
	public Color getLabelColor(Person object) {
		return null;
	}
}
