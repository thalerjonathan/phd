package socialForce.markup.impl;

import socialForce.markup.Markup;

public class Circle implements Markup {

	private Point c;
	private double r;
	
	public Circle(Point c, double r) {
		this.c = c;
		this.r = r;
	}
	
	@Override
	public double getNearestPoint(double x, double y, Point p) {
		Vector vec = Vector.fromPoints(c, new Point(x, y)).normalize().scale(this.r);
		p.override(vec.getX(), vec.getY());
		
		return this.r;
	}

	@Override
	public Point getRef() {
		return new Point(c);
	}

	public double getRadius() {
		return this.r;
	}
}
