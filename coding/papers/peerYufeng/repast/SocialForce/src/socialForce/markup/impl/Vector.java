package socialForce.markup.impl;

import socialForce.markup.Markup;

public class Vector implements Markup {

	private Point p;
	
	public Vector(double x, double y) {
		this.p = new Point(x, y);
	}
	
	public Vector(Point p) {
		this.p = new Point(p);
	}
	
	public double getX() {
		return this.p.getX();
	}

	public double getY() {
		return this.p.getY();
	}
	
	public Point toPoint() {
		return new Point(this.p);
	}
	
	@Override
	public Point getRef() {
		return this.toPoint();
	}
	
	@Override
	public double getNearestPoint(double x, double y, Point p) {
		// TODO implement
		return 0;
	}

	public Vector normalize() {
		double len = this.length();
		return this.scale(1/len); // new Vector(this.p.getX() / len, this.p.getY() / len);
	}
	
	public Vector scale(double s) {
		return new Vector(this.p.getX() * s, this.p.getY() * s);
	}
	
	public double length() {
		return Math.sqrt((p.getX() * p.getX()) + (p.getY() * p.getY()));
	}
	
	public static Vector fromPoints(Point p1, Point p2) {
		return new Vector(new Point(p2.getX() - p1.getX(), p2.getY() - p1.getY()));
	}
}
