package socialForce.markup.impl;

import socialForce.markup.Markup;
import socialForce.misc.Utils;

public class Point implements Markup {

	private double x;
	private double y;
	
	public Point() {
	}
	
	public Point(Point p) {
		this.x = p.x;
		this.y = p.y;
	}
	
	public Point(double x, double y) {
		this.x = x;
		this.y = y;
	}
	
	@Override
	public Point getRef() {
		return this;
	}
	
	public double getX() {
		return this.x;
	}
	
	public double getY() {
		return this.y;
	}

	public void override(double x, double y) {
		this.x = x;
		this.y = y;
	}
	
	@Override
	public double getNearestPoint(double x, double y, Point p) {
		double dist = Point.distance(this.x, this.y, x, y);
		
		p.x = this.x;
		p.y = this.y;
		
		return dist;
	}
	
	public static double distance(Point p1, Point p2) {
		return Math.sqrt(Utils.sqr(p1.getX()-p2.getX()) + Utils.sqr(p1.getY()-p2.getY()));
	}
	
	public static double distance(double x1, double y1, double x2, double y2) {
		return Math.sqrt(Utils.sqr(x1-x2) + Utils.sqr(y1-y2));
	}
}
