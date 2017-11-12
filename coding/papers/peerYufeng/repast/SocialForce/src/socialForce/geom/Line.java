package socialForce.geom;

import socialForce.Utils;

public class Line {

	private Point p1;
	private Point p2;
	private double len;
	
	public Line(Point p1, Point p2) {
		this.p1 = p1;
		this.p2 = p2;
		
		this.len = Utils.distance(p1, p2);
	}
	
	public double getX() {
		return this.p1.getX();
	}
	
	public double getY() {
		return this.p1.getY();
	}
	
	public double length() {
		return this.len;
	}
	
	public boolean contains(double x, double y) {
		// TODO: implement from anylogic:
		// Test if the shape contains the point with the given coordinates (relative to this shape's container, i.e. in the same system with the coordinates of this shape, x and y)
		
		// NOTE: this also returns true if the point lies on the extension of the line-segment
		double dp1 = Utils.distance(p1, new Point(x, y));
		double dp2 = Utils.distance(p2, new Point(x, y));
		double delta = Math.abs(dp1 - dp2);
		
		return delta < 0.01;
	}
}
