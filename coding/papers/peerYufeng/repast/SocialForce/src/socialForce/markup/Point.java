package socialForce.markup;

import socialForce.Utils;

public class Point implements IMarkup {

	private double x;
	private double y;
	
	public Point() {
	}
	
	public Point(double x, double y) {
		this.x = x;
		this.y = y;
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
		double dist = Utils.distance(this.x, this.y, x, y);
		
		p.x = this.x;
		p.y = this.y;
		
		return dist;
	}
}
