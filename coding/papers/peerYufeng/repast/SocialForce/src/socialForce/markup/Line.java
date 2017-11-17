package socialForce.markup;

import socialForce.Utils;

public class Line implements IMarkup {

	private Point from;
	private Point to;
	private double len;
	
	public Line(Point from, Point to) {
		this.from = from;
		this.to = to;
		
		this.len = Utils.distance(from, to);
	}

	public double getFromX() {
		return this.from.getX();
	}
	
	public double getFromY() {
		return this.from.getY();
	}
	
	public double getToX() {
		return this.to.getX();
	}
	
	public double getToY() {
		return this.to.getY();
	}
	
	public Point getVecFromTo() {
		return new Point(to.getX() - from.getX(), to.getY() - from.getY());
	}
	
	public double length() {
		return this.len;
	}
	
	public boolean contains(double x, double y) {
		// Test if the shape contains the point with the given coordinates (relative to this shape's container, i.e. in the same system with the coordinates of this shape, x and y)
		
		// NOTE: this also returns true if the point lies on the extension of the line-segment
		double dfrom = Utils.distance(from, new Point(x, y));
		double dto = Utils.distance(to, new Point(x, y));
		double delta = Math.abs(dfrom - dto);
		
		return delta < 0.01;
	}

	@Override
	public double getNearestPoint(double x, double y, Point p) {
		// NOTE: this is inspired by https://en.wikibooks.org/wiki/Linear_Algebra/Orthogonal_Projection_Onto_a_Line
		
		Point vecFromTo = getVecFromTo();
		
		double dotProdVecPoint = vecFromTo.getX() * x + vecFromTo.getY() * y;
		double dotProdVecVec = vecFromTo.getX() * vecFromTo.getX() + vecFromTo.getY() * vecFromTo.getY();
		double dotProdsDiv = dotProdVecPoint / dotProdVecVec;
		Point proj = new Point(vecFromTo.getX() * dotProdsDiv, vecFromTo.getY() * dotProdsDiv);
		
		p.override(proj.getX(), proj.getY());
		return Utils.distance(proj, new Point(x, y));
		
		/*
		double dist1 = Utils.distance(this.from.getX(), this.from.getY(), x, y);
		double dist2 = Utils.distance(this.to.getX(), this.to.getY(), x, y);
		
		if ( dist1 >= dist2 ) {
			p.override(this.from.getX(), this.from.getY());
			return dist1;
		} 
		
		p.override(this.to.getX(), this.to.getY());
		return dist2;
		*/
	}
}
