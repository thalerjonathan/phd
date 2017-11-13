package socialForce.markup;

import socialForce.Utils;
import socialForce.geom.Point;

public class Wall implements IMarkup {

	private Point ref;
	private Point[] points;
	
	public Wall(Point pRef, Point... ps) {
		this.ref = pRef;
		
		this.points = new Point[ps.length + 1];
		this.points[0] = pRef;
		
		for (int i = 0; i < ps.length; ++i) {
			Point p = ps[i];
			this.points[i+1] = new Point(pRef.x + p.x, pRef.y + p.y);
		}
	}
	
	public Point getRefPoint() {
		return this.ref;
	}
	
	public Point[] getPoints() {
		return this.points;
	}
	
	@Override
	public double getNearestPoint(double x, double y, Point p) {
		int minIdx = -1;
		double minDist = Double.POSITIVE_INFINITY;
		Point from = new Point(x, y);
		
		for (int i = 0; i < points.length; ++i) {
			double d = Utils.distance(from, points[i]);
		
			if (d < minDist) {
				minDist = d;
				minIdx = i;
			}
		}

		Point min = points[minIdx];
		p.x = min.x;
		p.y = min.y;
		
		return minDist;
	}
}
