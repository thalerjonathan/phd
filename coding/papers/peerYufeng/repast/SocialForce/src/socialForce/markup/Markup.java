package socialForce.markup;

import socialForce.markup.impl.Point;

public interface Markup {
	public Point getRef();
	
	public double getNearestPoint(double x, double y, Point p);
}
