package socialForce.enterable;

import socialForce.markup.impl.Point;

public interface Enterable {

	public boolean contains(Point p);
	public Point getRandomPointInside();
}
