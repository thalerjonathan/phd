package socialForce.markup;

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

	@Override
	public double getNearestPoint(double x, double y, Point p) {
		// TODO Auto-generated method stub
		return 0;
	}
}
