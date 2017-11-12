package socialForce.geom;

public class Rect {

	private Point p;
	private double width;
	private double height;
	
	public Rect(Point p, double width, double height) {
		this.p = p;
		this.width = width;
		this.height = height;
	}
	
	public Point randomPointInside() {
		Point p = null;
		
		double randX = this.p.getX() + Math.random() * width;
		double randY = this.p.getY() + Math.random() * height;
		
		return new Point(randX, randY);
	}
}
