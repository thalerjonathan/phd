package socialForce.markup.impl;

import socialForce.enterable.Enterable;
import socialForce.markup.Markup;

public class Rect implements Markup, Enterable {

	private Point p;
	private double width;
	private double height;
	
	public Rect(Point p, double width, double height) {
		this.p = p;
		this.width = width;
		this.height = height;
	}
	
	public Point getRef() {
		return this.p;
	}
	
	public double getWidth() {
		return this.width;
	}
	
	public double getHeight() {
		return this.height;
	}
	
	@Override
	public double getNearestPoint(double x, double y, Point p) {
		// TODO implement
		return 0;
	}

	@Override
	public boolean contains(Point p) {
		// TODO implement
		return false;
	}

	@Override
	public Point getRandomPointInside() {
		double randX = this.p.getX() + Math.random() * width;
		double randY = this.p.getY() + Math.random() * height;
		
		return new Point(randX, randY);
	}
}
