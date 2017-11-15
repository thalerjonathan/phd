package socialForce.scenario.pillarHall;

import java.awt.Color;

import repast.simphony.space.continuous.ContinuousSpace;
import socialForce.Utils;
import socialForce.geom.Point;

public class Person {

	private double x;
	private double y;
	
	private double pxX;
	private double pxY;
	
	private Color color;
	
	private Group belongedGroup;
	
	private double readingTime;
	
	private double ri;
	private double vi0;
	
	private PillarHall main;
	private ContinuousSpace<Object> space;
	
	// NOTE: start is in pixel-space
	public Person(PillarHall main, ContinuousSpace<Object> space, Point start) {
		this.main = main;
		this.space = space;
		
		this.pxX = start.x;
		this.pxY = start.y;
		this.x = start.x / PillarHall.METER_2_PX;
		this.y = start.y / PillarHall.METER_2_PX;
		
		this.color = Color.WHITE;
		
		this.readingTime = Utils.uniform(10,60);
		
		this.ri = Utils.uniform(0.15,0.25);
		this.vi0 = 1.4;
		
	}

	public double getX() {
		return this.x;
	}
	
	public double getY() {
		return this.y;
	}
	
	public void setRi(double ri) {
		this.ri = ri;
	}
	
	public double getRi() {
		return this.ri;
	}
	
	public void setVi0(double vi0) {
		this.vi0 = vi0;
	}
	
	public void setColor(Color c) {
		this.color = c;
	}
	
	public void setGroup(Group g) {
		this.belongedGroup = g;
	}
	
	public void setReadingTime(double d) {
		this.readingTime = d;
	}
	
	public void updatePosition() {
		Point p = Utils.anylogicToRePast(new Point(this.pxX, this.pxY));
		space.moveTo(this, p.x, p.y);
	}
}
