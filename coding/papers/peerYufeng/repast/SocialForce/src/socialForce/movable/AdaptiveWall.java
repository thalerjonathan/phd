package socialForce.movable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.space.continuous.ContinuousSpace;
import socialForce.scenario.hall.Hall;

public class AdaptiveWall implements Moveable {

	private double x;
	private double y;
	
	private double yMin;
	private double yMax;
	
	private double totalWidth;
	
	private double sumFijV;
	
	private double speed;
	
	private double Ai;
	private double Bi;
	private double AiS;
	private double BiS;
	private double K;
	
	private List<Double> wallsX;
	private List<Double> wallsWidth;
	private List<Double> doorsX;
	
	private Hall main;
	private ContinuousSpace<Object> space;
	
	public AdaptiveWall(Hall main, ContinuousSpace<Object> space) {
		this.main = main;
		this.space = space;
		
		this.x = 8;
		this.y = 10.8;
		
		this.yMin = 9;
		this.yMax = 16;
		
		this.totalWidth = 12;
		
		this.speed = 0;
		
		this.Ai = 5;
		this.Bi = 5;
		this.AiS = 200;
		this.BiS = 0.1;
		this.K = 1.2 * 10;
		
		this.wallsX = new ArrayList<Double>();
		this.wallsX.add(2.0);
		this.wallsX.add(4.0);
		this.wallsX.add(8.0);
		
		this.wallsWidth = new ArrayList<Double>();
		this.wallsWidth.add(2.0);
		this.wallsWidth.add(4.0);
		this.wallsWidth.add(2.0);
		
		this.doorsX = new ArrayList<Double>();
		this.doorsX.add(3.0);
		this.doorsX.add(13.0);
	}
	
	@ScheduledMethod(start = 0, interval = Hall.UNIT_TIME)
	public void move() {
		socialForce();
		double acceV = (sumFijV);
		double tspeed = speed + acceV*Hall.UNIT_TIME;
		//System.out.println(acceV);
		if(tspeed>-0.1 && tspeed <0.1){
			speed = tspeed;
		}
		double ty = y + (speed*Hall.UNIT_TIME);
		if(ty>yMin && ty<yMax){
			y=ty;
		}
		
		this.updatePosition();
	}
	
	public void updatePosition() {
		space.moveTo(this, this.x, this.y);
	}
	
	public List<Double> getWalls() {
		return Collections.unmodifiableList(this.wallsX);
	}
	
	public List<Double> getWallWidths() {
		return Collections.unmodifiableList(this.wallsWidth);
	}
	
	public List<Double> getDoors() {
		return Collections.unmodifiableList(this.doorsX);
	}
	
	public double getX() {
		return this.x;
	}
	
	public double getY() {
		return this.y;
	}
	
	public double getTotalWidth() {
		return this.totalWidth;
	}
	
	private void socialForce() {
		sumFijV = 0;
		for(Person j : main.getPeople()){
			//if(!j.inState(j.reading) && !j.inState(j.moving)){continue;}
			double ax = x;
			double ay = y;
			double dij = -1;
			dij = Math.abs(ay-j.getY());
			double nij1,nij2;
			double rij,gx;
			double fpsy;
			double fijH,fijV;
			rij = j.getRi();

			nij2 = (ay-j.getY())/dij;
			fpsy = Ai*Math.exp((rij-dij)/Bi);

			fijV = (fpsy)*nij2;
			sumFijV += fijV;
		}
	}
}
