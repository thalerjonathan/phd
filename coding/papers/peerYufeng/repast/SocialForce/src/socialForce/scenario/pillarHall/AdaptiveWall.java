package socialForce.scenario.pillarHall;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.space.continuous.ContinuousSpace;
import socialForce.Utils;
import socialForce.geom.Point;

public class AdaptiveWall {

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
	
	private PillarHall main;
	private ContinuousSpace<Object> space;
	
	public AdaptiveWall(PillarHall main, ContinuousSpace<Object> space) {
		this.main = main;
		this.space = space;
		
		this.x = 200;
		this.y = 270;
		
		this.yMin = 225;
		this.yMax = 400;
		
		this.totalWidth = 300;
		
		this.speed = 0;
		
		this.Ai = 5;
		this.Bi = 5;
		this.AiS = 200;
		this.BiS = 0.1;
		this.K = 1.2 * 10;
		
		this.wallsX = new ArrayList<Double>();
		this.wallsX.add(50.0);
		this.wallsX.add(100.0);
		this.wallsX.add(200.0);
		
		this.wallsWidth = new ArrayList<Double>();
		this.wallsWidth.add(50.0);
		this.wallsWidth.add(100.0);
		this.wallsWidth.add(50.0);
		
		this.doorsX = new ArrayList<Double>();
		this.doorsX.add(75.0);
		this.doorsX.add(325.0);
	}
	
	@ScheduledMethod(start = 0, interval = PillarHall.UNIT_TIME)
	public void action() {
		socialForce();
		double acceV = (sumFijV);
		double tspeed = speed + acceV*PillarHall.UNIT_TIME;
		//System.out.println(acceV);
		if(tspeed>-0.1 && tspeed <0.1){
			speed = tspeed;
		}
		double ty = y + (speed*PillarHall.UNIT_TIME)*PillarHall.METER_2_PX;
		if(ty>yMin && ty<yMax){
			y=ty;
		}
		
		this.updatePosition();
	}
	
	public void updatePosition() {
		Point p = Utils.anylogicToRePast(new Point(this.x, this.y));
		space.moveTo(this, p.x, p.y);
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
			double ax = x/PillarHall.METER_2_PX;
			double ay = y/PillarHall.METER_2_PX;
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
	
	private double getNearestPoint(double x1, double y1, Point p) {
		p.x = x1;
		p.y = this.y;
		return Utils.sqr(y1 - this.y);
	}
}
