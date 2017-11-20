package socialForce.scene.museum;

import java.awt.Color;

import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.ui.probe.ProbedProperty;
import socialForce.chart.museum.PersonMuseumStatechart;
import socialForce.markup.Wall;
import socialForce.markup.impl.Point;
import socialForce.misc.Utils;

/**
 * An proactive agent that chooses the destination by it
 * self and move under social force it recieved. The name
 * of social force variables are from the origional social 
 * force model paper by Helbing.
 * 
 * Pixels displayed should be converted into real world 
 * meter for social force calculation. In this class only 
 * pxX and pxY representing the current pixel position 
 * are pixel values.
 * 
 * Group behaviour is implemented as agents now get
 * close to their group members and will wait for them
 * before getting out.
 * 
 * @author		Yufeng Deng
 * @since 		17/8/2017
 *
 */
public class Person {
	
	///////////////////////////////////////////////////////////////////////////
	// Basic Properties
	
	public Color color = Color.WHITE;
	public Color rectColor = Color.WHITE;
	public double heading;
	
	public double pxX;
	public double pxY;
	
	public double x;
	public double y;
	
	public double speedX;
	public double speedY;
	
	public boolean injured;
	
	public double vi0;
	
	public boolean arrivedDest;
	
	public Group belongedGroup;
	
	public int clusterNumber;
	
	public int roomNo;
	
	public double destX;
	public double destY;
	
	public int groupArrived;
	
	public Screen destScreen;
	
	public double readingTime = Utils.uniform(3, 90);
	
	@ProbedProperty(displayName="PersonMuseumStatechart")
	PersonMuseumStatechart personStatechart = PersonMuseumStatechart.createStateChart(this, 0);
	
	public Museum main;
	private ContinuousSpace<Object> space;
	
	///////////////////////////////////////////////////////////////////////////
	// Social Force Model
	
	public double sumFiWH;
	public double sumFiWV;
	public double sumFijH;
	public double sumFijV;
	
	public boolean applyPsy = true;
	
	public double Ai = 2 * 100;
	public double Bi = 0.2;
	public double K = 1.2 * 100_000;
	public double k = 2.4 * 100_000;
	public double ri = Utils.uniform(0.15, 0.25);
	public double mi = 80;
	public double AiWall = 2 * 100;
	public double AiGrp = 5;

	private final static String READING_STATE = "reading";
	private final static String MOVING_STATE = "moving";
	private final static String RESTING_STATE = "resting";
	
	public final static double VI0_INIT = 1.4;
	public final static double VI0_READING = 0.25;
			
	private double ATTENTION_ANGLE = 5 * Math.PI / 6;
	private double CONNECTION_RANGE = 10;
	
	
	public Person(Museum main, ContinuousSpace<Object> space, double pre_ppl_psy, double pre_range, double pre_angle, double pre_wall_psy) {
		this.main = main;
		this.space = space;
		
		this.pxX = main.getStartPoint().getX();
		this.pxY = main.getStartPoint().getY();
		this.x = this.pxX / Museum.METER_2_PX;
		this.y = this.pxY / Museum.METER_2_PX;
		
		this.arrivedDest = false;
		this.heading = 0;
		
		this.vi0 = VI0_INIT; 
		this.Ai = 50 * Math.pow(2, pre_ppl_psy);
		this.Bi = 0.2;
		this.K = 1.2 * 100_000;
		this.k = 2.4 * 100_000;
		this.ri = Utils.uniform(0.15, 0.25);
		this.mi = 80;
		this.destX = 0;
		this.destY = 0;
		
		this.CONNECTION_RANGE = pre_range;
		this.ATTENTION_ANGLE = pre_angle;
		
		this.AiWall = 50 * Math.pow(2, pre_wall_psy);
		this.AiGrp = 5;
	}
	
	public boolean isMoving() {
		return this.personStatechart.withinState(MOVING_STATE);
	}
	
	public boolean isReading() {
		return this.personStatechart.withinState(READING_STATE);
	}
	
	public boolean isResting() {
		return this.personStatechart.withinState(RESTING_STATE);
	}
	
	public void sendResting() {
		groupArrived++;
	}
	
	public double calcvi0Horizontal() {
		if(destX==x || vi0==0){return 0;}
		return (destX-x)*vi0 / Utils.distance(x,y,destX,destY);
	}
	
	public double calcvi0Vertical() {
		if(destY==y || vi0==0){return 0;}
		return (destY-y)*vi0 / Utils.distance(x,y,destX,destY);
	}
	
	// TODO: cyclic event, first occurence at t = 0, then every 0.01 seconds
	@ScheduledMethod(start = 0, interval = Museum.UNIT_TIME)
	public void updateState() {
		if(!arrivedDest){
			if((destX-1<x && x<destX+1) && (destY-1<y && y<destY+1)){
				arrivedDest = true;
			}
		}
		calculatePpl();
		calculateWall();
		double totalForce = Math.sqrt(Utils.sqr(sumFijH)+Utils.sqr(sumFijV))+Math.sqrt(Utils.sqr(sumFiWH)+Utils.sqr(sumFiWV));
		if(totalForce/2/Math.PI/ri >= 16000){
			injured = true;
			this.color=Color.BLACK;
			vi0=0;
			speedX=0;
			speedY=0;
			return;
		}
		speedX += accelerationHorizontal() * Museum.UNIT_TIME;
		speedY += accelerationVertical() * Museum.UNIT_TIME;
		
		if(this.isReading()){
			heading = Math.atan2((destY-y),(destX-40/25-x)) + Math.PI/2;
		}else{
			heading = Math.atan2(speedY, speedX) + Math.PI/2;
		}
		x += (speedX * Museum.UNIT_TIME);
		y += (speedY * Museum.UNIT_TIME);
		pxX = x*Museum.METER_2_PX;
		pxY = y*Museum.METER_2_PX;

		updatePosition();
	}
	
	public void updatePosition() {
		Point p = Utils.anylogicToRePast(new Point(this.pxX, this.pxY));
		space.moveTo(this, p.x, p.y);
	}
	
	public void calculateWall() {
		sumFiWH = 0;
		sumFiWV = 0;
		for(Wall w : main.getWalls()){
			Point p = new Point();
			double dist = w.getNearestPoint(pxX,pxY,p);
			double diW = -1;
			if((diW = dist/Museum.METER_2_PX ) > CONNECTION_RANGE){continue;}
			
			double theta = Math.atan2(p.getY()-y, p.getX()-x)-Math.atan2(speedY,speedX);
			double cosTheta = 1;
			if(theta<(-ATTENTION_ANGLE/2) || theta>(ATTENTION_ANGLE/2)){
				cosTheta = 0;
			}
			
			double niW1,niW2,tiW1,tiW2;
			double gx;
			double deltavH, deltavV, deltav;
			double fpsy,fbody,friction;
			double fiWH,fiWV;
			niW1 = (x==(p.getX()/Museum.METER_2_PX) ? 0:(x-(p.getX()/Museum.METER_2_PX))/diW);
			niW2 = (y==(p.getY()/Museum.METER_2_PX) ? 0:(y-(p.getY()/Museum.METER_2_PX))/diW);
			tiW1 = -niW2;
			tiW2 = niW1;
			gx = (diW>ri ? 0:(ri-diW));
			fpsy = AiWall*Math.exp((ri-diW)/Bi)*cosTheta;
			fbody = K*gx;
			deltavH = -speedX;
			deltavV = -speedY;
			deltav = deltavH*tiW1+deltavV*tiW2;
			friction = k*gx*deltav;
			fiWH = (fpsy+fbody)*niW1+friction*tiW1;
			fiWV = (fpsy+fbody)*niW2+friction*tiW2;
			sumFiWH += fiWH;
			sumFiWV += fiWV;
		}
	}
	
	public void calculatePpl() {
		sumFijH = 0;
		sumFijV = 0;
		for(Person j : main.getPeople()){
			if(this==j){continue;}
			double dij = -1;
			if((dij = Utils.distance(x,y,j.x,j.y)) > CONNECTION_RANGE){continue;}
			double I = 1;
			if(belongedGroup != null){
			if(belongedGroup.people.contains(j)){
				I = -1;
			}
			}
			double theta = Math.atan2(j.y-y, j.x-x)-Math.atan2(speedY,speedX);
			double cosTheta = 1;
			if(theta<(-ATTENTION_ANGLE/2) || theta>(ATTENTION_ANGLE/2)){
				if(main.isEnableVisionArea()){
					cosTheta = 0;
				}
			}
			
			double nij1,nij2,tij1,tij2;
			double rij,gx;
			double deltavH, deltavV, deltav;
			double fpsy,fbody,friction;
			double fijH,fijV;
			rij = ri+j.ri;
			nij1 = (x==j.x ? 0:(x-j.x)/dij);
			nij2 = (y==j.y ? 0:(y-j.y)/dij);
			tij1 = -nij2;
			tij2 = nij1;
			gx = (dij>rij ? 0:(rij-dij));
			fpsy = Ai*Math.exp((rij-dij)/Bi)*cosTheta;
			if(I==-1){
				if(applyPsy){
					fpsy = -AiGrp*Math.exp((dij-rij)/2)*cosTheta;
				}else{
					fpsy = 0;
				}
			}
			fbody = K*gx;

			deltavH = j.speedX-speedX;
			deltavV = j.speedY-speedY;
			deltav = deltavH*tij1+deltavV*tij2;
			friction = k*gx*deltav;
			fijH = (fpsy+fbody)*nij1+friction*tij1;
			fijV = (fpsy+fbody)*nij2+friction*tij2;
			sumFijH += fijH;
			sumFijV += fijV;
		}
	}
	
	public double accelerationVertical() {
		return ((calcvi0Vertical()-speedY)*mi/0.5 + sumFijV + sumFiWV)/mi;
	}
	
	public double accelerationHorizontal() {
		return ((calcvi0Horizontal()-speedX)*mi/0.5 + sumFijH + sumFiWH)/mi;
	}

	public void getNearestScreen() {
		double deltaY = Double.POSITIVE_INFINITY;
		for(Screen s : main.getRooms().get(roomNo).screens){
			if(Math.abs(s.alliY + s.y - pxY) < deltaY){
				deltaY = Math.abs(s.alliY + s.y - pxY);
				destScreen = s;
			}
		}
	}
	
	public boolean shouldWaiting() {
		if(belongedGroup==null){
			return false;
		}
		for(Person p: belongedGroup.people){
			if (p.isReading()){
				return true;
			}
		}
		return false;
	}
	
	public boolean withGroup() {
		for(Person p: belongedGroup.people){
			if(Utils.distance(x,y,p.x,p.y) > 2){
				return false;
			}
		}
		if(Utils.distance(x,y,destX,destY) > 3){
			return false;
		}
		return true;
	}
}
