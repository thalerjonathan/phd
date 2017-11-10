package socialForce;

import java.awt.Color;

import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.ui.probe.ProbedProperty;
import socialForce.chart.PersonStatechart;

public class Person {
	
	///////////////////////////////////////////////////////////////////////////
	// Basic Properties
	
	public Color color = Color.WHITE;
	public Color rectColor = Color.WHITE;
	public double heading = INIT_HEADING;
	
	public double pxX = INIT_X;
	public double pxY = INIT_Y;
	
	public double x = INIT_X / SocialForce.METER_2_PX;
	public double y = INIT_Y / SocialForce.METER_2_PX;
	
	public double speedX;
	public double speedY;
	
	public boolean injured;
	
	public double vi0 = VI0_INIT;
	
	public boolean arrivedDest;
	
	public Group belongedGroup;
	
	public int clusterNumber;
	
	public int roomNo;
	
	public double destX;
	public double destY;
	
	public double groupArrived;
	
	public Screen destScreen;
	
	public double readingTime = Utils.uniform(3, 90);
	
	@ProbedProperty(displayName="PersonStatechart")
	PersonStatechart personStatechart = PersonStatechart.createStateChart(this, 0);
	
	private final static String READING_STATE = "reading";
	private final static String MOVING_STATE = "moving";
	
	public boolean isMoving() {
		return this.personStatechart.withinState(MOVING_STATE);
	}
	
	public boolean isReading() {
		return this.personStatechart.withinState(READING_STATE);
	}
	
	public final static double INIT_HEADING = 0;
	public final static double INIT_X = 0;
	public final static double INIT_Y = 0;
	public final static double VI0_INIT = 1.4;
	public final static double VI0_READING = 0.25;
			
	public final static double ATTENTION_ANGLE = 5 * Math.PI / 6;
	public final static double CONNECTION_RANGE = 10;
	
	public double calcvi0Horizontal() {
		if(destX==x || vi0==0){return 0;}
		return (destX-x)*vi0 / Utils.distance(x,y,destX,destY);
	}
	
	public double calcvi0Vertical() {
		if(destY==y || vi0==0){return 0;}
		return (destY-y)*vi0 / Utils.distance(x,y,destX,destY);
	}
	
	///////////////////////////////////////////////////////////////////////////
	// Social Force Model
	
	public double sumFiWH;
	public double sumFiWV;
	public double sumFijH;
	public double sumFijV;
	
	public boolean applyPsy = true;
	
	public final static double Ai = 2 * 100;
	public final static double Bi = 0.2;
	public final static double K = 1.2 * 100000;
	public final static double k = 2.4 * 100000;
	public final static double ri = 0; // TODO: uniform(0.15,0.25)
	public final static double mi = 80;
	public final static double AiWall = 2 * 100;
	public final static double AiGrp = 5;
	
	// TODO: cyclic event, first occurence at t = 0, then every 0.01 seconds
	@ScheduledMethod(start = 0, interval = SocialForce.UNIT_TIME)
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
		speedX += accelerationHorizontal() * SocialForce.UNIT_TIME;
		speedY += accelerationVertical() * SocialForce.UNIT_TIME;
		
		if(this.isReading()){
			heading = Math.atan2((destY-y),(destX-40/25-x)) + Math.PI/2;
		}else{
			heading = Math.atan2(speedY, speedX) + Math.PI/2;
		}
		x += (speedX * SocialForce.UNIT_TIME);
		y += (speedY * SocialForce.UNIT_TIME);
		pxX = x*SocialForce.METER_2_PX;
		pxY = y*SocialForce.METER_2_PX;
	}
	
	public void calculateWall() {
		sumFiWH = 0;
		sumFiWV = 0;
		for(Wall w : get_Main().walls){
			Point p = new Point();
			double sqrdist = w.getNearestPoint(pxX,pxY,p);
			double diW = -1;
			if((diW = Math.sqrt(sqrdist)/SocialForce.METER_2_PX ) > CONNECTION_RANGE){continue;}
			
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
			niW1 = (x==(p.getX()/SocialForce.METER_2_PX) ? 0:(x-(p.getX()/SocialForce.METER_2_PX))/diW);
			niW2 = (y==(p.getY()/SocialForce.METER_2_PX) ? 0:(y-(p.getY()/SocialForce.METER_2_PX))/diW);
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
		for(Person j : get_Main().people){
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
				if(get_Main().enableVisionArea){
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

	public Screen getNearestScreen() {
		double deltaY = Double.POSITIVE_INFINITY;
		for(Screen s : get_Main().rooms.get(roomNo).screens){
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
