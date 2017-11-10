package socialForce;

import java.util.List;

import com.sun.prism.paint.Color;

public class Person {
	
	///////////////////////////////////////////////////////////////////////////
	// Basic Properties
	
	private Color color = Color.WHITE;
	private Color rectColor = Color.WHITE;
	private double heading = INIT_HEADING;
	
	private double pxX = INIT_X;
	private double pxY = INIT_Y;
	
	private double x = INIT_X / SocialForce.METER_2_PX;
	private double y = INIT_Y / SocialForce.METER_2_PX;
	
	private double speedX;
	private double speedY;
	
	private boolean injured;
	
	private double vi0 = VI0_INIT;
	
	private boolean arrivedDest;
	
	private Group belongedGroup;
	
	private int clusterNumber;
	
	private int roomNo;
	
	private double destX;
	private double destY;
	
	private final static double INIT_HEADING = 0;
	private final static double INIT_X = 0;
	private final static double INIT_Y = 0;
	private final static double VI0_INIT = 1.4;
	private final static double VI0_READING = 0.25;
			
	private final static double ATTENTION_ANGLE = 5 * Math.PI / 6;
	private final static double CONNECTION_RANGE = 10;
	
	private double calcvi0Horizontal() {
		if(destX==x || vi0==0){return 0;}
		return (destX-x)*vi0 / Utils.distance(x,y,destX,destY);
	}
	
	private double calcvi0Vertical() {
		if(destY==y || vi0==0){return 0;}
		return (destY-y)*vi0 / Utils.distance(x,y,destX,destY);
	}
	
	///////////////////////////////////////////////////////////////////////////
	// Social Force Model
	
	private double sumFiWH;
	private double sumFiWV;
	private double sumFijH;
	private double sumFijV;
	
	private boolean applyPsy = true;
	
	private final static double Ai = 2 * 100;
	private final static double Bi = 0.2;
	private final static double K = 1.2 * 100000;
	private final static double k = 2.4 * 100000;
	private final static double ri = 0; // TODO: uniform(0.15,0.25)
	private final static double mi = 80;
	private final static double AiWall = 2 * 100;
	private final static double AiGrp = 5;
	
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
		double totalForce = Math.sqrt(Utils.sqr(sumFijH)+sqr(sumFijV))+Math.sqrt(Utils.sqr(sumFiWH)+Utils.sqr(sumFiWV));
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
		if(this.inState(reading)){
			heading = Math.atan2((destY-y),(destX-40/25-x)) + Math.PI/2;
		}else{
			heading = Math.atan2(speedY, speedX) + Math.PI/2;
		}
		x += (speedX * SocialForce.UNIT_TIME);
		y += (speedY * SocialForce.UNIT_TIME);
		pxX = x*SocialForce.METER_2_PX;
		pxY = y*SocialForce.METER_2_PX;
	}
	
	private void calculateWall() {
		sumFiWH = 0;
		sumFiWV = 0;
		for(Wall w : get_Main().walls){
			Point p = new Point();
			double sqrdist = w.getNearestPoint(pxX,pxY,p);
			double diW = -1;
			if((diW = Math.sqrt(sqrdist)/METER_2_PX) > connectionRange){continue;}
			
			double theta = atan2(p.getY()-y, p.getX()-x)-atan2(speedY,speedX);
			double cosTheta = 1;
			if(theta<(-attentionAngle/2) || theta>(attentionAngle/2)){
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
	
	private void calculatePpl() {
		sumFijH = 0;
		sumFijV = 0;
		for(Person j : get_Main().people){
			if(this==j){continue;}
			double dij = -1;
			if((dij = Utils.distance(x,y,j.x,j.y)) > connectionRange){continue;}
			double I = 1;
			if(belongedGroup != null){
			if(belongedGroup.contains(j)){
				I = -1;
			}
			}
			double theta = Math.atan2(j.y-y, j.x-x)-Math.atan2(speedY,speedX);
			double cosTheta = 1;
			if(theta<(-attentionAngle/2) || theta>(attentionAngle/2)){
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
	
	private double accelerationVertical() {
		return ((calcvi0Vertical()-speedY)*mi/0.5 + sumFijV + sumFiWV)/mi;
	}
	
	private double accelerationHorizontal() {
		return ((calcvi0Horizontal()-speedX)*mi/0.5 + sumFijH + sumFiWH)/mi;
	}

}
