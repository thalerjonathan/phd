package socialForce;

import java.util.List;

public class AdaptiveWall {
	
	private double x = 300;
	private double y = 250;
	
	private double min = 250;
	private double max = 400;
	
	private double totalWidth = 300;
	
	private double door0x = 75;
	private double door1x = 425;
	
	private double sumFijV;
	private double sumFijH;
	
	private List<Double> doorsX; // TODO: add door0x and door1x
	
	private final static double SPEED = 0;
	
	private final static double Ai = 5;
	private final static double Bi = 5;
	private final static double AiS = 200;
	private final static double BiS = 0.1;
	private final static double K = 1.2*10;
	
	// TODO: cyclic event, first occurence t=0, then every UNIT_TIME seconds
	private void action() {
		socialForce();
		double acceH = (sumFijH);
		double tspeed = SPEED + acceH*SocialForce.UNIT_TIME;
		//System.out.println(acceV);
		if(tspeed>-0.1 && tspeed <0.1){
			speed = tspeed;
		}
		double tx = x + (SPEED*SocialForce.UNIT_TIME)*SocialForce.METER_2_PX;
		if(tx>min && tx<max){
			x=tx;
		}

		/*
		double acceV = (sumFijV);
		double tspeed = speed + acceV*unitTime;
		//System.out.println(acceV);
		if(tspeed>-0.1 && tspeed <0.1){
			speed = tspeed;
		}
		double ty = y + (speed*unitTime)*get_Main().meter2px;
		if(ty>ymin && ty<ymax){
			y=ty;
		}
		*/
	}
	
	private void socialForce() {
		sumFijH = 0;
		for(Person j : get_Main().people){
			//if(!j.inState(j.reading) && !j.inState(j.moving)){continue;}
			double ax = x/SocialForce.METER_2_PX;
			double ay = y/SocialForce.METER_2_PX;
			double dij = -1;
			dij = Math.abs(ax-j.x);
			double nij1,nij2;
			double rij,gx;
			double fpsy;
			double fijH,fijV;
			rij = j.ri;

			nij1 = (ax-j.x)/dij;
			fpsy = Ai*exp((rij-dij)/Bi);

			fijH = (fpsy)*nij1;
			sumFijH += fijH;
		}
		/*sumFijV = 0;
		for(Person j : get_Main().people){
			//if(!j.inState(j.reading) && !j.inState(j.moving)){continue;}
			double ax = x/get_Main().meter2px;
			double ay = y/get_Main().meter2px;
			double dij = -1;
			dij = Math.abs(ay-j.y);
			double nij1,nij2;
			double rij,gx;
			double fpsy;
			double fijH,fijV;
			rij = j.ri;

			nij2 = (ay-j.y)/dij;
			fpsy = Ai*exp((rij-dij)/Bi);

			fijV = (fpsy)*nij2;
			sumFijV += fijV;
		}
		*/
	}
	
	private double getNearestPoint(double x1, double y1, Point p) {
		return Utils.sqr(x1 - this.x);
	}
}
