package socialForce.scenario.museum;

import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.space.continuous.ContinuousSpace;
import socialForce.Utils;

public class Screen {
	
	///////////////////////////////////////////////////////////////////////////
	// Basic Properties
		
	public double x;
	public double y;
	
	public boolean isXaxis;
	
	public double min;
	public double max;
	
	public double ri;
	
	public double alliX;
	public double alliY;
	
	public int roomNo;
	public double rotation;
	
	public double speed = 0;
	
	public final static double RI_INIT = 20;
	
	private Museum main;
	private ContinuousSpace<Object> space;
	
	public Screen(Museum main, ContinuousSpace<Object> space) {
		this.main = main;
		this.space = space;
	}
	
	public int calcReadingNum() {
		int sum = 0;
		for(Person p : main.getPeople()){
			if(p.isReading() && p.destScreen == this){
				sum++;
			}
		}
		return sum;
	}
	
	///////////////////////////////////////////////////////////////////////////
	// Events
	
	// TODO: cyclic event, first occurence at t = 0, then every UNIT_TIME seconds
	@ScheduledMethod(start = 0, interval = Museum.UNIT_TIME)
	public void action() {
		socialForce();
		double acceV = ((0.2-speed)/0.1 + sumFijV);
		double acceH = (-(sumFijH)-15)/10;
		double tSPEED = speed + acceV*Museum.UNIT_TIME;
		if(tSPEED>-0.2 && tSPEED <0.2){
			speed = tSPEED;
		}
		double ty = y + (speed*Museum.UNIT_TIME)* Museum.METER_2_PX;
		if(ty>min && ty<max){
			y=ty;
		}
		double tri = ri + acceH*Museum.UNIT_TIME;
		if(tri>=20 && tri<=40){
			ri = tri;
		}
		if(y+ri > max){
			y=max-ri;
		}else if(y-ri<min){
			y=min+ri;
		}
		
		// NOTE: update position in space, otherwise won't get re-rendered
		// NOTE: doing rendering in GLOBAL coordinates => set to 0/0 here
		space.moveTo(this, 0, 0); 
	}
		
	///////////////////////////////////////////////////////////////////////////
	// Social Force

	public double left2nearest;
	public double right2nearest;
	
	public double sumFijV;
	public double sumFijH;
	
	public final static double Ai = 5;
	public final static double Bi = 5;
	public final static double AiS = 200;
	public final static double BiS = 0.1;
	public final static double K = 1.2*10;
	
	public void socialForce() {
		sumFijH = 0;
		sumFijV = 0;
		for(Person j : main.getPeople()){
			if(!j.isReading() && !j.isMoving()){continue;}
			double ax = (x+alliX)/Museum.METER_2_PX;
			double aymin = (y+alliY-ri)/Museum.METER_2_PX;
			double aymax = (y+alliY+ri)/Museum.METER_2_PX;
			double ay = (y+alliY)/Museum.METER_2_PX;
			double dij = -1;
			if(j.y>aymin && j.y<aymax){
				dij = j.x - ax;
			}else{
				dij = Utils.distance(ax,ay,j.x,j.y);
			}
			double nij1,nij2;
			double rij,gx;
			double fpsy;
			double fijH,fijV;
			rij = j.ri;
			nij1 = (ax==j.x ? 0:(ax-j.x)/dij);
			if(j.y>aymin && j.y<aymax){
				nij2 = 0;
			}else{
				nij2 = (ay-j.y)/dij;
			}
			fpsy = Ai*Math.exp((rij-dij)/Bi);

			fijH = (fpsy)*nij1;
			fijV = (fpsy)*nij2;
			if(j.isReading()){
				sumFijH += fijH;
			}
			sumFijV += fijV;
		}
		for(Screen j : main.getRooms().get(roomNo).screens){
			if(j==this){continue;}
			double dij = (y-j.y)/ Museum.METER_2_PX;
			double vict = (dij>0?1:-1);
			dij = Math.abs(dij);
			double rij = (ri+j.ri)/Museum.METER_2_PX;
			double fpsy = AiS*Math.exp((rij-dij)/BiS);
			double gx = (dij>rij ? 0:(rij-dij));
			double fbody = K*gx;
			double fij = (fpsy+fbody)*vict;
			sumFijV += fij;
		}
	}
/*
	///////////////////////////////////////////////////////////////////////////
	// HAMMER implemenntation

	public int currNumOfClusters;
	public int futureNumOfClusters;
	
	public boolean moveLeftFlag;
	public boolean moveRightFlag;
	
	public double leftCFvalue;
	public double rightCFvalue;
	public double di;
	
	// TODO: cyclic event with first occurence after 5 seconds, then schedule every 0.5 seconds (model time)
	@ScheduledMethod(start = 5, interval = 0.5)
	public void monitoring() {
		if(inState(hold)){
			di = (Utils.uniform()>0.5? -1:1);
		}
		calcClusters();
	}
	
	public void calcClusters() {
		// TODO: why do we need synchronisation here?
		synchronized(this){ 
			get_Main().people_t.clear();
			for(Person p:get_Main().people){
				if(!p.isReading() && !p.isMoving()){
					continue;
				}
				get_Main().people_t.add(p);
			}
			get_Main().clustering();
			currNumOfClusters = get_Main().finalClusterNum;
			
			//----------forward model---------------
			get_Main().people_t.clear();
			for(Person p:get_Main().people){
				Person pt = new Person();
				pt.pxX = p.pxX;
				pt.pxY = p.pxY;
				pt.clusterNumber = p.clusterNumber;
				if(!p.isReading() && !p.isMoving()){
					continue;
				}
				if(p.destScreen == this){
					pt.pxY += di*speed*10*SocialForce.UNIT_TIME;
				}
				get_Main().people_t.add(pt);
			}
			get_Main().clustering();
			futureNumOfClusters = get_Main().finalClusterNum;
		}
		if(futureNumOfClusters > currNumOfClusters){
			if(di == -1){
				moveLeftFlag = true;
				leftCFvalue = 0;
			}else{
				moveRightFlag = true;
				rightCFvalue = 0;
			}
		}
		//System.out.println(get_Main().rooms.get(0).screens.size());
		//System.out.println(currNumOfClusters + " " + futureNumOfClusters + " " + di + " " +leftCFvalue + " " + rightCFvalue + " " + (futureNumOfClusters > currNumOfClusters && di == -1));
		if(currNumOfClusters > futureNumOfClusters){
			if(inState(moveLeft)){
				leftCFvalue --;
			}else if(inState(moveRight)){
				rightCFvalue --;
			}
		}else if(currNumOfClusters < futureNumOfClusters){
			if(inState(moveLeft)){
				leftCFvalue ++;
			}else if(inState(moveRight)){
				rightCFvalue ++;
			}
		}else{
			if(inState(moveLeft)){
				leftCFvalue -=0.1;
			}else if(inState(moveRight)){
				rightCFvalue -=0.1;
			}
		}
	}
	*/
}
