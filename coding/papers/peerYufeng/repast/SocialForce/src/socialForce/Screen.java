package socialForce;

public class Screen {
	
	///////////////////////////////////////////////////////////////////////////
	// Basic Properties
		
	private double x;
	private double y;
	
	private boolean isXaxis;
	
	private double min;
	private double max;
	
	private double ri;
	
	private double alliX;
	private double alliY;
	
	private int roomNo;
	private double rotation;
	
	private final static double SPEED = 0;
	
	private final static double RI_INIT = 20;
	
	private void calcReadingNum() {
		int sum = 0;
		for(Person p:get_Main().people){
			if(p.inState(p.reading) && p.destScreen == this){
				sum++;
			}
		}
		return sum;
	}
	
	///////////////////////////////////////////////////////////////////////////
	// HAMMER implemenntation
	
	private int currNumOfClusters;
	private int futureNumOfClusters;
	
	private boolean moveLeftFlag;
	private boolean moveRightFlag;
	
	private double leftCFvalue;
	private double rightCFvalue;
	private double di;
	
	private double left2nearest;
	private double right2nearest;
	
	private double sumFijV;
	private double sumFijH;
	
	private final static double Ai = 5;
	private final static double Bi = 5;
	private final static double AiS = 200;
	private final static double BiS = 0.1;
	private final static double K = 1.2*10;
	
	// TODO: cyclic event, first occurence at t = 0, then every UNIT_TIME seconds
	@ScheduledMethod(start = 0, interval = SocialForce.UNIT_TIME)
	private void action() {
		socialForce();
		double acceV = ((0.2-SPEED)/0.1 + sumFijV);
		double acceH = (-(sumFijH)-15)/10;
		double tSPEED = SPEED + acceV*SocialForce.UNIT_TIME;
		if(tSPEED>-0.2 && tSPEED <0.2){
			SPEED = tSPEED;
		}
		double ty = y + (SPEED*SocialForce.UNIT_TIME)*get_Main().meter2px;
		if(ty>min && ty<max){
			y=ty;
		}
		double tri = ri + acceH*SocialForce.UNIT_TIME;
		if(tri>=20 && tri<=40){
			ri = tri;
		}
		if(y+ri > max){
			y=max-ri;
		}else if(y-ri<min){
			y=min+ri;
		}
	}
	
	private void socialForce() {
		sumFijH = 0;
		sumFijV = 0;
		for(Person j : get_Main().people){
			if(!j.inState(j.reading) && !j.inState(j.moving)){continue;}
			double ax = (x+alliX)/SocialForce.METER_2_PX;
			double aymin = (y+alliY-ri)/SocialForce.METER_2_PX;
			double aymax = (y+alliY+ri)/SocialForce.METER_2_PX;
			double ay = (y+alliY)/SocialForce.METER_2_PX;
			double dij = -1;
			if(j.y>aymin && j.y<aymax){
				dij = j.x - ax;
			}else{
				dij = distance(ax,ay,j.x,j.y);
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
			if(j.inState(j.reading)){
				sumFijH += fijH;
			}
			sumFijV += fijV;
		}
		for(Screen j : get_Main().rooms.get(roomNo).screens){
			if(j==this){continue;}
			double dij = (y-j.y)/get_Main().meter2px;
			double vict = (dij>0?1:-1);
			dij = Math.abs(dij);
			double rij = (ri+j.ri)/get_Main().meter2px;
			double fpsy = AiS*Math.exp((rij-dij)/BiS);
			double gx = (dij>rij ? 0:(rij-dij));
			double fbody = K*gx;
			double fij = (fpsy+fbody)*vict;
			sumFijV += fij;
		}
	}
	
	// TODO: cyclic event with first occurence after 5 seconds, then schedule every 0.5 seconds (model time)
	@ScheduledMethod(start = 5, interval = 0.5)
	private void monitoring() {
		if(inState(hold)){
			di = (Utils.uniform()>0.5? -1:1);
		}
		calcClusters();
	}
	
	private void calcClusters() {
		// TODO: why do we need synchronisation here?
		synchronized(this){ 
			get_Main().people_t.clear();
			for(Person p:get_Main().people){
				if(!p.inState(p.reading) && !p.inState(p.moving)){
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
				pt.cluster_number = p.cluster_number;
				if(!p.inState(p.reading) && !p.inState(p.moving)){
					continue;
				}
				if(p.destScreen == this){
					pt.pxY += di*SPEED*10*SocialForce.UNIT_TIME;
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
}
