package socialForce.movable;

import java.awt.Color;
import java.util.List;

import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.ui.probe.ProbedProperty;
import socialForce.chart.hall.PersonHallStatechart;
import socialForce.markup.Markup;
import socialForce.markup.impl.Point;
import socialForce.misc.Group;
import socialForce.misc.Utils;
import socialForce.scenario.hall.Hall;

public class Person implements Moveable {

	private double x;
	private double y;

	private double destX;
	private double destY;
	
	private boolean top;
	
	private Point entry;
	
	private double heading;
	
	private double speedX;
	private double speedY;
	
	private double attentionAngle;
	private double connectionRange;
	
	private Color color;
	
	private Group belongedGroup;
	
	private double readingTime;
	
	private boolean injured;
	
	private boolean left;
	private boolean arrivedDest;
	
	private double AiWall;
	private double AiGrp;
	private double Bi;
	private double Ai;
	private double K;
	private double k;
	private double ri;
	private double vi0;
	private double mi;
	
	private boolean applyPsy;
	
	private double sumFiWH;
	private double sumFiWV;
	private double sumFijH;
	private double sumFijV;
	
	private Hall main;
	private ContinuousSpace<Object> space;
	
	public final static double VI0_INIT = 1.4;
	
	@ProbedProperty(displayName="PersonHallStatechart")
	PersonHallStatechart state = PersonHallStatechart.createStateChart(this, 0);
	
	// NOTE: start is in pixel-space
	public Person(Hall main, ContinuousSpace<Object> space, boolean top, Point start, Point entry) {
		this.main = main;
		this.space = space;
		
		this.top = top;
		
		this.entry = entry;
		
		this.x = start.getX();
		this.y = start.getY();
		
		this.heading = 0;
		
		this.attentionAngle = 5*Math.PI/6;
		this.connectionRange = 10;
		
		this.speedX = 0;
		this.speedY = 0;
		
		this.injured = false;
		this.arrivedDest = false;
		
		this.color = Color.WHITE;
		
		this.readingTime = Utils.uniform(10,60);
		
		this.applyPsy = true;
		
		this.AiWall = 2*100;
		this.AiGrp = 5;
		this.Bi = 0.2;
		this.Ai = 2*100;
		this.K = 1.2*100_000;
		this.k = 2.4*100_000;
		this.ri = Utils.uniform(0.15,0.25);
		this.vi0 = VI0_INIT;
		this.mi = 80;
		
		this.left = false;
	}
	
	public void leave() {
		this.left = true;
		this.main.removePerson(this);
	}
	
	public boolean hasLeft() {
		return this.left;
	}

	public Hall getMain() {
		return this.main;
	}
	
	public double getX() {
		return this.x;
	}
	
	public boolean isTop() {
		return this.top;
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
	
	public Color getColor() {
		return this.color;
	}
	
	public void setGroup(Group g) {
		this.belongedGroup = g;
	}
	
	public void setReadingTime(double d) {
		this.readingTime = d;
	}
	
	public boolean isInGroup() {
		return this.belongedGroup != null;
	}
	
	public Group getGroup() {
		return this.belongedGroup;
	}
	
	public boolean isAtDest() {
		return this.arrivedDest;
	}
	
	public void resetAtDest() {
		this.arrivedDest = false;
	}
	
	public void setDest(double dx, double dy) {
		this.destX = dx;
		this.destY = dy;
	}
	
	public double getReadingTime() {
		return this.readingTime;
	}
	
	public void flipTop() {
		this.top = !this.top;
	}
	
	public void updatePosition() {
		space.moveTo(this, this.x, this.y);
	}
	
	public void destToEntry() {
		this.destX = this.entry.getX();
		this.destY = this.entry.getY();
	}
	
	public void resetVi0() {
		this.vi0 = VI0_INIT;
	}
	
	public double getDestY() {
		return this.destY;
	}
	
	public double getDestX() {
		return this.destX;
	}
	
	public double getHeading() {
		return this.heading;
	}
	
	@ScheduledMethod(start = 0, interval = Hall.UNIT_TIME)
	public void move() {
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
		speedX += accelerationHorizontal() * Hall.UNIT_TIME;
		speedY += accelerationVertical() * Hall.UNIT_TIME;

		heading = Math.atan2(speedY, speedX) + Math.PI/2;
		x += (speedX * Hall.UNIT_TIME);
		y += (speedY * Hall.UNIT_TIME);
		
		this.updatePosition();
	}

	private double accelerationVertical() {
		return ((calcvi0Vertical()-speedY)*mi/0.5 + sumFijV + sumFiWV)/mi;
	}

	private double calcvi0Vertical() {
		if(destY==y || vi0==0){return 0;}
		return (destY-y)*vi0 / Point.distance(x,y,destX,destY);
	}

	private double accelerationHorizontal() {
		return ((calcvi0Horizontal()-speedX)*mi/0.5 + sumFijH + sumFiWH)/mi;
	}

	private double calcvi0Horizontal() {
		if(destX==x || vi0==0){return 0;}
		return (destX-x)*vi0 / Point.distance(x,y,destX,destY);
	}

	private void calculateWall() {
		sumFiWH = 0;
		sumFiWV = 0;
		for(Markup m : main.getMarkups()) {
			Point p = new Point();
			double dist = m.getNearestPoint(x,y,p);
			double diW = -1;
			if((diW = dist) > connectionRange){continue;}
			
			double theta = Math.atan2(p.getY()-y, p.getX()-x)-Math.atan2(speedY,speedX);
			double cosTheta = 1;
			if(theta<(-attentionAngle/2) || theta>(attentionAngle/2)){
				cosTheta = 0;
			}
			
			double niW1,niW2,tiW1,tiW2;
			double gx;
			double deltavH, deltavV, deltav;
			double fpsy,fbody,friction;
			double fiWH,fiWV;
			niW1 = (x==p.getX() ? 0:(x-p.getX())/diW);
			niW2 = (y==p.getY() ? 0:(y-p.getY())/diW);
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

		AdaptiveWall w = main.getAdaptiveWall();
		List<Double> adaptWalls = w.getWalls();
		List<Double> wallWidths = w.getWallWidths();
		for(int i = 0; i < adaptWalls.size(); i++){
			Point p;
			double xmin = w.getX()-w.getTotalWidth()/2+adaptWalls.get(i);
			double xmax = xmin + wallWidths.get(i);
			//System.out.println(i + " " + xmin + " " + xmax);
			if(x > xmax){
				p = new Point(xmax, w.getY());
			}else if(x<xmin){
				p = new Point(xmin,w.getY());
			}else{
				p = new Point(x,w.getY());
			}
			//System.out.println(i + " " + p.getX() + " " + p.getY());
			
			double sqrdist = (Point.distance(x,y,p.getX(),p.getY()));
			double diW = -1;
			if((diW = sqrdist) > connectionRange){return;}
			
			double theta = Math.atan2(p.getY()-y, p.getX()-x)-Math.atan2(speedY,speedX);
			double cosTheta = 1;
			if(theta<(-attentionAngle/2) || theta>(attentionAngle/2)){
				if(Hall.ENABLE_VISION_AREA){
					cosTheta = 0;
				}
			}
			
			double niW1,niW2,tiW1,tiW2;
			double gx;
			double deltavH, deltavV, deltav;
			double fpsy,fbody,friction;
			double fiWH,fiWV;
			niW1 = (x==(p.getX()) ? 0:(x-(p.getX()))/diW);
			niW2 = (y==(p.getY()) ? 0:(y-(p.getY()))/diW);
			tiW1 = -niW2;
			tiW2 = niW1;
			gx = (diW>ri ? 0:(ri-diW));
			fpsy = Ai*Math.exp((ri-diW)/Bi)*cosTheta;
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
		for(Person j : main.getPeople()) {
			if(this==j){continue;}
			double dij = -1;
			if((dij = Point.distance(x,y,j.x,j.y)) > connectionRange){continue;}
			double I = 1;
			if(belongedGroup != null){
				if(belongedGroup.isMember(j)){
					I = -1;
				}
			}
			double theta = Math.atan2(j.y-y, j.x-x)-Math.atan2(speedY,speedX);
			double cosTheta = 1;
			if(theta<(-attentionAngle/2) || theta>(attentionAngle/2)){
				if(Hall.ENABLE_VISION_AREA){
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
}
