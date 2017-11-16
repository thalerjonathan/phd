package socialForce.scenario.pillarHall;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import repast.simphony.context.Context;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.util.ContextUtils;
import socialForce.Utils;
import socialForce.geom.Point;
import socialForce.geom.Rect;
import socialForce.markup.Wall;

public class PillarHall {
	private ContinuousSpace<Object> space;
	
	private List<Person> people;
	private AdaptiveWall adaptiveWall;
	private List<Group> groups;

	private List<Wall> walls;
	
	private List<Point> bottomStartPoints;
	private List<Point> topStartPoints;
	private Point topEntrance;
	private Point bottomEntrance;
	private Point topExit;
	private Point bottomExit;;
	
	private Rect movingArea;
	
	private final static double GROUP_SPAWNING = 0.3;
	private final static double SPAWNING_SPEED_TOP = 5.0;
	private final static double SPAWNING_SPEED_BOTTOM = 5.0;
	
	//public final static double METER_2_PX = 25.0;
	public final static double UNIT_TIME = 0.01;
	
	public final static double EXIT_RATE = 0.5;
	public final static boolean ENABLE_VISION_AREA = false;
	
	public PillarHall(ContinuousSpace<Object> space) {
		this.space = space;
	
		this.groups = new ArrayList<Group>();
		this.people = new ArrayList<Person>();
		
		this.initPoints();
	}
	
	public void initAgents(Context<Object> context) {
		initMarkups(context);
		initAdaptiveWall(context);
	}
	
	public List<Person> getPeople() {
		return Collections.unmodifiableList(this.people);
	}
	
	public List<Wall> getWalls() {
		return Collections.unmodifiableList(this.walls);
	}
	
	public Rect getMovingArea() {
		return this.movingArea;
	}
	
	public AdaptiveWall getAdaptiveWall() {
		return this.adaptiveWall;
	}
	
	@ScheduledMethod(start = 0, interval = SPAWNING_SPEED_TOP)
	public void spawnEntryTop() {
		spawn(true);
	}

	@ScheduledMethod(start = 0, interval = SPAWNING_SPEED_BOTTOM)
	public void spawnEntryBottom() {
		spawn(false);
	}
	
	private void initMarkups(Context<Object> context) {
		this.movingArea = new Rect(new Point(2.8, 5.2), 10.4, 11.6);
		
		initWalls(context);
	}
	
	private void initWalls(Context<Object> context) {
		this.walls = new ArrayList<Wall>();
		this.walls.add(new Wall(new Point(2, 4), new Point(0, 14)));
		this.walls.add(new Wall(new Point(2, 18), new Point(0.8, 0)));
		this.walls.add(new Wall(new Point(4.8, 18), new Point(6.4, 0)));
		this.walls.add(new Wall(new Point(14, 18), new Point(-0.8, 0)));
		this.walls.add(new Wall(new Point(14, 18), new Point(0, -14)));
		this.walls.add(new Wall(new Point(14, 4), new Point(0, -0.8)));
		this.walls.add(new Wall(new Point(4.8, 4), new Point(6.4, 0)));
		
		// Square Pillar
		this.walls.add(new Wall(new Point(7.2, 7.2), new Point(0, 1.6), new Point(1.6, 1.6), new Point(1.6, 0), new Point(0, 0)));
		
		for (Wall w : this.walls) {
			context.add(w);
			space.moveTo(w, 0, 0); // add at 0/0, the rendering will take care of rendering it at the correct position
		}
	}
	
	// NOTE: points are not displayed, they just act as points to know where to spawn agents
	// and where they should move to
	private void initPoints() {
		this.bottomStartPoints = new ArrayList<Point>();
		this.topStartPoints = new ArrayList<Point>();
		
		this.bottomStartPoints.add(new Point(11.2, 19.6));
		this.bottomStartPoints.add(new Point(11.6, 20.8));
		this.bottomStartPoints.add(new Point(12.4, 19.6));
		this.bottomStartPoints.add(new Point(13.2, 20.8));
		this.bottomStartPoints.add(new Point(13.6, 19.6));
		
		this.topStartPoints.add(new Point(11.2, 2.4));
		this.topStartPoints.add(new Point(11.6, 1.2));
		this.topStartPoints.add(new Point(12.4, 2.4));
		this.topStartPoints.add(new Point(13.2, 1.2));
		this.topStartPoints.add(new Point(13.6, 2.4));
		
		this.topEntrance = new Point(12.4, 5.6);
		this.bottomEntrance = new Point(12.4, 16.8);
		
		this.topExit = new Point(3.6, 4.4);
		this.bottomExit = new Point(3.6, 17.6);
	}
	
	private void initAdaptiveWall(Context<Object> context) {
		this.adaptiveWall = new AdaptiveWall(this, space);
		
		context.add(adaptiveWall);
		this.adaptiveWall.updatePosition();
	} 
	
	private void spawn(boolean top) {
		if(Utils.uniform() > GROUP_SPAWNING){
			addPerson(top, getStartPoints(top).get(0), getEntrancePoint(top));
		}else{
			addGroup(top);
		}	
	}
	
	private Person addPerson(boolean top, Point startPoint, Point entryPoint) {
		Person p = new Person(this, space, top, startPoint, entryPoint);
		this.people.add( p );
		
		Context<Object> context = ContextUtils.getContext(this);
		context.add(p);
		p.updatePosition();
		
		return p;
	}
	
	private List<Point> getStartPoints(boolean top) {
		if (top) {
			return this.topStartPoints;
		} else {
			return this.bottomStartPoints;
		}
	}
	
	private Point getEntrancePoint(boolean top) {
		if (top) {
			return this.topEntrance;
		} else {
			return this.bottomEntrance;
		}
	}
	
	public Point getExitPoint(boolean top) {
		if (top) {
			return this.topExit;
		} else {
			return this.bottomExit;
		}
	}
	
	private void addGroup(boolean top) {
		Group g = new Group();
		this.groups.add(g);
		
		List<Point> startPoints = getStartPoints(top);
		Point entrancePoint = getEntrancePoint(top);
		
		Person p0 = addPerson(top, startPoints.get(0), entrancePoint);
		Person p1 = addPerson(top, startPoints.get(1), entrancePoint);
		
		g.addPerson(p0);
		g.addPerson(p1);
		
		//other people in the group spawn with a certain rate
		for(int i = 2; i < 5; i++){
			if(Utils.uniform()<0.6){
				continue;
			}
			
			Person p = addPerson(top, startPoints.get(i), entrancePoint);

			g.addPerson(p);
			p.setRi(Utils.uniform(0.11,0.25));
			p.setVi0(Utils.uniform(1.0,1.4));
		}
	}
}
