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
	private List<Point> exitPoints;
	
	private Rect area;
	private Rect legalArea;
	
	private boolean enableVisionArea;
	
	private static final boolean ENABLE_VISION_AREA = false;
	
	public static final double METER_2_PX = 25.0;
	public final static double UNIT_TIME = 0.01;
	
	private final static double GROUP_SPAWNING = 0.3;
	private final static double SPAWNING_SPEED_TOP = 5.0;
	private final static double SPAWNING_SPEED_BOTTOM = 5.0;
	
	private final static double EXIT_RATE = 0.5;
	
	public PillarHall(ContinuousSpace<Object> space) {
		this.space = space;
	
		this.groups = new ArrayList<Group>();
		this.people = new ArrayList<Person>();
		
		this.enableVisionArea = false;
		
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
	
	public AdaptiveWall getAdaptiveWall() {
		return this.adaptiveWall;
	}
	
	@ScheduledMethod(start = 0, interval = SPAWNING_SPEED_TOP)
	public void spawnEntryTop() {
		spawn(true);
	}
	
	public boolean isEnableVisionArea() {
		return this.enableVisionArea;
	}
	
	@ScheduledMethod(start = 0, interval = SPAWNING_SPEED_BOTTOM)
	public void spawnEntryBottom() {
		spawn(false);
	}
	
	private void initMarkups(Context<Object> context) {
		this.area = new Rect(new Point(70, 130), 260, 290);
		this.legalArea = new Rect(new Point(50, 30), 300, 490);
		
		initWalls(context);
	}
	
	private void initWalls(Context<Object> context) {
		this.walls = new ArrayList<Wall>();
		this.walls.add(new Wall(new Point(50, 100), new Point(0, 350)));
		this.walls.add(new Wall(new Point(50, 450), new Point(20, 0)));
		this.walls.add(new Wall(new Point(120, 450), new Point(160, 0)));
		this.walls.add(new Wall(new Point(350, 450), new Point(-20, 0)));
		this.walls.add(new Wall(new Point(350, 450), new Point(0, -350)));
		this.walls.add(new Wall(new Point(350, 100), new Point(0, -20)));
		this.walls.add(new Wall(new Point(120, 100), new Point(160, 0)));
		
		// Square Pillar
		this.walls.add(new Wall(new Point(180, 180), new Point(0, 40), new Point(40, 40), new Point(40, 0), new Point(0, 0)));
		
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
		this.exitPoints = new ArrayList<Point>();
		
		this.bottomStartPoints.add(new Point(280, 490));
		this.bottomStartPoints.add(new Point(290, 520));
		this.bottomStartPoints.add(new Point(310, 490));
		this.bottomStartPoints.add(new Point(330, 520));
		this.bottomStartPoints.add(new Point(340, 490));
		
		this.topStartPoints.add(new Point(280, 60));
		this.topStartPoints.add(new Point(290, 30));
		this.topStartPoints.add(new Point(310, 60));
		this.topStartPoints.add(new Point(330, 30));
		this.topStartPoints.add(new Point(340, 60));
		
		this.topEntrance = new Point(310, 140);
		this.bottomEntrance = new Point(310, 420);
		
		this.exitPoints.add(new Point(90, 110));
		this.exitPoints.add(new Point(90, 440));
	}
	
	private void initAdaptiveWall(Context<Object> context) {
		this.adaptiveWall = new AdaptiveWall(this);
		
		context.add(adaptiveWall);
		space.moveTo(adaptiveWall, 0, 0); // add at 0/0, the rendering will take care of rendering it at the correct position
	} 
	
	private void spawn(boolean top) {
		if(Utils.uniform() > GROUP_SPAWNING){
			addPerson(getStartPoints(top).get(0), getEntrancePoint(top));
		}else{
			addGroup(top);
		}	
	}
	
	private Person addPerson(Point startPoint, Point entryPoint) {
		Person p = new Person(this, space, startPoint, entryPoint);
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
	
	private void addGroup(boolean top) {
		Group g = new Group();
		this.groups.add(g);
		
		List<Point> startPoints = getStartPoints(top);
		Point entrancePoint = getEntrancePoint(top);
		
		Person p0 = addPerson(startPoints.get(0), entrancePoint);
		Person p1 = addPerson(startPoints.get(1), entrancePoint);
		
		g.addPerson(p0);
		g.addPerson(p1);
		
		//other people in the group spawn with a certain rate
		for(int i = 2; i < 5; i++){
			if(Utils.uniform()<0.6){
				continue;
			}
			
			Person p = addPerson(startPoints.get(i), entrancePoint);

			g.addPerson(p);
			p.setRi(Utils.uniform(0.11,0.25));
			p.setVi0(Utils.uniform(1.0,1.4));
		}
	}
}
