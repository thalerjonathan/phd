package socialForce.scenario.pillarHall;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import repast.simphony.context.Context;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.util.ContextUtils;
import socialForce.Utils;
import socialForce.markup.Line;
import socialForce.markup.Point;
import socialForce.markup.Rect;

public class PillarHall {
	private ContinuousSpace<Object> space;
	
	private List<Person> people;
	private AdaptiveWall adaptiveWall;
	private List<Group> groups;

	private List<Line> walls;
	
	private List<Point> bottomStartPoints;
	private List<Point> topStartPoints;
	private Point topEntrance;
	private Point bottomEntrance;
	private Point topExit;
	private Point bottomExit;
	
	private Rect movingArea;
	
	private final static double GROUP_SPAWNING = 0.3;
	private final static double SPAWNING_SPEED_TOP = 5.0;
	private final static double SPAWNING_SPEED_BOTTOM = 5.0;
	
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
	
	public List<Line> getWalls() {
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
	
	public void removePerson(Person p) {
		@SuppressWarnings("unchecked")
		Context<Object> context = ContextUtils.getContext(this);
		context.remove(p);

		this.people.remove(p);
		
		if (p.isInGroup()) {
			Group g = p.getGroup();
			if (g.groupHasLeft()) {
				//System.out.println("Group has left");
				this.groups.remove(g);
			}
		}
	}
	
	private void initMarkups(Context<Object> context) {
		this.movingArea = new Rect(new Point(2.8, 5.2), 10.4, 11.6);
		context.add(this.movingArea);
		space.moveTo(this.movingArea, this.movingArea.getRef().getX(), this.movingArea.getRef().getY());
		
		initWalls(context);
		
		for (Point p : this.topStartPoints) {
			context.add(p);
			space.moveTo(p, p.getX(), p.getY());
		}
	}
	
	private void initWalls(Context<Object> context) {
		this.walls = new ArrayList<Line>();
		this.walls.add(new Line(new Point(2, 4), new Point(2, 18)));
		this.walls.add(new Line(new Point(2, 18), new Point(2.8, 18)));
		this.walls.add(new Line(new Point(4.8, 18), new Point(11.2, 18)));
		this.walls.add(new Line(new Point(14, 18), new Point(13.2, 18)));
		this.walls.add(new Line(new Point(14, 18), new Point(14, 4)));
		this.walls.add(new Line(new Point(14, 4), new Point(14, 3.2)));
		this.walls.add(new Line(new Point(4.8, 4), new Point(11.2, 4)));
		
		// Square Pillar
		this.walls.add(new Line(new Point(7.2, 7.2), new Point(7.2, 8.8)));
		this.walls.add(new Line(new Point(7.2, 8.8), new Point(8.8, 8.8)));
		this.walls.add(new Line(new Point(8.8, 8.8), new Point(8.8, 7.2)));
		this.walls.add(new Line(new Point(8.8, 7.2), new Point(7.2, 7.2)));
		
		for (Line w : this.walls) {
			context.add(w);
			space.moveTo(w, w.getFromX(), w.getFromY());
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
		
		@SuppressWarnings("unchecked")
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
		for(int i = 2; i < 5; i++) {
			if(Utils.uniform() < 0.6) {
				continue;
			}
			
			Person p = addPerson(top, startPoints.get(i), entrancePoint);

			g.addPerson(p);
			p.setRi(Utils.uniform(0.11,0.25));
			p.setVi0(Utils.uniform(1.0,1.4));
		}
	}
}
