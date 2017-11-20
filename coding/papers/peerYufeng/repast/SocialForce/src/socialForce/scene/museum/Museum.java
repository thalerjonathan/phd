package socialForce.scene.museum;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import repast.simphony.context.Context;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.util.ContextUtils;
import socialForce.markup.Markup;
import socialForce.markup.impl.Line;
import socialForce.markup.impl.Point;
import socialForce.markup.impl.Rect;
import socialForce.misc.Utils;

public class Museum {

	private ContinuousSpace<Object> space;
	
	private Point startPoint;
	public Point endPoint;
	
	private double pre_angle = 5*Math.PI/6;
	private double pre_range = 10;
	private double pre_ppl_psy = 2;
	private double pre_grp_psy = 5;
	private double pre_wall_psy = 2;
	
	private List<Group> groups;
	private List<Room> rooms;
	private List<Person> people;
	public AdaptiveWall adaptiveWall;
	
	private List<Markup> markups;
	private List<Point> groupPoints;
	private Point groupPoint0;
	
	public Rect restArea;
	
	private boolean enableVisionArea = false;
	
	public static final double METER_2_PX = 25.0;
	public final static double UNIT_TIME = 0.01;
	
	public final static int ROOM_NUM = 1;
	private final static double GROUP_SPAWNING = 0.3;
	private final static double ENTER_SPEED = 7;
	
	public Museum(ContinuousSpace<Object> space) {
		this.space = space;
	
		this.groups = new ArrayList<Group>();
		this.rooms = new ArrayList<Room>();
		this.people = new ArrayList<Person>();
	}
	
	public void initAgents(Context<Object> context) {
		initMarkups(context);
		initRoom(context);
		initAdaptiveWall(context);
	}
	
	private void initMarkups(Context<Object> context) {
		this.restArea = new Rect(new Point(230, 160), 190, 180);
		
		this.startPoint = new Point(170, 520);
		this.endPoint = new Point(480, 10);
		
		this.groupPoint0 = new Point(210, 510);
		
		this.groupPoints = new ArrayList<Point>();
		this.groupPoints.add(new Point(200, 540));
		this.groupPoints.add(new Point(130, 510));
		this.groupPoints.add(new Point(140, 540));
		
		initWalls();
		
		for (Markup m : this.markups) {
			context.add(m);
			space.moveTo(m, m.getRef().getX(), m.getRef().getY());
		}
	}
	
	private void initWalls() {
		this.markups = new ArrayList<Markup>();
		this.markups.add(new Line(new Point(50, 450), new Point(100, 0)));	// wall0
		this.markups.add(new Line(new Point(205, 450), new Point(245, 0)));	// wall1
		this.markups.add(new Line(new Point(599, 450), new Point(0, -400)));	// wall2
		this.markups.add(new Line(new Point(50, 50), new Point(60, 0), new Point(400, 0)));	// wall3
		this.markups.add(new Line(new Point(50, 50), new Point(0, 400)));	// wall4
		this.markups.add(new Line(new Point(506, 50), new Point(93, 0)));	// wall5
		this.markups.add(new Line(new Point(450, 450), new Point(149, 0)));	// wall6
		
		this.markups.add(new Line(new Point(70, 80), new Point(0, 320)));	// room1Display
		this.markups.add(new Line(new Point(579, 80), new Point(0, 320)));	// room2Display
		
		this.markups.add(new Line(new Point(600, 400), new Point(-19, 0)));	// wall7
		this.markups.add(new Line(new Point(70, 80), new Point(-19, 0)));	// wall8
		this.markups.add(new Line(new Point(70, 400), new Point(-19, 0)));	// wall9
		this.markups.add(new Line(new Point(599, 80), new Point(-19, 0)));	// wall
	}
	
	private void initAdaptiveWall(Context<Object> context) {
		this.adaptiveWall = new AdaptiveWall(this);
		
		context.add(adaptiveWall);
		space.moveTo(adaptiveWall, 0, 0); // TODO: add at ref position
	} 
	
	public List<Person> getPeople() {
		return Collections.unmodifiableList(this.people);
	}
	
	public List<Room> getRooms() {
		return Collections.unmodifiableList(this.rooms);
	}
	
	public List<Markup> getMarkups() {
		return Collections.unmodifiableList(this.markups);
	}
	
	public boolean isEnableVisionArea() {
		return this.enableVisionArea;
	}
	
	public Point getStartPoint() {
		return this.startPoint;
	}
	
	// TODO: cyclic recurring event, first occurence: time() which means NOW?, then every enterSpeed seconds
	@ScheduledMethod(start = 0, interval = ENTER_SPEED)
	public void spawnVisitors() {
		if(Utils.uniform() > GROUP_SPAWNING){
			addPeople();
		}else{
			addGroup();
		}	
	}
	
	@SuppressWarnings("unchecked")
	private Person addPeople() {
		Person p = new Person(this, space, pre_ppl_psy, pre_range, pre_angle, pre_wall_psy);
		this.people.add( p );
		
		Context<Object> context = ContextUtils.getContext(this);
		context.add(p);
		p.updatePosition();
		
		return p;
	}
	
	// NOTE: occurs once at time 0, but do this when constructing this object
	private void initRoom(Context<Object> context) {
		Line roomAlignment = new Line( new Point(100, 80), new Point(100, 400));
		
		Room room = new Room(this, 5, 2);
		room.roomNo = 0;
		room.alignment = roomAlignment;
		room.initScreens(context);
		//room.entrance = entrances.get(i);
		//room.exit = exits.get(i);

		rooms.add(room);
	}
	
	private void addGroup() {
		Color tc =  new Color((int)(Math.random()*0x1000000));
		Screen tdest = rooms.get(0).screens.get((int)(Utils.uniform(0,rooms.get(0).screens.size())));
		Group g = new Group();
		this.groups.add(g);
		Person p0 = addPeople();
		g.people.add(p0);
		p0.color = tc;
		p0.belongedGroup = g;
		p0.destScreen = tdest;
		p0.updatePosition();
		
		//p0, p1 are two individuals that could form a "group"
//			-> a group people >= 2
		Person p1 = addPeople();
		p1.pxX = groupPoint0.getX();
		p1.pxY = groupPoint0.getY();
		p1.x = groupPoint0.getX() / METER_2_PX;
		p1.y = groupPoint0.getY() / METER_2_PX;
		p1.color = tc;
		g.people.add(p1);
		p1.belongedGroup = g;
		p1.destScreen = tdest;
		p1.updatePosition();
		
		//other people in the group spawn with a certain rate
		for(Point pn : groupPoints){
			if(Utils.uniform()<0.6){continue;}	// TODO: name magic number
			Person pt = addPeople();
			pt.pxX = pn.getX();
			pt.pxY = pn.getY();
			pt.x = pn.getX() / METER_2_PX;
			pt.y = pn.getY() / METER_2_PX;
			g.people.add(pt);
			pt.belongedGroup = g;
			pt.color = tc;
			pt.ri = Utils.uniform(0.11,0.25);
			pt.vi0 = Utils.uniform(1.0,1.4);
			pt.destScreen = tdest;
			pt.updatePosition();
		}
	}
}
