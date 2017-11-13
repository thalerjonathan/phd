package socialForce;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import repast.simphony.context.Context;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.util.ContextUtils;
import socialForce.geom.Line;
import socialForce.geom.Point;
import socialForce.geom.Rect;
import socialForce.markup.Wall;

public class SocialForce {

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
	
	private List<Wall> walls;
	private List<Point> groupPoints;
	private Point groupPoint0;
	
	public Rect restArea;
	
	private Point topLeft;
	private Point downRight;
	
	private int finalClusterNum;
	private int NUM_CLUSTERS = 4;
	
	private boolean isPlot = false;
	private boolean isActive = true;
	private boolean enableVisionArea = false;
	
	private List<Person> people_t;
	private List<Cluster> clusters;
	
	private final static int CLUSTER_THRESH = 50;
	
	public static final double METER_2_PX = 25.0;
	public final static double UNIT_TIME = 0.01;
	
	public final static int ROOM_NUM = 1;
	private final static double GROUP_SPAWNING = 0.3;
	private final static double ENTER_SPEED = 7;
	
	public SocialForce(ContinuousSpace<Object> space) {
		this.space = space;
	
		this.groups = new ArrayList<Group>();
		this.rooms = new ArrayList<Room>();
		this.people = new ArrayList<Person>();
		
		this.people_t = new ArrayList<Person>();
		this.clusters = new ArrayList<Cluster>();
	}
	
	public void initAgents(Context context) {
		initMarkups(context);
		initRoom(context);
		initAdaptiveWall(context);
	}
	
	private void initMarkups(Context context) {
		this.restArea = new Rect(new Point(230, 160), 190, 180);
		
		this.startPoint = new Point(170, 520);
		this.endPoint = new Point(480, 10);
		
		this.groupPoint0 = new Point(210, 510);
		
		this.groupPoints = new ArrayList<Point>();
		this.groupPoints.add(new Point(200, 540));
		this.groupPoints.add(new Point(130, 510));
		this.groupPoints.add(new Point(140, 540));
		
		this.topLeft = new Point(50, 50);
		this.downRight = new Point(599, 450);
		
		initWalls(context);
	}
	
	private void initWalls(Context context) {
		this.walls = new ArrayList<Wall>();
		this.walls.add(new Wall(new Point(50, 450), new Point(100, 0)));	// wall0
		this.walls.add(new Wall(new Point(205, 450), new Point(245, 0)));	// wall1
		this.walls.add(new Wall(new Point(599, 450), new Point(0, -400)));	// wall2
		this.walls.add(new Wall(new Point(50, 50), new Point(60, 0), new Point(400, 0)));	// wall3
		this.walls.add(new Wall(new Point(50, 50), new Point(0, 400)));	// wall4
		this.walls.add(new Wall(new Point(506, 50), new Point(93, 0)));	// wall5
		this.walls.add(new Wall(new Point(450, 450), new Point(149, 0)));	// wall6
		
		this.walls.add(new Wall(new Point(70, 80), new Point(0, 320)));	// room1Display
		this.walls.add(new Wall(new Point(579, 80), new Point(0, 320)));	// room2Display
		
		this.walls.add(new Wall(new Point(600, 400), new Point(-19, 0)));	// wall7
		this.walls.add(new Wall(new Point(70, 80), new Point(-19, 0)));	// wall8
		this.walls.add(new Wall(new Point(70, 400), new Point(-19, 0)));	// wall9
		this.walls.add(new Wall(new Point(599, 80), new Point(-19, 0)));	// wall
		
		for (Wall w : this.walls) {
			context.add(w);
			space.moveTo(w, 0, 0); // add at 0/0, the rendering will take care of rendering it at the correct position
		}
	}
	
	private void initAdaptiveWall(Context context) {
		this.adaptiveWall = new AdaptiveWall(this);
		
		context.add(adaptiveWall);
		space.moveTo(adaptiveWall, 0, 0); // add at 0/0, the rendering will take care of rendering it at the correct position
	} 
	
	public List<Person> getPeople() {
		return Collections.unmodifiableList(this.people);
	}
	
	public List<Room> getRooms() {
		return Collections.unmodifiableList(this.rooms);
	}
	
	public List<Wall> getWalls() {
		return Collections.unmodifiableList(this.walls);
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
	
	private Person addPeople() {
		Person p = new Person(this, space, pre_ppl_psy, pre_range, pre_angle, pre_wall_psy);
		this.people.add( p );
		
		Context<Object> context = ContextUtils.getContext(this);
		context.add(p);
		
		return p;
	}
	
	// NOTE: occurs once at time 0, but do this when constructing this object
	private void initRoom(Context context) {
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
	
	private void plotClusters() {
		for (int i = 0; i < NUM_CLUSTERS; i++) {
		    Cluster c = clusters.get(i);
		    c.plotCluster();
		}
	}
	
	// TODO: cyclic event, first occurence after 5 seconds, then every 5 seconds
	@ScheduledMethod(start = 5, interval = 5)
	public void displayClusters() {
		synchronized(this){
			people_t.clear();
			for(Person p:people){
				if(!p.isReading() && !p.isMoving()){
					continue;
				}
				people_t.add(p);
			}
			clustering();
			if(isPlot){plotClusters();}
		}
	}
	
	private void kmeans() {
		for (int i = 0; i < NUM_CLUSTERS; i++) {
		    Cluster cluster = new Cluster();
		    cluster.id = i;
		    Point centroid = createRandomPoint(topLeft,downRight,i);
		    cluster.centroid = centroid;
		    clusters.add(cluster);
		}
		boolean finish = false;
			int iteration = 0;
			while(!finish) {
		    clearClusters();
		    List<Point> lastCentroids = getCentroids();
		    //Assign points to the closer cluster
		    assignCluster();
		    //Calculate new centroids.
		    calculateCentroids();
		    iteration++;
			List<Point> currentCentroids = getCentroids();
		    //Calculates total distance between new and old Centroids
		    double distance = 0;
		    for(int i = 0; i < lastCentroids.size(); i++) {
		        distance += Utils.distance(lastCentroids.get(i),currentCentroids.get(i));
		    }       	
		    if(distance == 0) {
		        finish = true;
		    }
		}
	}
	
	private void clustering() {
		calcBestClusters();
		kmeans();
		finalClusterNum = 0;
		for(Cluster c : clusters){
			if(c.points.size()>0){finalClusterNum++;}
		}
	}
	
	private void calcBestClusters() {
		outerloop: 
		for(int i = 7; i > 1; i--){
			NUM_CLUSTERS = i;
			kmeans();
			boolean flag = true;
			for(int j = 0; j < NUM_CLUSTERS-1; j++){
				for(int k = j+1; k < NUM_CLUSTERS; k++){
					if(Utils.distance(clusters.get(j).centroid, clusters.get(k).centroid)<CLUSTER_THRESH){
						flag = false;
						continue outerloop;
					}
				}
			}
			if(flag){
				return;
			}
		}
	}
	
	private void clearClusters() {
		for(Cluster cluster : clusters) {
			cluster.clear();
		}
	}
	
	// NOTE: this does not involve any random element at the moment
	private Point createRandomPoint(Point topLeft, Point bottomRight, int i) {
		double x = 100;
		double y = topLeft.getY() + (bottomRight.getY() - topLeft.getY())*i/NUM_CLUSTERS;
		return new Point(x,y);
	}
	
	private List<Point> getCentroids() {
		List<Point> centroids = new ArrayList<Point>();
		for(Cluster cluster : clusters) {
		  	Point aux = cluster.centroid;
		   	Point point = new Point(aux.getX(),aux.getY());
		   	centroids.add(point);
		}
		return centroids;
	}
	
	private void assignCluster() {
		double max = Double.MAX_VALUE;
		double min = max; 
		int cluster = 0;                 
		double distance = 0.0; 
		for(Person p : people_t) {
			min = max;
			for(int i = 0; i < NUM_CLUSTERS; i++) {
				Cluster c = clusters.get(i);
				distance = Utils.distance(new Point(p.pxX, p.pxY), c.centroid);
				if(distance < min){
					min = distance;
					cluster = i;
				}
			}
			p.clusterNumber = cluster;
			clusters.get(cluster).points.add(p);
		}
	}
	
	private void calculateCentroids() {
		for(Cluster cluster : clusters) {
			double sumX = 0;
			double sumY = 0;
			List<Person> list = cluster.points;
			int n_points = list.size();
			for(Person point : list) {
				sumX += point.pxX;
				sumY += point.pxY;
			}
		            
			Point centroid = cluster.centroid;
			if(n_points > 0) {
				double newX = sumX / n_points;
				double newY = sumY / n_points;
				centroid.x = newX;
				centroid.y = newY;
			}
		}
	}
}
