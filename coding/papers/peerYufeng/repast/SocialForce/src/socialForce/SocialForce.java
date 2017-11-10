package socialForce;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class SocialForce {

	public static final double METER_2_PX = 25.0;
	public final static double UNIT_TIME = 0.01;
	
	///////////////////////////////////////////////////////////////////////////
	// Agent creation and paramteres
	
	private final static int roomNum = 1;

	private final static double ENTER_SPEED = 7;
	private double totalTime;
	
	private void calcTime(double t) {
		synchronized(this){
			totalTime += t;
			/*if(existTimes.size()>20){
				totalTime -= existTimes.get(0);
				existTimes.remove(0);
			}
			*/
			}
	}
	
	// TODO: cyclic recurring event, first occurence: time() which means NOW?, then every enterSpeed seconds
	@ScheduledMethod(start = 0, interval = ENTER_SPEED)
	private void spawnVisitors() {
		if(Utils.uniform()>group_spawn_rate){
			add_people();
		}else{
			addGroup();
		}	
	}
	
	// TODO: occurs once at time 0
	private void initRooms() {
		for(int i = 0; i < roomNum; i++){
			add_rooms();
			rooms.get(i).roomNo = i;
			rooms.get(i).alignment = alignments.get(i);
			rooms.get(i).entrance = entrances.get(i);
			rooms.get(i).exit = exits.get(i);
		}
	}
	
	private void addGroup() {
		Color tc =  new Color((int)(Math.random()*0x1000000));
		Screen tdest = rooms.get(0).screens.get((int)(Utils.uniform(0,rooms.get(0).screens.size())));
		Group g = add_groups();
		Person p0 = add_people();
		g.people.add(p0);
		p0.color = tc;
		p0.belongedGroup = g;
		p0.destScreen = tdest;

		//p0, p1 are two individuals that could form a "group"
//			-> a group people >= 2
		Person p1 = add_people();
		p1.pxX = groupPoint0.getX();
		p1.pxY = groupPoint0.getY();
		p1.x = groupPoint0.getX() / METER_2_PX;
		p1.y = groupPoint0.getY() / METER_2_PX;
		p1.color = tc;
		g.people.add(p1);
		p1.belongedGroup = g;
		p1.destScreen = tdest;
		//other people in the group spawn with a certain rate
		for(PointNode pn : groupPoints){
			if(uniform()<0.6){continue;}
			Person pt = add_people();
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
		}
	}
	
	///////////////////////////////////////////////////////////////////////////
	// Collections and agents
	
	private List<Group> groups;
	private List<Room> rooms;
	private List<Person> people;
	private AdaptiveWall adaptiveWall;
	
	///////////////////////////////////////////////////////////////////////////
	// k-means clustering
	
	private int finalClusterNum;
	private int NUM_CLUSTERS = 4;
	
	private boolean isPlot = false;
	private boolean isActive = true;
	private boolean enableVisionArea = false;
	
	private List<Person> people_t;
	private List<Cluster> clusters;
	
	private final static int CLUSTER_THRESH = 50;
	
	private void plotClusters() {
		for (int i = 0; i < NUM_CLUSTERS; i++) {
		    Cluster c = clusters.get(i);
		    c.plotCluster();
		}
	}
	
	// TODO: cyclic event, first occurence after 5 seconds, then every 5 seconds
	@ScheduledMethod(start = 5, interval = 5)
	private void displayClusters() {
		synchronized(this){
			people_t.clear();
			for(Person p:people){
				if(!p.inState(p.reading) && !p.inState(p.moving)){
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
					if(Utils.distance(clusters.get(j).centroid, clusters.get(k).centroid)<clusterThresh){
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
				distance = distance(new Point(p.pxX, p.pxY), c.centroid);
				if(distance < min){
					min = distance;
					cluster = i;
				}
			}
			p.cluster_number = cluster;
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
