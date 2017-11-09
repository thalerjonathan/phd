package socialForce;

import java.awt.Color;
import java.util.List;

public class Cluster {
	private int id;
	
	private Point centroid;
	
	private List<Person> points;
	
	public void clear() {
		this.points.clear();
	}
	
	public void plotCluster() {
		//System.out.println("[Cluster: " + id+"]");
		//System.out.println("[Centroid: " + centroid + "]");
		//System.out.println("[Points: \n");
		Color tc = new Color((int)(Math.random()*0x1000000));
		for(Person p : points) {
			p.rectColor = tc;
//			System.out.println(p.pxX + ", " + p.pxY);
		}
		//System.out.println("]");
	}
}
