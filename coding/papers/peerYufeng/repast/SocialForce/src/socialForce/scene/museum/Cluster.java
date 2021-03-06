package socialForce.scene.museum;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import socialForce.markup.impl.Point;

public class Cluster {
	public int id;
	
	public Point centroid;
	
	public List<Person> points;
	
	public Cluster() {
		this.points = new ArrayList<Person>();
	}
	
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
