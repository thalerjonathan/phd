package socialForce.scenario.pillarHall;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import socialForce.Utils;

public class Group {

	private double destX;
	private double destY;
	
	private double readingTime;
	
	private boolean modified;
	private boolean exit;
	
	private Color color;
	
	private List<Person> people;
	
	public Group() {
		this.color = new Color((int)(Math.random()*0x1000000));
		this.readingTime = Utils.uniform(10, 60);
		
		this.people = new ArrayList<Person>();
	}
	
	public double getReadingTime() {
		return this.readingTime;
	}
	
	public boolean isMember(Person p) {
		return this.people.contains(p);
	}
	
	public void addPerson(Person p) {
		p.setReadingTime(this.readingTime);
		p.setColor(this.color);
		p.setGroup(this);
		
		this.people.add(p);	
	}
}
