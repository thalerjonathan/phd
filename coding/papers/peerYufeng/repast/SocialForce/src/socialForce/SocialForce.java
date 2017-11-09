package socialForce;

import java.awt.Color;
import java.util.List;

public class SocialForce {

	public static final double METER_2_PX = 25.0;
	public final static double UNIT_TIME = 0.01;
	
	///////////////////////////////////////////////////////////////////////////
	// Agent creation and paramteres
	
	private final static int roomNum = 1;

	// TODO: cyclic recurring event, first occurence: time() which means NOW?, then every enterSpeed seconds
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
	
}
