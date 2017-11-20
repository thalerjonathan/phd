package socialForce.scene.museum;

import java.util.ArrayList;
import java.util.List;

/*
 * Group only contains a collection of people, representing
 * they are in a same group, for the calculation of social force
 * in class Person.
 * All the people in a same group will be marked in a same 
 * color.

 * @author		Yufeng Deng
 * @since 		17/8/2017
 */
public class Group {

	public List<Person> people;
	
	public boolean isUpdated = false;
	
	public Group() {
		this.people = new ArrayList<Person>();
	}
}
