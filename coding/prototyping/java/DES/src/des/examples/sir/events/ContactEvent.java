package des.examples.sir.events;
import des.Event;
import des.examples.sir.Person;

public class ContactEvent implements Event {
	
	public Person other;
	
	public ContactEvent(Person person) {
		this.other = person;
	}
}
