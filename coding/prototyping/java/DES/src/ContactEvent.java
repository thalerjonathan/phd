
public class ContactEvent implements DESEvent {
	
	public Person other;
	
	public ContactEvent(Person person) {
		this.other = person;
	}
}
