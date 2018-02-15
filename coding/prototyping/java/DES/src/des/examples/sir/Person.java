package des.examples.sir;
import java.util.List;

import des.Event;
import des.IClock;
import des.IProcess;
import des.RandomUtils;
import des.examples.sir.events.ContactEvent;
import des.examples.sir.events.MakeContacts;
import des.examples.sir.events.RecoveryEvent;

public class Person implements IProcess {
	
	public enum SIRState {
		SUSCEPTIBLE,
		INFECTED,
		RECOVERED
	}
	
	private SIRState s;
	private List<Person> population;
	
	private final static double TIME_UNIT = 1.0;
	
	private final static double INFECTIVITY = 0.05;
	private final static double CONTACT_RATE = 5.0;
	private final static double ILLNESS_DURATION = 15.0;
	
	public Person(SIRState s, List<Person> population) {
		this.s = s;
		this.population = population;
	}
	
	public SIRState getState() {
		return this.s;
	}
	
	public void onStart(IClock c) {
		if (SIRState.SUSCEPTIBLE == this.s) {
			this.makeContact(c);
			
		} else if (SIRState.INFECTED == this.s) {
			this.infected(c);
		}
	}
	
	public void onEvent(Event e, IClock c) {
		if (e instanceof RecoveryEvent) {
			this.s = SIRState.RECOVERED;
			
		} else if (e instanceof MakeContacts) {
			this.makeContact(c);
			
		} else if (e instanceof ContactEvent) {
			contactFrom(((ContactEvent) e).other, c);
		}
	}
	
	@Override
	public String toString() {
		return this.s.toString();
	}
	
	private void contactFrom(Person p, IClock c) {
		if (SIRState.INFECTED == this.s) {
			c.scheduleEventNow(p, new ContactEvent(this));
			
		} else if (SIRState.SUSCEPTIBLE == this.s) {
			if (SIRState.INFECTED == p.getState()) {
				if (INFECTIVITY >= RandomUtils.nextDouble()) {
					this.infected(c);
				}
			}
		}
	} 
	
	private void makeContact(IClock c) {
		if (SIRState.SUSCEPTIBLE != this.s)
			return;
		
		int numcontacts = (int) RandomUtils.randomExp(TIME_UNIT / CONTACT_RATE);
		for (int i = 0; i < numcontacts; i++) {
			int randIdx = (int) (RandomUtils.nextDouble() * population.size());
			Person randContact = population.get(randIdx);
			
			/*
			if (SIRState.INFECTED == randContact.getState()) {
				if (INFECTIVITY >= r.nextDouble()) {
					this.infected(c);
					return;
				}
			}
			*/
			
			double rdt = RandomUtils.nextDouble();
			c.scheduleEvent(randContact, rdt, new ContactEvent(this));
		}
		
		c.scheduleEvent(this, TIME_UNIT, new MakeContacts());
	}
	
	private void infected(IClock c) {
		double recoveryTime = RandomUtils.randomExp(1 / ILLNESS_DURATION);
		c.scheduleEvent(this, recoveryTime, new RecoveryEvent());
	}
}
