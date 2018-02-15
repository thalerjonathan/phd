import java.util.List;
import java.util.Random;

public class Person {
	
	public enum SIRState {
		SUSCEPTIBLE,
		INFECTED,
		RECOVERED
	}
	
	private SIRState s;
	private Random r;
	private List<Person> population;
	
	private final static double TIME_UNIT = 1.0;
	
	private final static double INFECTIVITY = 0.05;
	private final static double CONTACT_RATE = 5.0;
	private final static double ILLNESS_DURATION = 15.0;
	
	public Person(SIRState s, Random r, List<Person> population) {
		this.s = s;
		this.r = r;
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
	
	public void onEvent(DESEvent e, IClock c) {
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
				if (INFECTIVITY >= r.nextDouble()) {
					this.infected(c);
				}
			}
		}
	} 
	
	private void makeContact(IClock c) {
		if (SIRState.SUSCEPTIBLE != this.s)
			return;
		
		int numcontacts = (int) this.randomExp(TIME_UNIT / CONTACT_RATE);
		for (int i = 0; i < numcontacts; i++) {
			int randIdx = (int) (r.nextDouble() * population.size());
			Person randContact = population.get(randIdx);
			
			/*
			if (SIRState.INFECTED == randContact.getState()) {
				if (INFECTIVITY >= r.nextDouble()) {
					this.infected(c);
					return;
				}
			}
			*/
			
			double rdt = r.nextDouble();
			c.scheduleEvent(randContact, rdt, new ContactEvent(this));
		}
		
		c.scheduleEvent(this, TIME_UNIT, new MakeContacts());
	}
	
	private void infected(IClock c) {
		double recoveryTime = this.randomExp(1 / ILLNESS_DURATION);
		c.scheduleEvent(this, recoveryTime, new RecoveryEvent());
	}
	
	private double randomExp(double lambda) {
	    return (-Math.log(r.nextDouble())) / lambda;
	}
}
