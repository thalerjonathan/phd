package des.examples.sir;
import java.util.ArrayList;
import java.util.List;

import des.Clock;

public class SIR {

	private final static int POPULATION_SIZE = 1000;
	private final static int NUM_INFECTED = 1;
	
	public static void main(String[] args) {
		Clock c = new Clock();
		List<Person> population = new ArrayList<Person>();
		
		for (int i = 0; i < NUM_INFECTED; ++i) {
			Person p = new Person(Person.SIRState.INFECTED, population);
			population.add(p);
		}
		
		for (int i = 0; i < POPULATION_SIZE - NUM_INFECTED; ++i) {
			Person p = new Person(Person.SIRState.SUSCEPTIBLE, population);
			population.add(p);
		}
		
		for (Person p : population) {
			p.onStart(c);
		}
		
		int events = 0;
		long susceptibles = 0;
		long infected = 0;
		long recovered = 0;
		
		while (c.hasEvents()) {
			events++;

			susceptibles = population.stream()
					.filter(p -> p.getState() == Person.SIRState.SUSCEPTIBLE)
					.count();
			infected = population.stream()
					 .filter(p -> p.getState() == Person.SIRState.INFECTED)
					 .count();
			recovered = population.stream()
					 .filter(p -> p.getState() == Person.SIRState.RECOVERED)
					 .count();
			
			if (infected == 0)
				break;
			
			//if (events % 1000 == 0)
			//	System.out.println(susceptibles + "/" + infected + "/" + recovered);
			
			c.nextEvent();
		}
		
		System.out.println("Simulation finished after " + events + " events at time = " + c.getTime());
		System.out.println(susceptibles + "/" + infected + "/" + recovered);
	}
	
	private static void printPopulationStats(List<Person> pop) {
		int susceptibles = 0;
		int infected = 0;
		int recovered = 0;
		
		for (Person p : pop) {
			if (Person.SIRState.SUSCEPTIBLE == p.getState()) {
				susceptibles++;
				
			} else if (Person.SIRState.INFECTED == p.getState()) {
				infected++;
				
			} else if (Person.SIRState.RECOVERED == p.getState()) {
				recovered++;
			} 
		}
		
		System.out.println(susceptibles + "/" + infected + "/" + recovered);
	}
}
