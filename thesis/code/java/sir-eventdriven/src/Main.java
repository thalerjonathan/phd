import java.util.List;

public class Main {

	public static void main(String[] args) {
		int sus0 = 1;
		int inf0 = 10;
		int rec0 = 0;
		int beta = 5;
		double gamma = 0.05;
		double delta = 15;
		
		double tMax = 10;
		
		long start = System.currentTimeMillis();
		
		SIR sir = new SIR(sus0, inf0, rec0, beta, gamma, delta);
		List<SIRStep> steps = sir.run(tMax);
		
		SIRStep lastStep = steps.get(steps.size() - 1);

		long stop = System.currentTimeMillis();
		
		System.out.println("Finished within " + (stop - start) + "ms " +
				"after " + steps.size() + " steps, " +
				"at t = " + lastStep.t + " susceptible = " + lastStep.s + 
				", infected = " + lastStep.i + ", recovered = " + lastStep.r);
	}

}
