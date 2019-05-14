import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;

public class Main {
	public static void main(String[] args) throws IOException {
		int sus0 = 1000;
		int inf0 = 1;
		int rec0 = 0;
		int beta = 5;
		double gamma = 0.05;
		double delta = 15;
		
		double tMax = 150;
		double dt = 0.01;
		
		int seed = 42;
		
		long start = System.currentTimeMillis();
		
		SIR sir = new SIR(sus0, inf0, rec0, beta, gamma, delta, seed);
		List<SIRStep> steps = sir.run(tMax, dt);
		
		SIRStep lastStep = steps.get(steps.size() - 1);

		long stop = System.currentTimeMillis();
		
		System.out.println("Finished within " + (stop - start) + "ms " +
				"after " + steps.size() + " steps, " +
				"at t = " + lastStep.t + " susceptible = " + lastStep.s + 
				", infected = " + lastStep.i + ", recovered = " + lastStep.r);
		
		PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter("sir-time.csv")));
		out.println("T,S,I,R");
		for (SIRStep s : steps ) {
			out.println("" + s.t + ", " + s.s + ", " + s.i + ", " + s.r);
		}
		
		out.close();
	}

}
