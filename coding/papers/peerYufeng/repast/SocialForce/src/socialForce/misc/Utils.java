package socialForce.misc;

import repast.simphony.random.RandomHelper;

public class Utils {
	
	public static double sqr(double x) {
		return x*x;
	}
	
	public static double uniform() {
		return RandomHelper.getUniform().nextDouble();
	}
	
	public static double uniform(double from, double to) {
		return RandomHelper.getUniform().nextDoubleFromTo(from, to);
	}
}
