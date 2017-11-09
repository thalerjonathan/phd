package socialForce;

import repast.simphony.random.RandomHelper;

public class Utils {
	
	public static double sqr(double x) {
		return x*x;
	}
	
	public static double distance(double x1, double y1, double x2, double y2) {
		return Math.sqrt(sqr(x1-x2) + sqr(y1-y2));
	}
	
	public static double uniform() {
		return RandomHelper.getUniform().nextDouble();
	}
	
	public static double uniform(double from, double to) {
		return RandomHelper.getUniform().nextDoubleFromTo(from, to);
	}
}
