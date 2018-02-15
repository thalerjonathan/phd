package des;

public class RandomUtils {

	public static double nextDouble() {
		return Math.random();
	}
	
	public static double randomExp(double lambda) {
	    return (-Math.log(nextDouble())) / lambda;
	}
	
	public static boolean randomBool(double p) {
		return nextDouble() <= p;
	}
	
	// taken from https://stackoverflow.com/questions/33220176/triangular-distribution-in-java
	public static double randomTri(double a, double b, double c) {
	    double F = (c - a) / (b - a);
	    double rand = nextDouble();
	    if (rand < F) {
	        return a + Math.sqrt(rand * (b - a) * (c - a));
	    } else {
	        return b - Math.sqrt((1 - rand) * (b - a) * (b - c));
	    }
	}
}
