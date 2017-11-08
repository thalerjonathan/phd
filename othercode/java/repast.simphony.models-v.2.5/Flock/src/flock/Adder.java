package flock;

import repast.simphony.random.RandomHelper;
import repast.simphony.space.Dimensions;
import repast.simphony.space.continuous.ContinuousAdder;
import repast.simphony.space.continuous.ContinuousSpace;

/**
 * A custom implementation of the continuous random adder that clusters newly
 *   added agents near the origin.
 *   
 * @author Nick Collier  
 * @author Eric Tatara
 *
 * @param <T>
 */
public class Adder <T> implements ContinuousAdder<T> {

	/**
	 * Divides the space by this number for adding.  Larger number results in
	 *   closer packing.
	 */
	int clusterFactor;  
	
	public Adder(int clusterFactor){
		this.clusterFactor = clusterFactor;
	}
	
	public void add(ContinuousSpace<T> space, T obj) {
		Dimensions dims = space.getDimensions();
		double[] location = new double[dims.size()];
		findLocation(location, dims);
		while (!space.moveTo(obj, location)) {
			findLocation(location, dims);
		}
	}

	private void findLocation(double[] location, Dimensions dims) {
		double[] origin = dims.originToDoubleArray(null);
		for (int i = 0; i < location.length; i++) {
			try{
				location[i] = RandomHelper.getUniform().nextDoubleFromTo(0, 
						dims.getDimension(i)/clusterFactor) - origin[i];
			}
			catch(Exception e){
				
			}
		}
	}
}