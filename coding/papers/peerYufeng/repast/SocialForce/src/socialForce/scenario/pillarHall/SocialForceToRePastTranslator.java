package socialForce.scenario.pillarHall;

import repast.simphony.space.continuous.AbstractPointTranslator;
import repast.simphony.space.continuous.NdPoint;

public class SocialForceToRePastTranslator extends AbstractPointTranslator {

	@Override
	public void translate(double[] location, double... displacement) {
		for (int i = 0; i < displacement.length; i++) {
			double val = location[i] + displacement[i];
			location[i] = val;
		}
	}
	
	@Override
	public void transform(double[] transformedLocation, double... targetLocation) {
		for (int i=0; i< targetLocation.length; i++)
			transformedLocation[i] = targetLocation[i];
	}

	@Override
	public void translate(NdPoint location, double[] newLocation, double... displacement) {
		double[] locationArr = location.toDoubleArray(new double[location.dimensionCount()]);
		translate(locationArr, displacement);
		for (int i = 0; i < locationArr.length; i++) {
			newLocation[i] = locationArr[i];
		}
	}

	@Override
	public boolean isPeriodic() {
		return false;
	}
}
