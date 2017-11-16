package socialForce.scenario.pillarHall;

import repast.simphony.space.continuous.AbstractPointTranslator;
import repast.simphony.space.continuous.NdPoint;
import socialForce.markup.Point;

public class SocialForceToRePastTranslator extends AbstractPointTranslator {

	private final static double METER_2_PX = 25.0;
	
	public static Point scaleFromSocialForceMeterToRePastPixel(Point p) {
		return new Point(p.getX() * METER_2_PX, p.getY() * METER_2_PX);
	}
	
	@Override
	public void transform(double[] transformedLocation, double... targetLocation) {
		for (int i=0; i< targetLocation.length; i++)
			transformedLocation[i] = targetLocation[i] * METER_2_PX;
		
		// NOTE: social-force model has its origin top left and extends both axes positive towards bottom right
		// flip y-achsis
		transformedLocation[1] = PillarHallBuilder.SPACE_HEIGHT - transformedLocation[1];
	}
	
	@Override
	public void translate(double[] location, double... displacement) {
		for (int i = 0; i < displacement.length; i++) {
			double val = location[i] + displacement[i];
			location[i] = val;
		}
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
