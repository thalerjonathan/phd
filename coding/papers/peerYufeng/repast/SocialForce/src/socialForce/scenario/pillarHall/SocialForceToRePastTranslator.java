package socialForce.scenario.pillarHall;

import repast.simphony.space.continuous.AbstractPointTranslator;
import repast.simphony.space.continuous.NdPoint;
import socialForce.markup.Point;

public class SocialForceToRePastTranslator extends AbstractPointTranslator {

	private final static double METER_2_PX = 25.0;
	private static double[] transformedLocationTemp = new double[2];
	private static double[] targetLocationTemp = new double[2];
	
	public static Point transformSocialForceMeterToRePastPixel(Point p) {
		targetLocationTemp[0] = p.getX();
		targetLocationTemp[1] = p.getY();
		
		transformSocialForceMeterToRePastPixel(transformedLocationTemp, targetLocationTemp);

		return new Point(transformedLocationTemp[0], transformedLocationTemp[1]);
	}
	
	public static void transformSocialForceMeterToRePastPixel(double[] transformedLocation, double... targetLocation) {
		for (int i=0; i< targetLocation.length; i++)
			transformedLocation[i] = targetLocation[i] * METER_2_PX;
		
		// NOTE: social-force model has its origin top left and extends both axes positive towards bottom right
		// flip y-achsis
		//transformedLocation[1] = PillarHallBuilder.SPACE_HEIGHT - transformedLocation[1];
	}
	
	@Override
	public void transform(double[] transformedLocation, double... targetLocation) {
		transformSocialForceMeterToRePastPixel(transformedLocation, targetLocation);
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
