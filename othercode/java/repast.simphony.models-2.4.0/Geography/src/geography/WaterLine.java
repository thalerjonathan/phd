package geography;

/**
 * WaterLines are water supply pipes and are represented by a line feature.
 * 
 * @author Eric Tatara
 *
 */
public class WaterLine {

	private String name;
	private double flowRate;
	
	public WaterLine(String name, double flowRate){
		this.name = name;
		this.flowRate = flowRate;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public double getFlowRate() {
		return flowRate;
	}

	public void setFlowRate(double flowRate) {
		this.flowRate = flowRate;
	}	
}