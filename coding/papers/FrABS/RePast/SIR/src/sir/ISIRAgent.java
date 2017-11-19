package sir;

public interface ISIRAgent {
	
	public void makeContact();
	public void inContactWith(ISIRAgent a);
	public void infect();
	
	public boolean isSusceptible();
	public boolean isInfected();
	public boolean isRecovered();
	
	public double getInfectionProb();
	public double getContactRate(); 
	public double getIllnessDuration();
}
