package sir;

public interface ISIRAgent {
	public void makeContact();
	
	public boolean isSusceptible();
	public boolean isInfected();
	public boolean isRecovered();
}
