public class SIREvent {	
	double timeStamp;
	Agent receiver;
	Agent sender;
	SIREventType type;
	
	public String toString() {
		return "" + type + "@" + timeStamp + " " + sender.getState() + " -> " + receiver.getState() + "\n";
	}
}