public class SIREvent {	
	double timeStamp;
	Integer receiver;
	Integer sender;
	SIREventType type;
	Object data;
	
	public String toString() {
		return "" + type + "@" + timeStamp + " " + sender + " -> " + receiver + "\n";
	}
}