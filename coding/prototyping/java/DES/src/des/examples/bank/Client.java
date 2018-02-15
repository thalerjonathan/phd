package des.examples.bank;

public class Client {
	private static long CLIENT_ID = 0;
	
	private long id;
	private double enterTime;
	
	public Client(double t) {
		this.enterTime = t;
		this.id = CLIENT_ID++;
	}
	
	public double getEnterTime() {
		return this.enterTime;
	}
	
	public long getId() {
		return this.id;
	}
}
