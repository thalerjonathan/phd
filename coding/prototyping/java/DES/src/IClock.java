
public interface IClock {
	public void scheduleEvent(Person p, double t, DESEvent e);
	public void scheduleEventNow(Person p, DESEvent e);
	public double getTime();
}
