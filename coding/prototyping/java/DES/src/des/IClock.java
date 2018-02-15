package des;

public interface IClock {
	public void scheduleEvent(IProcess p, double t, Event e);
	public void scheduleEventNow(IProcess p, Event e);
	public double getTime();
}
