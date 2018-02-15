package des;

public interface IProcess {

	public void onStart(IClock c);
	void onEvent(Event e, IClock c);
}
