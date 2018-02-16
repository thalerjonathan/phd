package des.primitives;

import des.Event;
import des.ifaces.IClock;
import des.ifaces.IConsumer;
import des.ifaces.IProcess;
import des.utils.RandomUtils;

public abstract class Source<T> implements IProcess {

	private double arrivalRate;
	private IConsumer<T> consumer;
	private final static Event EVT = new Event() {};
	
	public Source(double rate, IConsumer<T> consumer) {
		this.arrivalRate = rate;
		this.consumer = consumer;
	}

	@Override
	public void onStart(IClock c) {
		scheduleNextArrival(c);
	}
	
	@Override
	public void onEvent(Event e, IClock c) {
		if (e != EVT)
			return;
		
		T t = createNextEntity(c);
		this.consumer.inputArrival(t, c);
		
		this.scheduleNextArrival(c);
	}
	
	protected abstract T createNextEntity(IClock c);

	private void scheduleNextArrival(IClock c) {
		double dt = RandomUtils.randomExp(this.arrivalRate);
		c.scheduleEvent(this, dt, EVT);
	}
}
