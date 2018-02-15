package des.primitives;

import des.Event;
import des.IClock;
import des.IConsumer;
import des.IProcess;
import des.IProducer;
import des.RandomUtils;

public class Delay<T> implements IProcess, IConsumer<T> {

	private IProducer<T> prod;
	private IConsumer<T> forward;
	private T occupied;
	private final static Event EVT = new Event() {};
	
	public Delay(IConsumer<T> forward) {
		this.forward = forward;
		this.occupied = null;
	}
	
	public void setProducer(IProducer<T> prod) {
		this.prod = prod;
	}
	
	@Override
	public void onStart(IClock c) {
	}

	@Override
	public void onEvent(Event e, IClock c) {
		if (EVT != e) 
			return;
		
		T obj = this.occupied;
		this.occupied = null;
		
		// forwarding unconditionally
		this.forward.inputArrival(obj, c);
		
		if (this.prod.hasNext()) {
			T t = this.prod.getNext();
			
			this.inputArrival(t, c);
		}
	}

	@Override
	public void inputArrival(T e, IClock c) {
		if (false == isReady()) {
			throw new RuntimeException(); // TODO remove
		}
		
		this.occupied = e;
		
		double dt = RandomUtils.randomTri(1, 2, 4); // TODO remove hardcoded
		c.scheduleEvent(this, dt, EVT);
	}

	@Override
	public boolean isReady() {
		return occupied == null;
	}
}
