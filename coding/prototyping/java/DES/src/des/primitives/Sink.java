package des.primitives;

import des.IClock;
import des.IConsumer;

public class Sink<T> implements IConsumer<T> {

	public Sink() {
	}
	
	@Override
	public boolean isReady() {
		// a sink is ALWAYS ready
		return true;
	}

	@Override
	public void inputArrival(T e, IClock c) {
	}
}
