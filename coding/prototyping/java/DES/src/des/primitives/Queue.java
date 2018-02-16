package des.primitives;

import java.util.LinkedList;

import des.ifaces.IClock;
import des.ifaces.IConsumer;
import des.ifaces.IProducer;

public class Queue<T> implements IConsumer<T>, IProducer<T> {

	private LinkedList<T> fifo;
	private IConsumer<T> forward;
	
	public Queue(IConsumer<T> forward) {
		this.forward = forward;
		this.fifo = new LinkedList<T>();
	}
	
	@Override
	public void inputArrival(T e, IClock c) {
		// forwarding conditionally
		if (this.forward.isReady()) {
			this.forward.inputArrival(e, c);
			
		} else {
			this.fifo.addLast(e);
		}
	}

	@Override
	public boolean isReady() {
		// this queue is always ready to take elements
		// need to implement a limited size, then it could refuse
		return true;
	}

	@Override
	public T getNext() {
		return this.fifo.removeFirst();
	}

	@Override
	public boolean hasNext() {
		return false == this.fifo.isEmpty();
	}
}
