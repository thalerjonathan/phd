package des.primitives;

import java.util.ArrayList;
import java.util.LinkedList;

import des.Event;
import des.IClock;
import des.IConsumer;
import des.IProcess;
import des.RandomUtils;

public class Service<T> implements IProcess, IConsumer<T> {

	private ArrayList<T> occupiers;
	private LinkedList<T> fifo;
	private IConsumer<T> forward;
	
	public Service(int workers, IConsumer<T> forward) {
		this.occupiers = new ArrayList<T>();
		this.fifo = new LinkedList<T>();
		this.forward = forward;
		
		for (int i = 0; i < workers; ++i) {
			this.occupiers.add(null);
		}
	}
	
	@Override
	public void onStart(IClock c) {
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public void onEvent(Event e, IClock c) {
		if (false == e instanceof Service.FinishedEvent)
			return;
		
		int workerIdx = ((Service<T>.FinishedEvent) e).workerIdx;

		T obj = this.occupiers.get(workerIdx);
		
		this.occupiers.set(workerIdx, null);
		this.forward.inputArrival(obj, c);
		
		this.checkNextInQueue(c);
	}
	
	@Override
	public void inputArrival(T e, IClock c) {
		int freeWorkerPos = -1;
		for (int i = 0; i < this.occupiers.size(); ++i) {
			if (null == this.occupiers.get(i)) {
				freeWorkerPos = i;
				break;
			}
		}
		
		// all workers occupied, enqueue
		if (freeWorkerPos == -1) {
			this.fifo.addLast(e);
			
		} else {
			this.occupiers.set(freeWorkerPos, e);
			
			double dt = RandomUtils.randomTri(3, 5, 20);
			c.scheduleEvent(this, dt, new FinishedEvent(freeWorkerPos));
		}
	}
	
	@Override
	public boolean isReady() {
		// this service has an integrated unlimited queue (fifo) for now => always ready
		return true;
	}
	
	private void checkNextInQueue(IClock c) {
		if (this.fifo.isEmpty()) 
			return;
		
		T obj = this.fifo.removeFirst();
		this.inputArrival(obj, c);
	}
	
	private class FinishedEvent implements Event {
		private int workerIdx;
		
		public FinishedEvent(int idx) {
			this.workerIdx = idx;
		}
	}
	
}
