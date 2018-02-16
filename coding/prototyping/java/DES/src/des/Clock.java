package des;
import java.util.PriorityQueue;

import des.ifaces.IClock;
import des.ifaces.IProcess;

public class Clock implements IClock {

	private double time;
	private PriorityQueue<QueueElem> queue;
	
	private class QueueElem implements Comparable<QueueElem> {
		double time;
		IProcess p;
		Event e;
		
		public QueueElem(double time, IProcess p, Event e) {
			this.time = time;
			this.p = p;
			this.e = e;
		}
		
		@Override
		public String toString() {
			return e.getClass().getSimpleName() + ", from " + p + " at t = " + time;
		}
		
		@Override
		public int compareTo(QueueElem o) {
			if (null == o)
				return 1;
			
			if (this == o)
				return 0;
			
			return Double.compare(this.time, o.time);
		}
	}
	
	public Clock() {
		this.time = 0;
		this.queue = new PriorityQueue<>();
	}
	
	@Override
	public void scheduleEvent(IProcess p, double dt, Event e) {
		double absoluteTime = this.time + dt;
		this.queue.add(new QueueElem(absoluteTime, p, e));
	}
	
	@Override
	public void scheduleEventNow(IProcess p, Event e) {
		this.queue.add(new QueueElem(this.time, p, e));
	}
	
	public void nextEvent() {
		QueueElem e = this.queue.poll();
		//System.out.println(e);
		this.time = e.time;
		
		e.p.onEvent(e.e, this);
	}
	
	public boolean hasEvents() {
		return false == this.queue.isEmpty();
	}
	
	@Override
	public double getTime() {
		return this.time;
	}
}
