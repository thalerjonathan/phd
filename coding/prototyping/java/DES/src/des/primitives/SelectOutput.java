package des.primitives;

import des.ifaces.IClock;
import des.ifaces.IConsumer;
import des.utils.RandomUtils;

public class SelectOutput<T> implements IConsumer<T> {

	private double prob;
	private IConsumer<T> fst;
	private IConsumer<T> snd;
	
	public SelectOutput(double p, IConsumer<T> fst, IConsumer<T> snd) {
		this.prob = p;
		
		this.fst = fst;
		this.snd = snd;
	}
	
	public void inputArrival(T e, IClock c) {
		boolean selectFirst = RandomUtils.randomBool(this.prob);
		
		if (selectFirst) {
			fst.inputArrival(e, c);
		} else {
			snd.inputArrival(e, c);
		}
	}

	@Override
	public boolean isReady() {
		// a selectoutput is ALWAYS ready
		return true;
	}
}
