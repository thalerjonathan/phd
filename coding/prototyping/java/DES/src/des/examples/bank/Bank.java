package des.examples.bank;

import des.Clock;
import des.ifaces.IClock;
import des.primitives.Delay;
import des.primitives.Queue;
import des.primitives.SelectOutput;
import des.primitives.Service;
import des.primitives.Sink;
import des.primitives.Source;
import des.utils.Histogram;

public class Bank {
	
	public static void main(String[] args) {
		Clock c = new Clock();
		Histogram hist = new Histogram(10, 0, 60);
		
		Sink<Client> clientSink = new Sink<Client>() {
			@Override
			public void inputArrival(Client client, IClock c) {
				double dur = c.getTime() - client.getEnterTime();
				hist.add(dur);
			}
		};
		Service<Client> serviceAtCashier = new Service<Client>(5, clientSink);
		
		// TODO: something seems not right: when selecting none to the service, the mean time reduces
		SelectOutput<Client> needAdditonalService = new SelectOutput<Client>(0.3, serviceAtCashier, clientSink);
		
		Delay<Client> serviceAtAtm = new Delay<Client>(needAdditonalService);
		Queue<Client> queueToAtm = new Queue<Client>(serviceAtAtm);
		
		serviceAtAtm.setProducer(queueToAtm);
		
		SelectOutput<Client> needToSeeCashier = new SelectOutput<Client>(0.5, queueToAtm, serviceAtCashier);
		
		Source<Client> source = new Source<Client>(0.75, needToSeeCashier) {
			@Override
			protected Client createNextEntity(IClock c) {
				return new Client(c.getTime());
			}
		};
		
		source.onStart(c);
		serviceAtCashier.onStart(c);

		long events = 0;
		
		while (c.hasEvents()) {
			events++;

			//if (events > 10_000_000)
			//	break;
			
			if (hist.getTotalItems() > 500_000)
				break;
			
			c.nextEvent();
		}
		
		System.out.println("Simulation finished after " + events + " events at time = " + c.getTime());
		System.out.println("Average time in system = " + hist.getMean());
		System.out.println(hist);
	}
}
