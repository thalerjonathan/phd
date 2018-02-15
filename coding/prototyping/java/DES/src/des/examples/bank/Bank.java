package des.examples.bank;

import java.util.ArrayList;
import java.util.List;

import des.Clock;
import des.IClock;
import des.primitives.Delay;
import des.primitives.Queue;
import des.primitives.SelectOutput;
import des.primitives.Service;
import des.primitives.Sink;
import des.primitives.Source;

public class Bank {
	
	public static void main(String[] args) {
		Clock c = new Clock();
		List<Double> durations = new ArrayList<Double>();
		
		Sink<Client> clientSink = new Sink<Client>() {
			@Override
			public void inputArrival(Client client, IClock c) {
				double dur = c.getTime() - client.getEnterTime();
				//System.out.println("Client " + client.getId() + " time in system: " + dur);
				durations.add(dur);
			}
		};
		Service<Client> serviceAtCashier = new Service<Client>(5, clientSink);
		
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

			if (events > 10_000_000)
				break;
			
			c.nextEvent();
		}
		
		System.out.println("Simulation finished after " + events + " events at time = " + c.getTime());
		
		double avg = 0;
		for (Double d : durations) {
			avg += d;
		}
		
		avg /= durations.size();
		
		System.out.println("Average time in system = " + avg);
	}
}
