package barter;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.evensen.util.GetOptions;

/**
 * Handles the command line arguments for the program.
 * 
 * @ThreadSafe This class is thread safe!
 */
public class BarterEconomyOptions {
	// TODO: Make an enum out of this, including e.g. balloon-help texts. (Pelle)
	private static final GetOptions opts = new GetOptions();
	private static final Map<String,OptionHandler<List<String>,BarterParams>> optHandlers = setupOptions();
	
	private static void addOption(Map<String,OptionHandler<List<String>,BarterParams>> handlers,
			String optId, GetOptions.Arity arity,
			OptionHandler<List<String>,BarterParams> handler) {
		opts.add(optId, arity);
		handlers.put(optId, handler);
	}
	
	private static Map<String,OptionHandler<List<String>,BarterParams>> setupOptions() {
		Map<String,OptionHandler<List<String>,BarterParams>> handlers =
			new LinkedHashMap<String,OptionHandler<List<String>,BarterParams>>();

		final String seedId = "-s";
		addOption(handlers, seedId, GetOptions.Arity.ONE, new OptionHandler<List<String>,BarterParams>() {
			public void handleOption(List<String> option, BarterParams eco) {
				long seed;
				try {
					seed = Long.parseLong(option.get(0));
					eco._setSeed(seed);
				} catch(NumberFormatException e) {
					throw new IllegalArgumentException(seedId + " takes a single long argument.");
				}
			}
		});

		final String agentsId = "-a";
		addOption(handlers, agentsId, GetOptions.Arity.ONE, new OptionHandler<List<String>,BarterParams>() {
			public void handleOption(List<String> option, BarterParams eco) {
				int agentsPerGood;
				try {
					agentsPerGood = Integer.parseInt(option.get(0));
					eco.setAgentsPerGood(agentsPerGood);
				} catch(NumberFormatException e) {
					throw new IllegalArgumentException(agentsId + " takes a single int argument.");
				}
			}
		});

		final String reproduceId = "-rp";
		addOption(handlers, reproduceId, GetOptions.Arity.ONE, new OptionHandler<List<String>,BarterParams>() {
			public void handleOption(List<String> option, BarterParams eco) {
				int repPeriod;
				try {
					repPeriod = Integer.parseInt(option.get(0));
					eco.setReproducePeriod(repPeriod);
				} catch(NumberFormatException e) {
					throw new IllegalArgumentException(reproduceId + " takes a single int argument.");
				}
			}
		});

		final String replaceRateId = "-rr";
		addOption(handlers, replaceRateId, GetOptions.Arity.ONE, new OptionHandler<List<String>,BarterParams>() {
			public void handleOption(List<String> option, BarterParams eco) {
				double replaceRate;
				try {
					replaceRate = Double.parseDouble(option.get(0));
					eco.setReplacementRate(replaceRate);
				} catch(NumberFormatException e) {
					throw new IllegalArgumentException(replaceRateId + " takes a single double argument.");
				}
			}
		});

		final String mutationRateId = "-m";
		addOption(handlers, mutationRateId, GetOptions.Arity.ONE, new OptionHandler<List<String>,BarterParams>() {
			public void handleOption(List<String> option, BarterParams eco) {
				double mutationRate;
				try {
					mutationRate = Double.parseDouble(option.get(0));
					eco.setMutationRate(mutationRate);
				} catch(NumberFormatException e) {
					throw new IllegalArgumentException(mutationRateId + " takes a single double argument.");
				}
			}
		});

		final String consumeId = "-c";
		addOption(handlers, consumeId, GetOptions.Arity.MANY, new OptionHandler<List<String>,BarterParams>() {
			public void handleOption(List<String> option, BarterParams eco) {
				try {
					eco.setConsume(option);
				} catch(NumberFormatException e) {
					throw new IllegalArgumentException(consumeId + " takes a number of doubles equal to the number of goods.");
				}
			}
		});

		final String triesId = "-t";
		addOption(handlers, triesId, GetOptions.Arity.ONE, new OptionHandler<List<String>,BarterParams>() {
			public void handleOption(List<String> option, BarterParams eco) {
				try {
					eco.setMaxTries(Integer.parseInt(option.get(0)));
				} catch(NumberFormatException e) {
					throw new IllegalArgumentException(triesId + " takes a single integer argument.");
				}
			}
		});

		final String quitId = "-q";
		addOption(handlers, quitId, GetOptions.Arity.ONE, new OptionHandler<List<String>,BarterParams>() {
			public void handleOption(List<String> option, BarterParams eco) {
				try {
					eco._setPeriodsBeforeQuit(Integer.parseInt(option.get(0)));
					eco._setShouldQuit(true);
				} catch(NumberFormatException e) {
					throw new IllegalArgumentException(quitId + " takes a single integer argument.");
				}
			}
		});

		final String updateId = "-uf";
		addOption(handlers, updateId, GetOptions.Arity.ONE, new OptionHandler<List<String>,BarterParams>() {
			public void handleOption(List<String> option, BarterParams eco) {
				try {
					int skipFrames = Integer.parseInt(option.get(0));
					eco.setSkipChartFrames(skipFrames);
				} catch(NumberFormatException e) {
					throw new IllegalArgumentException(updateId + " takes a single int argument.");
				}
			}
		});

		final String mutationDeltaId = "-md";
		addOption(handlers, mutationDeltaId, GetOptions.Arity.ONE, new OptionHandler<List<String>,BarterParams>() {
			public void handleOption(List<String> option, BarterParams eco) {
				double mutationDelta;
				try {
					mutationDelta = Double.parseDouble(option.get(0));
					eco.setMutationDelta(mutationDelta);
				} catch(NumberFormatException e) {
					throw new IllegalArgumentException(mutationDeltaId + " takes a single double argument.");
				}
			}
		});

		final String varySupplyId = "-vs";
		addOption(handlers, varySupplyId, GetOptions.Arity.NONE, new OptionHandler<List<String>,BarterParams>() {
			public void handleOption(List<String> option, BarterParams eco) {
				eco.setVarySupply(true);
			}
		});
		
		return handlers;
	}

	/**
	 * Prints out the syntax help.
	 */
	public static void showSyntax() {
		System.out.println("Available options:\n" +
				"-s, --seed  <long>								Random seed\n" +
				"-a, --agents <int>								Number of agents per good\n" +
				"-rp, --reproduce-period <int>					Periods between reproduction\n" +
				"-rr, --replacement-rate <0.0-1.0>				Ratio of replaced agents\n" +
				"-m,  --mutation-rate <double>					Ratio of mutation for replaced agents\n" +
				"-md, --mutation-delta <double>                 Mutation delta\n" +
				"-c, --consume <rate1 ... rateN>				Consumption rates\n" +
				"-t, --maxtries <int>							Max offerer tries\n" +
				"-ps, --producer-shift-rate <double>			?\n" +
				"-vs, --vary-supply								Vary the goods supply\n" +
				"-ce, --check-efficiency						?\n" +
				"-ep, --equilibrium-initial-prices				?\n" +
				"-f, --log-file	<filename>						Log to filename\n" +
				"-fa, --log-auto-file							Log to auto-generated filename\n" +
				"-lr, --log-rate <n>							Log every n:th period\n" +
				"-uf, --update-frequency <n>					Update charts every n:th period\n" +
				"-q, --quit-after <n>							Quit after n periods\n");
	}

	/**
	 * Sets the command line options in a {@link BarterParams BarterParams} object.
	 * 
	 * @param args	Command line arguments.
	 * @param eco	The parameters object to configure accordingly.
	 */
	public static void handleOptions(String[] args, BarterParams eco) {
		//		numGoods-numAgents-stoppedAfter-reproducePeriod-replacementRate-mutationRateconsume-
		//		maxTries-prodShiftRate-varSupply-checkEff-equiInit-seed.log
		opts.parse(args);

		if(!opts.parsed()) {
			showSyntax();
			throw new IllegalArgumentException("");
		}

		for(String optId : opts.getMatches().keySet()) {
			optHandlers.get(optId).handleOption(opts.matched(optId), eco);
		}
	}

	// TODO: Make a "main-substitute" returning all base parameters and number of steps till equilibrium.
	// numGoods-numAgents-stoppedAfter-reproducePeriod-replacementRate-mutationRateconsume-
	// maxTries-prodShiftRate-varSupply-checkEff-equiInit-seed.log
}
