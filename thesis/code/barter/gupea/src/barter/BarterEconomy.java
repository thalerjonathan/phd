package barter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.evensen.util.Pair;
import org.jfree.data.statistics.Statistics;

import sim.engine.Schedule;
import sim.engine.SimState;
import sim.engine.Steppable;

/**
 * The main model class. Responsible for setting up the simulation model.
 * Creates and schedules the agents when <code>start()</code> is called
 * and is then ready to be stepped forward.
 * 
 * @NotThreadSafe This class is NOT thread safe!
 */
public class BarterEconomy extends SimState {

	private static final long serialVersionUID = -2988851414518202520L;

	private BarterParams params;

	private Map<Class<? extends BarterStrategy>, Double> barterStrategies;
	private Class<? extends ImprovementStrategy> improvmentStrategy;
	private ReplacementStrategy replacementStrategy;

	private List<List<TradeAgent>> traders;

	private double[] avgPrice;
	private double[] avgRelPrice;
	private double[] avgScore;
	private double totalAvgScore;

	// to hold mean price of stdDevGood for many periods
	private List<Double> meanPrices;

	// average score of all agents with common equilibrium prices
	private double commonPriceScore;

	private int[] tradeOrder;

	/**
	 * Constructs <code>BarterEconomy</code> with default parameters.
	 */
	public BarterEconomy() {
		/* The seed set here is overwritten with the seed from Console, which
		   in turn is overwritten with the seed from BarterParams class (if it
		   is non-zero there).
		 */
		super(0);
		params = new BarterParams();
		barterStrategies = new HashMap<Class<? extends BarterStrategy>, Double>();
	}

	/**
	 * Starts the simulation by creating and scheduling the agents. Does not run
	 * the simulation. The latter is done by repeatedly calling the
	 * <code>this.schedule.step()</code> method.
	 */
	@Override
	public void start() {
		super.start();

		// TODO: Horrible ugliness since we can not call the Console to set the seed
		//       there. This overrides the previous state if the constructor was
		//       called with an argument not equal to 0. (Pelle)
		if (params._getSeed() != 0) {
			random.setSeed(params._getSeed());
		}

		// add default barter strategy
		if (barterStrategies.isEmpty()) {
			addBarterStrategy(OriginalBarterStrategy.class, 1.0);
		}

		// add default replacement strategy
		if (replacementStrategy == null) {
			replacementStrategy = new DelphiReplacementStrategy();
		}

		// add default improvement strategy
		if (improvmentStrategy == null) {
			improvmentStrategy = CopyAndMutateImprovementStrategy.class;
		}

		traders = new ArrayList<List<TradeAgent>>(params.getNumGoods());
		meanPrices = new LinkedList<Double>();
		avgPrice = new double[params.getNumGoods()];
		avgRelPrice = new double[params.getNumGoods()];
		avgScore = new double[params.getNumGoods()];

		int[] numProducers = getNumProducers();

		// create agents with common prices to get commonPriceScore
		createAgents(numProducers, true);
		scheduleAgents();

		// step once and get the commonPriceScore
		schedule.step(this);
		calcAvgScore();
		commonPriceScore = totalAvgScore;
		schedule.reset();
		traders.clear();

		// recreate the agents with prices according to model parameters
		createAgents(numProducers, false);
		scheduleAgents();
		calcAvgScore();

		//REVERT-BUGS - swapped the orderings of getting nextGen and setting demand/supply
		schedule.scheduleRepeating(params.getReproducePeriod(),
				params.getNumGoods() + 1, new Steppable() {
			@Override
			public void step(SimState state) {
				calcAvgScore();
				for (int good = 0; good < params.getNumGoods(); good++) {
					replacementStrategy.getNextGeneration(traders.get(good), params, random);
					for (TradeAgent ta : traders.get(good)) {
						ta.resetScore();
					}
				}

				if (params.isVarySupply()) {
					producerShift();
				}
			}
			private static final long serialVersionUID = -6813563792056673743L;
		}
		, params.getReproducePeriod());

		schedule.scheduleRepeating(0, params.getNumGoods(), new Steppable() {
			@Override
			public void step(SimState state) {
				// every agent produces after each period
				for (List<TradeAgent> producers : traders) {
					for (TradeAgent ta : producers) {
						ta.produce();
					}
				}

				double period = state.schedule.getTime();
				if (params._isShouldQuit() &&
						period >= Schedule.EPOCH &&
						period < Schedule.AFTER_SIMULATION &&
						period > params._getPeriodsBeforeQuit()) {

					System.err.println("Reached periodsBeforeQuit (" +
							params._getPeriodsBeforeQuit() + ") Terminating.");
					System.exit(0);
				}

				// reschedule agents
				scheduleAgents();
			}
			private static final long serialVersionUID = -8411883437500190906L;
		});
	}

	private int[] getNumProducers() {
		int[] numProducers = new int[params.getNumGoods()];

		if (params.isVarySupply()) {
			// divide producers among goods unequally (prefers first goods)
			// TODO: crashes if agentsPerGood < 10
			Arrays.fill(numProducers, 10); // at least 10 for each good
			int maxAgents = params.getNumGoods() * (params.getAgentsPerGood() - 10);

			for (int good = 0; good < params.getNumGoods() - 1; good++) {
				numProducers[good] += random.nextInt(maxAgents + 1);
				maxAgents -= numProducers[good] - 10;
			}
			numProducers[params.getNumGoods() - 1] += maxAgents;

		} else {
			Arrays.fill(numProducers, params.getAgentsPerGood());
		}

		return numProducers;
	}

	private void createAgents(int[] numProducers, boolean commonPrice) {
		double shareSum = 0.0;
		for (Double share : barterStrategies.values()) {
			shareSum += share;
		}

		for (int produceGood = 0; produceGood < params.getNumGoods(); produceGood++) {

			traders.add(new ArrayList<TradeAgent>(numProducers[produceGood]));

			for (Class<? extends BarterStrategy> strat : barterStrategies.keySet()) {
				long numAgents = (long) Math.ceil(barterStrategies.get(strat)
						* numProducers[produceGood]
						               / shareSum);

				int n = 0;
				while (n < numAgents) {

					// break if already have desired number of agents due to
					// the rounding error
					// TODO: use some other method to get more fair shares
					if (traders.get(produceGood).size()
							== numProducers[produceGood]) {
						break;
					}

					double[] prices = commonPrice ?
							params.getEquiPrice() : getPriceVector(produceGood);

							TradeAgent ta =
								new TradeAgent(produceGood, prices, params);

							try {
								ta.setBarterStrategy(strat.newInstance());
								ta.setImprovementStrategy(improvmentStrategy.newInstance());
							} catch (Exception e) {
								e.printStackTrace(System.err);
							}

							traders.get(produceGood).add(ta);
							n++;
				}
			}
		}
	}

	private void scheduleAgents() {
		tradeOrder = Util.randomPermutation(params.getNumGoods(), random);

		for (List<TradeAgent> producers : traders) {
			for (TradeAgent ta : producers) {
				schedule.scheduleOnce(ta, tradeOrder[ta.getProduceGood()]);
			}
		}
	}

	private void producerShift() {
		long replacements = Math.round(params.getTotalAgents()
				* params.getProducerShiftRate());

		//REVERT-BUGS for some reason Gintis gives that many replacements per good
		replacements *= params.getNumGoods(); 

		for (int j = 0; j < replacements; j++) {
			Pair<Integer, Integer> pair = Util.randomPair(params.getNumGoods(), random);

			List<TradeAgent> g1Producers = getProducers(pair.first);
			List<TradeAgent> g2Producers = getProducers(pair.second);

			// Since an agent produces one single good,
			// $g_1Producers \cap g_2Producers = \emptyset \Rightarrow t_1 \neq t_2$. 
			TradeAgent t1 = g1Producers.get(random.nextInt(g1Producers.size()));
			TradeAgent t2 = g2Producers.get(random.nextInt(g2Producers.size()));

			// TODO: Explain what's going on here. (Pelle)
			if (avgScore[pair.first] < avgScore[pair.second]) {
				if (g1Producers.size() > 10) {
					t1.copy(t2);
					g1Producers.remove(t1);
					g2Producers.add(t1);
				}
			} else {
				if (g2Producers.size() > 10) {
					t2.copy(t1);
					g2Producers.remove(t2);
					g1Producers.add(t2);
				}
			}
		}
	}

	/**
	 * Returns a price vector of length numGoods with random prices from [0..1),
	 * unless checkEfficiency is true - all prices are 1 or equiInitialPrices is
	 * true - returns equilibrium prices. Price of produceGood is reduced by
	 * ownGoodPriceFactor.
	 */
	private double[] getPriceVector(int produceGood) {
		double[] price = new double[params.getNumGoods()];

		if (params.isCheckEfficiency()) {
			Arrays.fill(price, 1.0);
		}
		else if (params.isEquiInitialPrices()) {
			price = params.getEquiPrice();
		}
		else {
			for (int good = 0; good < price.length; good++) {
				price[good] = random.nextDouble();
			}
		}

		price[produceGood] *= params.getProduceGoodPriceFactor();

		return price;
	}

	private void calcAvgScore() {
		for (int good = 0; good < params.getNumGoods(); good++) {
			avgPrice[good] = 0;
			avgScore[good] = 0;
		}

		for (List<TradeAgent> producers : traders) {
			for (TradeAgent ta : producers) {

				for (int good = 0; good < params.getNumGoods(); good++) {
					avgPrice[good] += ta.getPrice(good)
					/ ta.getPrice(params.getPriceUnitGood());
				}

				avgScore[ta.getProduceGood()] += ta.getScore();
			}
		}

		totalAvgScore = 0;
		for (int good = 0; good < params.getNumGoods(); good++) {
			double equiPrice = params.getEquiPrice()[good];

			avgPrice[good] /= params.getTotalAgents();
			avgRelPrice[good] = (avgPrice[good] - equiPrice) / equiPrice;
			avgScore[good] /= getProducers(good).size();
			totalAvgScore += avgScore[good];
		}

		totalAvgScore /= params.getNumGoods();

		meanPrices.add(avgPrice[params.getStdDevGood()]);
		if (meanPrices.size() > params.getReproducePeriod()) {
			meanPrices.remove(0);
		}
	}

	/**
	 * Calculates and returns the standard deviation of producer prices for
	 * the good specified in <code>BarterParams.getStdDevGood()</code>.
	 * Producers are the agents who produce the given good.
	 * 
	 * @return The standard deviation of producer prices.
	 */
	public double getProducerPriceStdDev() {
		List<TradeAgent> producers = getProducers(params.getStdDevGood());
		List<Double> prices = new ArrayList<Double>(producers.size());

		for (TradeAgent ta : producers) {
			prices.add(ta.getPrice(params.getStdDevGood()) 
					/ ta.getPrice(params.getPriceUnitGood()));
		}

		return Statistics.getStdDev(prices.toArray(new Double[] {}));
	}

	/**
	 * Calculates and returns the standard deviation of consumer prices for
	 * the good specified in <code>BarterParams.getStdDevGood()</code>.
	 * Consumers are the agents who do not produce the given good.
	 * 
	 * @return The standard deviation of consumer prices.
	 */
	public double getConsumerPriceStdDev() {
		List<Double> prices = new ArrayList<Double>(params.getTotalAgents()
				- getProducers(params.getStdDevGood()).size());

		for (int produceGood = 0; produceGood < params.getNumGoods(); produceGood++) {
			if (produceGood == params.getStdDevGood()) {
				// producers of this good not considered
				continue;
			}

			for (TradeAgent ta : getProducers(produceGood)) {
				prices.add(ta.getPrice(params.getStdDevGood())
						/ ta.getPrice(params.getPriceUnitGood()));
			}
		}

		return Statistics.getStdDev(prices.toArray(new Double[] {}));
	}

	/**
	 * Calculates and returns the standard deviation of mean prices for
	 * the good specified in <code>BarterParams.getStdDevGood()</code>.
	 * The mean price from the last 10 reproduction periods is considered.
	 * 
	 * @return The standard deviation of mean prices.
	 */
	public double getMeanPriceStdDev() {
		return Statistics.getStdDev(meanPrices.toArray(new Double[] {}));
	}

	/**
	 * Sets the model parameters. If the simulation is running, some parameters
	 * don't take effect. Those parameters are:
	 * <ul>
	 * <li><code>reproducePeriod</code>
	 * <li><code>agentsPerGood</code>
	 * <li><code>numGoods</code> (the length of <code>consume</code> array)
	 * </ul>
	 * And some parameters are only used when starting up the model:
	 * <ul>
	 * <li><code>produceGoodPriceFactor</code>
	 * <li><code>equiInitialPrices</code>
	 * </ul>
	 * 
	 * @param newParams	New parameters for the model.
	 */
	public void setParams(BarterParams newParams) {
		// if the simulation is not running (no events scheduled),
		// we want to copy all the params 
		if (schedule.scheduleComplete()) {
			params = newParams.clone();
			return;
		}

		// some of the changed params will take effect instantly

		double[] oldValue = params.getConsumeArray();
		double[] newValue = newParams.getConsumeArray();

		// make runtime changes only if the number of goods didn't change
		if (oldValue.length == newValue.length && !Arrays.equals(oldValue, newValue)) {

			// new consume vector for every agent
			for (List<TradeAgent> producers : traders) {
				for (TradeAgent ta : producers) {
					ta.setConsume(newParams.getConsumeArray());
				}
			}
			params.setConsume(newParams.getConsume());
		}

		if (params.getStdDevGood() != newParams.getStdDevGood()) {
			meanPrices.clear();
			params.setStdDevGood(newParams.getStdDevGood());
		}

		params.setMaxTries(newParams.getMaxTries());
		params.setCheckEfficiency(newParams.isCheckEfficiency());
		params.setVarySupply(newParams.isVarySupply());
		params.setProducerShiftRate(newParams.getProducerShiftRate());
		params.setMutationRate(newParams.getMutationRate());
		params.setMutationDelta(newParams.getMutationDelta());
		params.setReplacementRate(newParams.getReplacementRate());
		params.setUpdateFrequency(newParams.getUpdateFrequency());
		params.setSkipChartFrames(newParams.getSkipChartFrames());

		// TODO: runtime changes to agentsPerGood could also be implemented
	}

	/**
	 * Adds a new barter strategy for the agents. Every strategy is assigned a share
	 * and when the model is started those shares are summed up
	 * to determine how many agents with each strategy should be created. If only
	 * one strategy is added, all agents will use it. If no strategies are added,
	 * all agents use {@link OriginalBarterStrategy OriginalBarterStrategy}.
	 * 
	 * @param strategy	The class of the strategy.
	 * @param share		The share of the agents that will use this strategy. Only
	 * 		the relative size of the share matters compared to other strategies.
	 */
	public void addBarterStrategy(Class<? extends BarterStrategy> strategy, double share) {
		barterStrategies.put(strategy, share);
	}

	/**
	 * Sets the replacement strategy that determines how the agents are replaced
	 * at each {@link BarterParams#setReproducePeriod(int) reproducePeriod}. The
	 * default is {@link DelphiReplacementStrategy DelphiReplacementStrategy}.
	 * 
	 * @param replacementStrategy	The replacement strategy.
	 */
	public void setReplacementStrategy(
			Class<? extends ReplacementStrategy> replacementStrategy) {
		try {
			this.replacementStrategy = replacementStrategy.newInstance();			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Sets the improvement strategy that determines how the agents improve
	 * their prices. The default is
	 * {@link CopyAndMutateImprovementStrategy CopyAndMutateImprovementStrategy}.
	 * 
	 * @param improvementStrategy	The improvement strategy.
	 */
	public void setImprovementStrategy(
			Class<? extends ImprovementStrategy> improvementStrategy) {
		this.improvmentStrategy = improvementStrategy;
	}

	/**
	 * Returns the parameters of the model. Modifications to those have no effect
	 * unless they are set again using the {@link #setParams(BarterParams) setParams}
	 * method.
	 * 
	 * @return Current model parameters.
	 */
	public BarterParams getParams() {
		return params.clone();
	}

	/**
	 * Returns the average scores for the producers of each good. The scores are
	 * calculated every {@link BarterParams#setReproducePeriod(int) reproducePeriod}
	 * and are just returned here.
	 * 
	 * @return Average scores for the producers of each good.
	 */
	public double[] getAvgScore() {
		return avgScore;
	}

	/**
	 * Returns the average score for all producers (agents). This is the
	 * "efficiency" of the economy. Calculated every
	 * {@link BarterParams#setReproducePeriod(int) reproducePeriod}.
	 * 
	 * @return	The average score for all producers.
	 */
	public double getTotalAvgScore() {
		return totalAvgScore;
	}

	/**
	 * Returns the average relative price for each good. The prices are
	 * relative to the equilibrium price. To get the difference in percentages,
	 * the values should be multiplied by 100.
	 * 
	 * @return The average relative prices.
	 */
	public double[] getAvgRelPrice() {
		return avgRelPrice;
	}

	/**
	 * Returns the {@link #getTotalAvgScore() totalAverageScore} if all the agents
	 * traded with equilibrium prices. This value is calculated once every time the
	 * model is started.
	 * 
	 * @return The score that could be achieved if all agents traded with equilibrium prices.
	 */
	public double getCommonPriceScore() {
		return commonPriceScore;
	}

	/**
	 * Returns the agents who produce the given good.
	 * 
	 * @param good	The produced good.
	 * @return A list of producers of given good. 
	 */
	public List<TradeAgent> getProducers(int good) {
		return traders.get(good);
	}

	int[] getTradeOrder() {
		return tradeOrder;
	}

	/**
	 * Returns all the agents in a single list.
	 * 
	 * @return The list of all agents.
	 */
	public List<TradeAgent> getAgents() {
		List<TradeAgent> a = new ArrayList<TradeAgent>();
		for(List<TradeAgent> e : traders) {
			a.addAll(e);
		}

		return a;
	}
}