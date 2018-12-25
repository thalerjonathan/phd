package barter;

import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.util.List;

import sim.engine.SimState;
import sim.engine.Steppable;
import ec.util.MersenneTwisterFast;

/**
 * The agent class.
 * 
 * @NotThreadSafe This class is NOT thread safe!
 */
public final class TradeAgent implements Steppable,Comparable<TradeAgent> {

	private static final long serialVersionUID = 4247735360343951500L;

	private BarterStrategy barterStrategy;
	private ImprovementStrategy improvementStrategy;

	private int produceGood;
	private double score;
	private double produceAmount;

	private double[] price;			// price \equiv p_{this,goodIdx}
	private double[] demand;
	private double[] exchangeFor;
	private double[] inventory;
	private double[] consume;

	private transient TradeAgentProxy myProxy; // No need to serialize this

	/**
	 * Constructs an agent ready to start trading.
	 * 
	 * @param produceGood	the good that this agent produces
	 * @param price			the price vector for this agent
	 * @param params		the model parameters used to set up the agent
	 */
	public TradeAgent(int produceGood, double[] price, BarterParams params) {
		this.produceGood = produceGood;
		this.price = price.clone();
		int numGoods = params.getNumGoods();

		demand = new double[numGoods];
		exchangeFor = new double[numGoods];
		inventory = new double[numGoods];
		barterStrategy = new OriginalBarterStrategy();
		improvementStrategy = new CopyAndMutateImprovementStrategy();

		myProxy = new TradeAgentProxy(this);

		setConsume(params.getConsumeArray());
		produce();

		assert checkInvariants(); // invariants established?
	}

	private boolean checkInvariants() {
		if (consume.length != price.length
				|| consume.length != demand.length
				|| consume.length != exchangeFor.length
				|| consume.length != inventory.length
				|| produceGood < 0
				|| produceGood >= consume.length
				|| produceAmount != consume[produceGood] * consume.length
				|| score < 0
				|| myProxy == null
				|| barterStrategy == null
				|| improvementStrategy == null) {

			return false;
		}

		// TODO: can't check in Gintis' version
//		for (int g = 0; g < consume.length; g++) {
//			if (exchangeFor[g] != price[g] * demand[g] / price[produceGood]) {
//				return false;
//			}
//		}

		return true;
	}

	void produce() {
		// first reset the inventory for all goods
		for (int good = 0; good < price.length; good++) {
			inventory[good] = 0;
		}
		inventory[produceGood] = produceAmount;

		setDemandAndSupply();
		assert checkInvariants();
	}

	private void setDemandAndSupply() {
		double lambda = lambda();

		assert lambda == 1.0;

		for (int good = 0; good < price.length; good++) {
			if (good != produceGood) {
				demand[good] = consume[good] * lambda;
				exchangeFor[good] = price[good] * demand[good] / price[produceGood];
			}
		}

		demand[produceGood] = 0;
		exchangeFor[produceGood] = 0;
	}

	/**
	 * How much value agent wants to consume divided by how much value it has
	 * produced (according to its own prices).
	 */
	private double lambda() {
		double consumeValue = 0;

		// See section 3.2, eq. 2, incomeConstraint \equiv M^o
		double incomeConstraint = price[produceGood] * inventory[produceGood];

		// Compute denominator of eq. 4 from section 3.2.
		for (int good = 0; good < price.length; good++) {
			// See sec. 3.2, eq. 4
			// consumeValue += price[good] * consume[good];
			consumeValue += price[good] * inventory[good]; //REVERT-BUGS
		}

		// TODO: parameterize this (boolean allowExcessConsumption)
		if (incomeConstraint > consumeValue) {
			return 1;
		}

		return incomeConstraint / consumeValue; // See section 3.2, eq. 4, \equiv \lambda^*
	}

	/**
	 * Tells this agent to start trading.  
	 */
	@Override
	public void step(SimState state) {
		BarterEconomy be = (BarterEconomy) state;
		int maxTries = be.getParams().getMaxTries();
		double[] amounts = new double[2];

		for (int wantGood : be.getTradeOrder()) {
			if (demand[wantGood] == 0) {
				continue;
			}

			List<TradeAgent> producers = be.getProducers(wantGood);

			amounts[0] = demand[wantGood];
			amounts[1] = exchangeFor[wantGood];

			for (int i = 0; i < maxTries; i++) {
				TradeAgent responder = producers.get(be.random.nextInt(producers.size()));

				if (responder.acceptOffer(produceGood, amounts[1], amounts[0])) {

					// in and out amounts adjusted by initiator and responder

					// If resources of responder is depleted
					adjustAmounts(wantGood, 0, produceGood, 1, amounts);
					responder.adjustAmounts(produceGood, 1, wantGood, 0, amounts);

					Double before1 = null, before2 = null;
					assert (before1 = responder.getInventory(produceGood) + inventory[produceGood]) != null;
					assert (before2 = responder.getInventory(wantGood) + inventory[wantGood]) != null;

					exchangeGoods(wantGood, amounts[0], produceGood, amounts[1]);
					responder.exchangeGoods(produceGood, amounts[1], wantGood, amounts[0]);

					Double after1 = null, after2 = null;
					assert (after1 = responder.getInventory(produceGood) + inventory[produceGood]) != null;
					assert (after2 = responder.getInventory(wantGood) + inventory[wantGood]) != null;

					assert Math.abs(before1 - after1) < 0.02;
					assert Math.abs(before2 - after2) < 0.02;

					eat();
					responder.eat();

					// Are invariants preserved after a trade?
					assert checkInvariants();
					assert responder.checkInvariants();

					break;
				}
			}
		}
	}

	/**
	 * Exchanged amount is limited by two factors:
	 * - how much of the outgoing good do we have in inventory
	 * - how much of the incoming good do we actually want (demand)
	 * 
	 * @return Array with adjusted amounts (inAmount, outAmount)
	 */
	private void adjustAmounts(int inGood, int inIdx,
			int outGood, int outIdx, double[] amounts) {

		double outAmount = amounts[outIdx];
		double inAmount = amounts[inIdx];

		double haveAmount = inventory[outGood];
		if (outAmount > haveAmount) {
			// we are offering more than we have, so lets offer only the
			// amount that we have and request less by the same factor
			inAmount *= haveAmount / outAmount;
			outAmount = haveAmount;
		}

		// shouldn't be the case for offerer!
		double demandAmount = demand[inGood];
		if (inAmount > demandAmount) {
			// lets take only as much as we want and give less by the same
			// factor
			outAmount *= demandAmount / inAmount;
			inAmount = demandAmount;
		}

		//REVERT-BUGS is relevant if don't set demand and supply after getting next generation
		if (exchangeFor[inGood] < outAmount) {
			// we don't agree to give as much as requested, so lets give what
			// we can and take less by the same factor
			inAmount *= exchangeFor[inGood] / outAmount;
			outAmount = exchangeFor[inGood];
		}

		// to ensure that trade conditions have not been changed
		assert outAmount == 0.0 ||
			Math.abs(amounts[inIdx] / amounts[outIdx] - inAmount / outAmount) < 0.00001
					: amounts[inIdx] / amounts[outIdx] + " vs " + inAmount / outAmount;

		amounts[inIdx] = inAmount;
		amounts[outIdx] = outAmount;
	}

	/**
	 * Takes one good from inventory and adds another. Also sets new demand and
	 * supply for received good.
	 */
	private void exchangeGoods(int inGood, double inAmount, int outGood, double outAmount) {
		inventory[inGood] += inAmount;
		inventory[outGood] -= outAmount;

		demand[inGood] -= inAmount;

		// exchangeFor[inGood] = validateAmount(price[inGood] * demand[inGood] / price[outGood]);
		exchangeFor[inGood] -= outAmount; //REVERT-BUGS

		for (int good = 0; good < consume.length; good++) {
			inventory[good] = validateAmount(inventory[good]);
			demand[good] = validateAmount(demand[good]);
			exchangeFor[good] = validateAmount(exchangeFor[good]);
		}
	}

	private boolean acceptOffer(int offerGood, double offerAmount, double wantAmount) {
		return inventory[produceGood] > 0 && 
			barterStrategy.acceptOffer(myProxy,	offerGood, offerAmount, wantAmount);
	}

	private final void eat() {
		double min = Double.MAX_VALUE;

		// Calculate utility function (See section 3, eq. 1)
		for (int good = 0; good < price.length; good++) {
			min = Math.min(min, inventory[good] / consume[good]);
		}

		// If we have no inventory to consume, just return.
		if (min == 0.0) {
			return;
		}

		score += min;

		assert min <= 1 : "min > 1.0, was " + min;

		if (min < 1.0) {
			for (int good = 0; good < price.length; good++) {
				inventory[good] *= 1.0 - min;
			}
		} else {
			produce();
		}
	}

	private final static double validateAmount(double amount) {
		assert amount >= 0 : ("Negative amount: " + amount); 

		return amount < 0.01 ? 0.0 : amount;
	}

	// TODO: Could this be used for any malice/stupidity? Probably by the BarterStrategy! (Pelle)
	void resetScore() {
		score = 0.0;
	}

	/**
	 * Give this agent a chance to improve its prices based on a better agent. 
	 */
	public void improve(TradeAgent better,
			BarterParams params, MersenneTwisterFast random) {

		double[] newPrices =
			improvementStrategy.improve(better.getView(), myProxy, params, random);

		for (int good = 0; good < price.length; good++) {
			price[good] = newPrices[good];
		}

		assert checkInvariants();
	}

	/**
	 * Copy the strategy and also produced good of a better agent.
	 */
	void copy(TradeAgent better) {
		// TODO: Find a good way for the strategy to override this. (Pelle)

		for (int good = 0; good < price.length; good++) {
			price[good] = better.price[good];
			inventory[good] = better.inventory[good];
			demand[good] = better.demand[good];
			exchangeFor[good] = better.exchangeFor[good];
		}

		produceAmount = better.produceAmount;
		produceGood = better.produceGood;

		assert checkInvariants();
	}

	/**
	 * Returns the production good of this agent.
	 * @return The production good.
	 */
	public int getProduceGood() {
		return produceGood;
	}

	double getProduceAmount() {
		return produceAmount;
	}

	double getPrice(int good) {
		return price[good];
	}

	Class<? extends ImprovementStrategy> getImprovementStrategy() {
		return improvementStrategy.getClass();
	}
	
	Class<? extends BarterStrategy> getBarterStrategy() {
		return barterStrategy.getClass();
	}

	/**
	 * Returns the price array of this agent.
	 * @return The price array.
	 */
	public double[] getPrices() {
		// TODO: Can we do a defensive copy and yet have the ability
		// to graph the price from the MASON console? (Pelle)
		return price;
		//return price.clone();
	}

	double getDemand(int good) {
		return demand[good];
	}

	double getExchangeFor(int good) {
		return exchangeFor[good];
	}

	double getInventory(int good) {
		return inventory[good];
	}

	int getNumGoods() {
		return price.length;
	}

	/**
	 * Returns the score of this agent.
	 * @return The score.
	 */
	public double getScore() {
		return score;
	}

	/**
	 * Returns a limited view on this agent.
	 * @return The view on this agent.
	 */
	public TradeAgentProxy getView() {
		return myProxy;
	}

	void setConsume(double[] consume) {
		this.consume = consume.clone();
		produceAmount = consume[produceGood] * consume.length;
	}

	/**
	 * Sets the barter strategy for this agent.
	 * @param strategy	The barter strategy.
	 */
	public void setBarterStrategy(BarterStrategy strategy) {
		this.barterStrategy = strategy;
	}

	/**
	 * Sets the improvement strategy for this agent.
	 * @param strategy	The improvement strategy.
	 */
	public void setImprovementStrategy(ImprovementStrategy strategy) {
		this.improvementStrategy = strategy;
	}

	/**
	 * Compares this agent to another one. Returns 1 if this agent has better
	 * score, -1 if the score is lower, or 0 if they are equal.
	 */
	@Override
	public int compareTo(TradeAgent a) {
		return (int) -Math.signum(getScore() - a.getScore());			
	}

	private void readObject(ObjectInputStream s) throws IOException, ClassNotFoundException {
		s.defaultReadObject();

		myProxy = new TradeAgentProxy(this);

		// Check that our invariants are satisfied
		if (!checkInvariants()) {
			throw new InvalidObjectException("Invariants of TradeAgent not satisfied!");
		}
	}
}
