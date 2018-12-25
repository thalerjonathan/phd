package barter;

/**
 * A proxy class for the TradeAgent. Wraps a TradeAgent.
 * 
 * @NotThreadSafe This class is NOT thread safe.
 */
public final class TradeAgentProxy {

	private TradeAgent agent;

	/**
	 * Constructs the proxy by wrapping the given TradeAgent.
	 * @param agent The agent to wrap.
	 */
	public TradeAgentProxy(TradeAgent agent) {
		this.agent = agent;
	}

	/**
	 * Returns the production good of the agent.
	 * @return The production good.
	 */
	public int getProduceGood() {
		return agent.getProduceGood();
	}

	/**
	 * Returns the price vector..
	 * @return The price vector.
	 */
	public double[] getPrices() {
		// TODO: Consider having the array as a field in the proxy and update
		// it from the agent upon read. This would let us pass the proxy to the
		// visualization code and to the console inspector. (Pelle)

		// TODO: Like this... (Pelle)
		// int numGoods = agent.getNumGoods(); 
		// for(int i = 0; i < numGoods; i++) {
		//   myPrices[i] = agent.getPrice(i);
		// }
		// return myPrices;
		return agent.getPrices().clone();
	}

	/**
	 * Returns the price of the given good.
	 * @param good	the good
	 * @return The price of the good.
	 */
	public double getPrice(int good) {
		return agent.getPrice(good);
	}

	/**
	 * Returns the demand of the given good.
	 * @param good	the good
	 * @return The demand of the good.
	 */
	public double getDemand(int good) {
		return agent.getDemand(good);
	}

	/**
	 * Returns the amount that the wrapped agent is willing
	 * to exchange for the given good.
	 * @param good	the good
	 * @return An amount of the agent's production good.
	 */
	public double getExchangeFor(int good) {
		return agent.getExchangeFor(good);
	}

	/**
	 * Returns the amount of the given good that is currently
	 * in the inventory.
	 * @param good	the good
	 * @return The amount of the good in the inventory.
	 */
	public double getInventory(int good) {
		return agent.getInventory(good);
	}

	/**
	 * Returns the score of the agent.
	 * @return The score.
	 */
	public double getScore() {
		return agent.getScore();
	}

	/**
	 * Returns the amount of production good that this agent produces.
	 * @return The produced amount.
	 */
	public double getProduceAmount() {
		return agent.getProduceAmount();
	}

	/**
	 * Returns the improvement strategy class.
	 * @return The improvement strategy.
	 */
	public Class<? extends ImprovementStrategy> getImprovementStrategy() {
		return agent.getImprovementStrategy();
	}

	/**
	 * Returns the barter strategy class.
	 * @return The barter strategy.
	 */
	public Class<? extends BarterStrategy> getBarterStrategy() {
		return agent.getBarterStrategy();
	}
}
