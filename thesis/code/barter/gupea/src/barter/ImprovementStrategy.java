package barter;

import java.io.Serializable;

import ec.util.MersenneTwisterFast;

/**
 * The interface for an improvement strategy.
 */
public interface ImprovementStrategy extends Serializable {

	/**
	 * Calculates and returns an improved price vector based on the prices
	 * of a better agent.
	 * 
	 * @param betterAgent	view of the better agent
	 * @param myself		view of the improving agent
	 * @param params		the model parameters
	 * @param random		the random number generator
	 * @return An improved price vector.
	 */
	public double[] improve(TradeAgentProxy betterAgent, TradeAgentProxy myself,
			BarterParams params, MersenneTwisterFast random);
}
