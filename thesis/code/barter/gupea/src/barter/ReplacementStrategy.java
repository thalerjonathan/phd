package barter;

import java.io.Serializable;
import java.util.List;
import ec.util.MersenneTwisterFast;

/**
 * The interface for a replacement strategy.
 */
public interface ReplacementStrategy extends Serializable {

	/**
	 * Replaces or improves some agents to produce the next generation.
	 * 
	 * @param producers		the list of producers of some good
	 * @param params		the model parameters
	 * @param random		the random number generator
	 */
	public void getNextGeneration(List<TradeAgent> producers,
			BarterParams params, MersenneTwisterFast random);
}
