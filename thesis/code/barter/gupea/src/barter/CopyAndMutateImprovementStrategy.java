package barter;

import java.util.Arrays;

import ec.util.MersenneTwisterFast;

/**
 * An implementation of the improvement strategy that corresponds to the Gintis'
 * implementation. A fraction  of agents copy the prices of better scoring
 * agents and each price is mutated with a small probability.
 * 
 * @Immutable This class is immutable.
 */
public class CopyAndMutateImprovementStrategy implements ImprovementStrategy {

	private static final long serialVersionUID = -4740157044347265190L;

	private double[] getMutationVector(BarterParams params, MersenneTwisterFast random) {
		double[] priceMutation = new double[params.getNumGoods()];

		Arrays.fill(priceMutation, 1.0);

		for (int good = 0; good < priceMutation.length; good++) {
			if (random.nextDouble() < params.getMutationRate()) {
				if(random.nextBoolean()) {
					priceMutation[good] = params.getMutationDelta();
				} else {
					priceMutation[good] = 1.0 / params.getMutationDelta();
				}
			}
		}

		return priceMutation;
	}

	/**
	 * Copies the price vector from <code>betterAgent</code> to
	 * <code>myself</code> and mutates the prices. 
	 */
	@Override
	public double[] improve(TradeAgentProxy betterAgent, TradeAgentProxy myself,
			BarterParams params, MersenneTwisterFast random) {

		double[] newPrices = new double[params.getNumGoods()];
		double[] priceMutation = getMutationVector(params, random);

		for (int good = 0; good < params.getNumGoods(); good++) {
			newPrices[good] = betterAgent.getPrice(good) * priceMutation[good];
		}

		return newPrices;
	}
}
