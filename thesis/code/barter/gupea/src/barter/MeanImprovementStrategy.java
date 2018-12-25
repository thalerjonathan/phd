package barter;

import ec.util.MersenneTwisterFast;

/**
 * An implementation of improvement strategy that gets the new prices
 * by taking the mean of own price and the price of the better agent.
 * 
 * @Immutable This class is immutable.
 */
public class MeanImprovementStrategy implements ImprovementStrategy {

	private static final long serialVersionUID = -8225135430857638711L;

	@Override
	public double[] improve(TradeAgentProxy betterAgent, TradeAgentProxy myself,
			BarterParams params, MersenneTwisterFast random) {

		int numGoods = params.getNumGoods();
		double[] newPrices = new double[numGoods];
		
		for(int good = 0; good < numGoods; good++) {
			newPrices[good] = (betterAgent.getPrice(good) +
					myself.getPrice(good)) * 0.5; 
		}
		
		return newPrices;
	}
}
