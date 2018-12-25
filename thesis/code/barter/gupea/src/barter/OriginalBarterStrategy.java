package barter;

/**
 * An implementation of the barter strategy that corresponds
 * to Gintis' implementation. Agents using this strategy only accept
 * profitable trades. 
 * 
 * @Immutable This class is immutable.
 */
public class OriginalBarterStrategy implements BarterStrategy {

	private static final long serialVersionUID = -8423476125598513620L;

	@Override
	public boolean acceptOffer(TradeAgentProxy me, int inGood,
			double inAmount, double outAmount) {

		return !(me.getDemand(inGood) == 0 ||
				 me.getExchangeFor(inGood) == 0 ||
				 me.getPrice(inGood) * inAmount <
				 me.getPrice(me.getProduceGood()) * outAmount);
	}
}
