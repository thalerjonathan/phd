package barter;

/**
 * An implementation of barter strategy that accepts every trade offer.
 * 
 * @Immutable This class is immutable.
 */
public class GenerousBarterStrategy implements BarterStrategy {

	private static final long serialVersionUID = -1288050823644832611L;

	@Override
	public boolean acceptOffer(TradeAgentProxy me, int offerGood,
			double offerAmount, double wantAmount) {

		// Trade if we do have a demand and we are offered anything at all. 
		return me.getDemand(offerGood) > 0 && me.getExchangeFor(offerGood) > 0;
	}
}