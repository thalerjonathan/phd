package barter;

import java.io.Serializable;

/**
 * The interface for a barter strategy.
 */
public interface BarterStrategy extends Serializable {

	/**
	 * Returns <code>true</code> if the agent accepts the trade offer,
	 * false otherwise.
	 * 
	 * @param me			view of the TradeAgent that receives the offer
	 * @param offerGood		the offered good
	 * @param offerAmount	the offered amount
	 * @param wantAmount	the wanted good
	 * @return <code>true</code> if and only if the trade was accepted
	 */
	public boolean acceptOffer(TradeAgentProxy me, int offerGood,
			double offerAmount, double wantAmount);
}
