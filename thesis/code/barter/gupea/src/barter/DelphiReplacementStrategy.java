package barter;

import java.util.List;

import ec.util.MersenneTwisterFast;

/**
 * An implementation of the replacement strategy that corresponds
 * to the algorithm described in Gintis' paper. Picks two random agents
 * and replaces the worse one with the better.
 * 
 * @Immutable This class is immutable.
 */
public class DelphiReplacementStrategy implements ReplacementStrategy {

	private static final long serialVersionUID = 1805372382499089433L;

	/**
	 * Picks {@link BarterParams#getReplacementRate()} * <code>producers.size()</code>
	 * agents to replace and gives the lower scoring agents a chance to improve
	 * based on the better agent.
	 */
	@Override
	public void getNextGeneration(List<TradeAgent> producers,
			BarterParams params, MersenneTwisterFast random) {

		long replacements = Math.round(producers.size()	* params.getReplacementRate());

		replacements = Math.max(replacements, 1);

		for (int i = 0; i < replacements; i++) {
			int j = random.nextInt(producers.size());
			int k = random.nextInt(producers.size());

			if (producers.get(j).getScore() > producers.get(k).getScore()) {
				producers.get(k).improve(producers.get(j), params, random);
			} else {
				producers.get(j).improve(producers.get(k), params, random);
			}
		}
	}
}
