package barter;

import java.util.BitSet;
import java.util.List;

import ec.util.MersenneTwisterFast;

/**
 * An implementation of the replacement strategy that corresponds to
 * Gintis' description of the replacement procedure. Agents are replaced
 * based on their "fitness".
 * 
 * @Immutable This class is immutable.
 */
public class OriginalReplacementStrategy implements ReplacementStrategy {

	private static final long serialVersionUID = -7803986167559620008L;

	@Override
	public void getNextGeneration(List<TradeAgent> producers,
			BarterParams params, MersenneTwisterFast random) {
		// From Gintis' price system paper, section 3.3.
		// 		The Score of all agents of a particular type are
		//		normalized proportionally so that the lowest becomes zero and the highest becomes
		//		one.

		if(params.getReplacementRate() >= 0.5) {
			throw new IllegalArgumentException("Replacement rate too high (can not be higher than 0.5, was " +
					params.getReplacementRate() + ")");
		}

		double maxScore = Double.NEGATIVE_INFINITY;
		double minScore = Double.POSITIVE_INFINITY;

		for(TradeAgent e : producers) {
			maxScore = Math.max(maxScore, e.getScore());
			minScore = Math.min(minScore, e.getScore());
		}

		double delta = maxScore - minScore;

		long shouldReplace = Math.max(1, Math.round(producers.size() * params.getReplacementRate()));
		int n = producers.size();
		BitSet worsePerforming = new BitSet(n);
		double ms = 0.0;

		//      Agents of this type are then repeatedly randomly addressed, and with probability
		//		one minus the agent's Score, are designated for replacement. This continues
		//		until enough replacements have been found.
		while(worsePerforming.cardinality() < shouldReplace) {
			int aIdx = random.nextInt(n);
			// Is this agent already among the worse agents? 
			if(!worsePerforming.get(aIdx)) {
				double prob = (producers.get(aIdx).getScore() - minScore) / delta;
				if(prob < random.nextDouble()) {
					worsePerforming.set(aIdx);
					ms += prob;
				}
			}
		}

		BitSet betterPerforming = new BitSet(n);
		ms = 0;
		//      Then, agents who are not to be replaced
		//		are randomly addressed and an agent serves as a replacement with a probability
		//		equal to its Score.
		//      Agents of this type are then repeatedly randomly addressed, and with probability
		//		one minus the agent's Score, are designated for replacement. This continues
		//		until enough replacements have been found.
		while(betterPerforming.cardinality() < shouldReplace) {
			int aIdx = random.nextInt(n);
			// Is this agent already among the better agents? 
			if(!worsePerforming.get(aIdx) && !betterPerforming.get(aIdx)) {
				double prob = (producers.get(aIdx).getScore() - minScore) / delta;
				if(prob > random.nextDouble()) {
					ms += prob;
					betterPerforming.set(aIdx);
				}
			}
		}

		//      Each agent to be replaced is then paired up with a replacement,
		//		and the former copies the latter's private prices.
		while(worsePerforming.cardinality() > 0) {
			int worseIdx = worsePerforming.nextSetBit(random.nextInt(worsePerforming.length()));
			int betterIdx = betterPerforming.nextSetBit(random.nextInt(betterPerforming.length()));
			producers.get(worseIdx).improve(producers.get(betterIdx), params, random);
			worsePerforming.clear(worseIdx);
			betterPerforming.clear(betterIdx);
		}
	}
}
