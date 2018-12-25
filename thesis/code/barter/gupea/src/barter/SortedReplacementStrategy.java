package barter;

import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import ec.util.MersenneTwisterFast;

/**
 * An implementation of the replacement strategy that first sorts the
 * agents in two groups, weaker and stronger agents. Then a number of 
 * weaker agents are replaced with the stronger ones.
 * 
 * @Immutable This class is immutable.
 */
public class SortedReplacementStrategy implements ReplacementStrategy {

	private static final long serialVersionUID = 3745267392657323589L;

	@Override
	public void getNextGeneration(List<TradeAgent> producers,
			BarterParams params, MersenneTwisterFast random) {

		int replacements = (int) Math.max(1, Math.round(producers.size() * params.getReplacementRate()));
		if(replacements >= producers.size() / 2) {
			throw new IllegalArgumentException("Replacement rate too high, was " +
					params.getReplacementRate());
		}

		double maxScore = Double.NEGATIVE_INFINITY;
		double minScore = Double.POSITIVE_INFINITY;

		for(TradeAgent e : producers) {
			maxScore = Math.max(maxScore, e.getScore());
			minScore = Math.min(minScore, e.getScore());
		}

		SortedSet<TradeAgent> scoredAgents = new TreeSet<TradeAgent>();
		scoredAgents.addAll(producers);
		Iterator<TradeAgent> setPosition = scoredAgents.iterator(); 

		for(int i = 0; i < scoredAgents.size() / 2; i++) {
			setPosition.next();
		}
		TradeAgent median = setPosition.next();

//		System.out.println("Best: " + maxScore + "\tMedian score: " +
//				median.getScore() + "\tMin score: " + minScore);
		
		SortedSet<TradeAgent> strongerAgentSet = scoredAgents.headSet(median);
		SortedSet<TradeAgent> weakerAgentSet = scoredAgents.tailSet(median);
		weakerAgentSet.add(median);

		TradeAgent[] weakerAgents = weakerAgentSet.toArray(new TradeAgent[0]);
		TradeAgent[] strongerAgents = strongerAgentSet.toArray(new TradeAgent[0]);
		Util.shuffleHead(weakerAgents, replacements, random);
		Util.shuffleHead(strongerAgents, replacements, random);

		for(int i = 0; i < replacements; i++) {
//			System.out.print(weakerAgents[i].getScore() + " <- " + strongerAgents[i].getScore() + "\t");
			weakerAgents[i].improve(strongerAgents[i], params, random);
		}
//		System.out.println("");
	}
}
