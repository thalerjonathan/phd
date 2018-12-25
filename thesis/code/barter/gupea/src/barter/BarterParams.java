package barter;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;

import sim.util.Interval;

/**
 * Manages all the model parameters.
 * 
 * @ThreadSafe This class is thread safe!
 */
public class BarterParams implements Cloneable, Serializable {

	private static final long serialVersionUID = -4061253009113751786L;

	private int agentsPerGood;
	private int reproducePeriod;
	private double replacementRate;
	private double mutationRate;
	private double mutationDelta;
	private double[] consume;
	private int maxTries;
	private double producerShiftRate;
	private boolean varySupply;
	private boolean checkEfficiency;
	private boolean equiInitialPrices;

	// After how many seconds should graphs be updated
	private double updateFrequency;

	// How frequently we add date to the charts
	private int skipChartFrames;

	// Standard deviation is calculated for this good
	private int stdDevGood;

	// Own good price of an agent is multiplied by this factor
	private double produceGoodPriceFactor;

	// If we are going to quit after a set number of periods
	private boolean shouldQuit;

	// How long until we quit
	private long periodsBeforeQuit;

	private double[] equiPrice;

	// TODO: If the Console would accept calls to set the seed this would not
	//       be needed. Patch MASON? (Pelle)
	/**
	 * A non-zero seed implies that we will always start with that seed.
	 * A zero seed makes us use either the millisec time or the random number generator
	 * in the state it was set before calling <code>start</code>.
	 */
	private long seed;

	public BarterParams() {
		agentsPerGood = 100;
		reproducePeriod = 10;
		replacementRate = 0.05;
		mutationRate = 0.1;
		mutationDelta = 0.95;
		consume = new double[] { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 };
		maxTries = 5;
		producerShiftRate = 0.01;
		varySupply = false;
		checkEfficiency = false;
		equiInitialPrices = false;

		stdDevGood = 0;
		produceGoodPriceFactor = 0.8;
		updateFrequency = 2;
		skipChartFrames = 100;

		shouldQuit = false;
		seed = 0;
		calcEquiPrice();
	}

	private void calcEquiPrice() {
		equiPrice = new double[consume.length];
		for (int i = 0; i < consume.length; i++) {
			equiPrice[i] = consume[getPriceUnitGood()] / consume[i];
		}
	}

	/**
	 * Returns the number of chart frames that is skipped from drawing.
	 * 
	 * @return The number of skipped chart frames.
	 */
	public int getSkipChartFrames() {
		return skipChartFrames;
	}

	/**
	 * Returns the chart update frequency in seconds.
	 *  
	 * @return The chart update frequency.
	 */
	public synchronized double getUpdateFrequency() {
		return updateFrequency;
	}

	/**
	 * Returns the maximum number of tries that each agent makes before
	 * giving up acquiring a particular good.
	 * 
	 * @return The maximum trade attempts for each good.
	 */
	public int getMaxTries() {
		return maxTries;
	}

	/**
	 * Returns the mutation rate of prices. Each price of each replaced agent
	 * is mutated with this probability (range [0..1]).
	 * 
	 * @return The mutation rate.
	 */
	public synchronized double getMutationRate() {
		return mutationRate;
	}

	/**
	 * Returns the slider range for mutationRate.
	 * @return The range of mutation rate.
	 */
	public Object domMutationRate() {
		return new Interval(0.0, 1.0);
	}

	/**
	 * Returns the mutation delta. Each mutated price is either divided or
	 * multiplied by this value (with equal probability).
	 * 
	 * @return The mutation delta.
	 */
	public synchronized double getMutationDelta() {
		return mutationDelta;
	}

	/**
	 * Returns the number of periods that the model should run before
	 * exiting the program.
	 * 
	 * @return The number of periods before quit.
	 */
	public synchronized long _getPeriodsBeforeQuit() {
		return periodsBeforeQuit;
	}

	/**
	 * Returns true the model should be stopped after
	 * {@link #_getPeriodsBeforeQuit() some} period.
	 * 
	 * @return True if model is stopped after a certain time.
	 */
	public boolean _isShouldQuit() {
		return shouldQuit;
	}

	/**
	 * Returns the number of periods that should be stepped before each
	 * reproduction period. Thus, if <code>currentPeriod % reproducePeriod == 0</code>
	 * then it is reproduction takes place.
	 * 
	 * @return The number of periods skipped before each reproduction period.
	 */
	public int getReproducePeriod() {
		return reproducePeriod;
	}

	/**
	 * Returns the replacement rate of agents. At each reproduction period,
	 * <code>replacementRate * agentsPerGood</code> agents per production good
	 * are replaced with better ones.
	 * 
	 * @return The replacement rate.
	 */
	public synchronized double getReplacementRate() {
		return replacementRate;
	}

	/**
	 * Returns the slider range for replacementRate.
	 * @return The range of replacement rate.
	 */
	public Object domReplacementRate() {
		return new Interval(0.0, 1.0);
	}

	/**
	 * Returns the good that is used to calculate standard deviations in
	 * {@link BarterEconomy#getConsumerPriceStdDev()} and
	 * {@link BarterEconomy#getProducerPriceStdDev()} and
	 * {@link BarterEconomy#getMeanPriceStdDev()}.
	 * 
	 * @return The good for standard deviation calculations.
	 */
	public int getStdDevGood() {
		return stdDevGood;
	}

	/**
	 * Returns the price unit good. Every agents takes the price of this good
	 * as the unit and express the prices of other goods relative to this.
	 * 
	 * @return The price unit good.
	 */
	public synchronized int getPriceUnitGood() {
		return 	consume.length - 1;
	}

	/**
	 * Returns the equilibrium prices for all goods.
	 * 
	 * @return The equilibrium prices.
	 */
	public synchronized double[] getEquiPrice() {
		return equiPrice.clone();
	}

	/**
	 * Returns the number of goods in the economy. Equal to the lenght of
	 * consume array.
	 * 
	 * @return The number of goods in the economy.
	 */
	public synchronized int getNumGoods() {
		return consume.length;
	}

	/**
	 * Returns the rate by which the agents shift their production good when
	 * <code>varySupply</code> is enabled. Range [0..1].
	 * 
	 * @return The producer shift rate.
	 */
	public synchronized double getProducerShiftRate() {
		return producerShiftRate;
	}

	/**
	 * Returns the slider range for producerShiftRate.
	 * @return The range of producer shift rate.
	 */
	public Object domProducerShiftRate() {
		return new Interval(0.0, 1.0);
	}

	/**
	 * Returns the number of agents that are created per production good.
	 * 
	 * @return The number of agents per good.
	 */
	public int getAgentsPerGood() {
		return agentsPerGood;
	}

	/**
	 * Returns <code>true</code> if agents are allowed shift production goods.
	 * 
	 * @return <code>true</code> if agents shift production good,
	 * 	<code>false</code> otherwise.
	 */
	public boolean isVarySupply() {
		return varySupply;
	}

	/**
	 * Returns <code>true</code> if the efficiency of the economy is tested.
	 * The mutation is disabled while checkEfficiency is enabled.
	 * 
	 * @return <code>true</code> if efficiency is checked, <code>false</code>
	 * 	otherwise.
	 */
	public boolean isCheckEfficiency() {
		return checkEfficiency;
	}

	/**
	 * Returns <code>true</code> if the economy is started with equilibrium
	 * prices.
	 * 
	 * @return <code>true</code> if initial price are the equilibrium prices,
	 *  <code>false</code> otherwise.
	 */
	public boolean isEquiInitialPrices() {
		return equiInitialPrices;
	}

	/**
	 * Returns the number of total agents in the economy.
	 * 
	 * @return The total number of agents.
	 */
	public int getTotalAgents() {
		return agentsPerGood * getNumGoods();
	}

	/**
	 * Returns the factor by which the initial price of each agent's
	 * production good is multiplied.
	 * 
	 * @return The produce good price factor.
	 */
	public synchronized double getProduceGoodPriceFactor() {
		return produceGoodPriceFactor;
	}

	synchronized double[] getConsumeArray() {
		return consume.clone();
	}

	/**
	 * Returns the String representation of the consume array.
	 * Used by MASON.
	 * 
	 * @return The String representation of the consume array.
	 */
	public synchronized String getConsume() {
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < consume.length - 1; i++) {
			sb.append(consume[i] + ", ");
		}
		sb.append(consume[consume.length - 1]);
		return sb.toString();
	}

	/**
	 * Sets the consume array by extracting the values from the given String.
	 * The format is doubles separated by commas (e.g. "1.0,2.0,3.0").
	 * 
	 * @param consume	The consume array as a string.
	 */
	public synchronized void setConsume(String consume) {
		String[] arr = consume.split(",");

		this.consume = new double[arr.length];
		for (int i = 0; i < arr.length; i++) {
			this.consume[i] = new Double(arr[i]);
		}
		calcEquiPrice();
	}

	/**
	 * Sets the consume array by a List of String values, where each String
	 * is expected to contain a double value.
	 * 
	 * @param consume	The consume array as a string.
	 */
	public synchronized void setConsume(List<String> consume) {
		this.consume = new double[consume.size()];

		for (int i = 0; i < consume.size(); i++) {
			this.consume[i] = new Double(consume.get(i));
		}
		calcEquiPrice();
	}

	/**
	 * Sets the consume to a given consume array.
	 * 
	 * @param consume The consume array.
	 */
	public synchronized void setConsume(double[] consume) {
		this.consume = consume.clone();
		calcEquiPrice();
	}

	/**
	 * Sets the number of agents per good. See {@link #getAgentsPerGood()}.
	 * @param agentsPerGood	The number of agents per good.
	 */
	public void setAgentsPerGood(int agentsPerGood) {
		this.agentsPerGood = agentsPerGood;
	}

	/**
	 * Sets the reproduce period. See {@link #getReproducePeriod()}.
	 * @param reproducePeriod	The reproduce period.
	 */
	public void setReproducePeriod(int reproducePeriod) {
		this.reproducePeriod = reproducePeriod;
	}

	/**
	 * Sets the replacement rate. See {@link #getReplacementRate()}.
	 * @param replacementRate The replacement rate.
	 */
	public synchronized void setReplacementRate(double replacementRate) {
		this.replacementRate = replacementRate;
	}

	/**
	 * Sets the mutation rate. See {@link #getMutationRate()}.
	 * @param mutationRate The mutation rate.
	 */
	public synchronized void setMutationRate(double mutationRate) {
		this.mutationRate = mutationRate;
	}

	/**
	 * Sets the mutation delta. See {@link #getMutationDelta()}.
	 * @param mutationDelta The mutation delta.
	 */
	public synchronized void setMutationDelta(double mutationDelta) {
		this.mutationDelta = mutationDelta;
	}

	/**
	 * Sets the maximum trade attempts. See {@link #getMaxTries()}.
	 * @param maxTries The maximum number of tries.
	 */
	public void setMaxTries(int maxTries) {
		this.maxTries = maxTries;
	}

	/**
	 * Sets the producer shift rate. See {@link #getProducerShiftRate()}.
	 * @param producerShiftRate The producer shift rate.
	 */
	public synchronized void setProducerShiftRate(double producerShiftRate) {
		this.producerShiftRate = producerShiftRate;
	}

	/**
	 * Sets the flag for the agents to shift their production good.
	 * See {@link #isVarySupply()}.
	 * @param varySupply The flag for varying supply.
	 */
	public void setVarySupply(boolean varySupply) {
		this.varySupply = varySupply;
	}

	/**
	 * Sets the flag for checking the efficiency of the economy.
	 * See {@link #isCheckEfficiency()}.
	 * @param checkEfficiency	The flag for checking efficiency.
	 */
	public void setCheckEfficiency(boolean checkEfficiency) {
		this.checkEfficiency = checkEfficiency;
	}

	/**
	 * Sets the standard deviation good. See {@link #getStdDevGood()}.
	 * @param stdDevGood The standard deviation good.
	 */
	public void setStdDevGood(int stdDevGood) {
		this.stdDevGood = stdDevGood;
	}

	/**
	 * Sets the flag for equilibrium initial prices.
	 * See {@link #isEquiInitialPrices()}.
	 * @param equiInitialPrices	The flag for equilibrium initial prices.
	 */
	public void setEquiInitialPrices(boolean equiInitialPrices) {
		this.equiInitialPrices = equiInitialPrices;
	}

	/**
	 * Sets the chart update frequency. See {@link #getUpdateFrequency()}.
	 * @param updateFrequency	The chart update frequency.
	 */
	public synchronized void setUpdateFrequency(double updateFrequency) {
		this.updateFrequency = updateFrequency;
	}

	/**
	 * Sets the number of skipped chart frames. See {@link #getSkipChartFrames()}.
	 * @param skipChartFrames The number of skipped chart frames.
	 */
	public void setSkipChartFrames(int skipChartFrames) {
		this.skipChartFrames = skipChartFrames;
	}

	/**
	 * Sets the price factor for production goods.
	 * See {@link #getProduceGoodPriceFactor()}.
	 * @param produceGoodPriceFactor The price factor for production goods.
	 */
	public synchronized void setProduceGoodPriceFactor(double produceGoodPriceFactor) {
		this.produceGoodPriceFactor = produceGoodPriceFactor;
	}

	/**
	 * Sets the flag for quitting the program. See {@link #_isShouldQuit()}.
	 * @param shouldQuit The flag for quitting the program.
	 */
	public void _setShouldQuit(boolean shouldQuit) {
		this.shouldQuit = shouldQuit;
	}

	/**
	 * Sets the number of periods before the program quits.
	 * See {@link #_getPeriodsBeforeQuit()}.
	 * @param periodsBeforeQuit	The number of periods before quit.
	 */
	public synchronized void _setPeriodsBeforeQuit(long periodsBeforeQuit) {
		this.periodsBeforeQuit = periodsBeforeQuit;
	}

	/**
	 * Returns the seed used in the random number generator.
	 * A non-zero seed implies that we will always start with that seed. A zero
	 * seed makes us use either the millisec time or the random number generator
	 * in the state it was set before calling <code>start</code>.
	 * @return The seed.
	 */
	public synchronized long _getSeed() {
		return seed;
	}

	/**
	 * Sets the seed. See {@link #_getSeed()}.
	 * @param seed The seed.
	 */
	public synchronized void _setSeed(long seed) {
		this.seed = seed;
	}

	/**
	 * Compares <code>this</code> to the argument and returns true
	 * if they are equal. Two instances of <code>BarterParams</code> are
	 * not equal if any parameter that can be changed during a simulation
	 * is different. Other parameters are not compared.
	 */
	@Override
	public synchronized boolean equals(Object o) {
		BarterParams other = (BarterParams) o;
		if (o == null || !(o instanceof BarterParams) ||
			!Arrays.equals(consume, other.getConsumeArray())
			|| agentsPerGood != other.getAgentsPerGood()
			|| replacementRate != other.getReplacementRate()
			|| mutationRate != other.getMutationRate()
			|| mutationDelta != other.getMutationDelta()
			|| maxTries != other.getMaxTries()
			|| producerShiftRate != other.getProducerShiftRate()
			|| varySupply != other.isVarySupply()
			|| checkEfficiency != other.isCheckEfficiency()
			|| stdDevGood != other.getStdDevGood()
			|| skipChartFrames != other.getSkipChartFrames()
			|| updateFrequency != other.getUpdateFrequency()) {

			return false;
		}

		return true;
	}

	/**
	 * Clones this <code>BarterParams</code> object.
	 */
	public synchronized BarterParams clone() {
		try {
			return (BarterParams) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
			return null;
		}
	}
}