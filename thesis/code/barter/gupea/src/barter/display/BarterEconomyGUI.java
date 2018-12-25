package barter.display;

import java.awt.Color;
import java.awt.Frame;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import org.evensen.util.Pair;
import org.jfree.data.xy.XYSeries;

import sim.display.Console;
import sim.display.Controller;
import sim.display.Display2D;
import sim.display.GUIState;
import sim.engine.Schedule;
import sim.engine.SimState;
import sim.engine.Steppable;
import sim.util.media.chart.TimeSeriesChartGenerator;
import barter.BarterEconomy;
import barter.BarterParams;

/**
 * The class responsible for the GUI of the barter economy.
 * 
 * @NotThreadSafe This class is NOT thread safe!
 */
public class BarterEconomyGUI extends GUIState {

	private BarterParams params;

	private Display2D display;
	private JFrame displayFrame;

	private TimeSeriesChartGenerator stdDevChart;
	private TimeSeriesChartGenerator avgRelPriceChart;
	private TimeSeriesChartGenerator avgScoreChart;
	private TimeSeriesChartGenerator populationChart;
	private XYSeries[] stdDevSeries;
	private XYSeries[] avgRelPriceSeries;
	private XYSeries[] avgScoreSeries;
	private XYSeries[] populationSeries;
	private Thread timer = null;

	// TODO: Remove this flag. Only for timing.
	private static final boolean SHOULD_CHART = true;
	private static final boolean SHOULD_LOG = false;

	/**
	 * The default constructor called by MASON if the simulation is
	 * started from the File menu in the Console.
	 */
	public BarterEconomyGUI() {
		this(new BarterEconomy());
	}

	/**
	 * Creates the GUI for the given barter economy.
	 * @param barterEconomy	the barter economy
	 */
	public BarterEconomyGUI(BarterEconomy barterEconomy) {
		super(barterEconomy);
		params = barterEconomy.getParams();
	}

	/**
	 * Initiates the GUI. Called from MASON.
	 */
	public void init(Controller c) {
		super.init(c);
		c.registerFrame(createStdDevChart());
		c.registerFrame(createAvgRelPriceChart());
		c.registerFrame(createAvgScoreChart());
		c.registerFrame(createPopulationChart());

		display = new Display2D(500, 400, this,11);
		display.setBackdrop(Color.black);

		displayFrame = display.createFrame();
		displayFrame.setTitle("Agent Visualisation");
//		displayFrame.setVisible(true);
//		displayFrame.pack();
//	    displayFrame.setVisible(false);
		c.registerFrame(displayFrame);
	}

	private JFrame createStdDevChart() {
		Pair<TimeSeriesChartGenerator,JFrame> chart = 
			createTimeSeriesFrame("Standard Deviation", "Standard Deviation", "Period");

		stdDevChart = chart.first;

		return chart.second;
	}

	private JFrame createAvgRelPriceChart() {
		Pair<TimeSeriesChartGenerator,JFrame> chart = 
			createTimeSeriesFrame("Average Price",
					"% of The Equilibrium Price", "Period");

		avgRelPriceChart = chart.first;

		return chart.second;
	}

	private JFrame createAvgScoreChart() {
		Pair<TimeSeriesChartGenerator,JFrame> chart =
			createTimeSeriesFrame("Average Score", "Avgerage Score", "Period");

		avgScoreChart = chart.first;

		return chart.second;
	}

	private JFrame createPopulationChart() {
		Pair<TimeSeriesChartGenerator,JFrame> chart = 
			createTimeSeriesFrame("Producer Shares", "Producer Share", "Period");

		populationChart = chart.first;

		return chart.second;
	}

	private Pair<TimeSeriesChartGenerator,JFrame> createTimeSeriesFrame(String title,
			String rangeLabel, String domainLabel) {
		JFrame chartFrame;
		TimeSeriesChartGenerator chart;

		chart = new TimeSeriesChartGenerator();
		chart.setTitle(title);
		chart.setRangeAxisLabel(rangeLabel);
		chart.setDomainAxisLabel(domainLabel);

		chartFrame = chart.createFrame(this);
		chartFrame.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);

		return new Pair<TimeSeriesChartGenerator,JFrame>(chart, chartFrame);
	}

	/**
	 * Initiates the simulation.
	 */
	@Override
	public void start() {
		// maybe some params were modified before play was pressed
		((BarterEconomy) state).setParams(params);
		super.start();

		display.detatchAll();
		BarterPortrayalSetup.setupPortrayals(super.state, display);

		initStdDevSeries();
		initAvgRelPriceSeries();
		initAvgScoreSeries();
		initPopulationSeries();
		initParamsMonitor();
	}

	private void startTimer(final long milliSeconds) {
		if(timer == null) {
			timer = sim.util.Utilities.doLater(milliSeconds, new Runnable() {
				public void run() {
					if(stdDevChart != null) {
						stdDevChart.update();
					}
					if(avgRelPriceChart != null) {
						avgRelPriceChart.update();
					}
					if(avgScoreChart != null) {
						avgScoreChart.update();
					}
					if(populationChart != null) {
						populationChart.update();
					}
					timer = null;
				}
			});
		}
	}

	private void initStdDevSeries() {
		stdDevChart.removeAllSeries();
		stdDevSeries = new XYSeries[3];
		stdDevSeries[0] = new XYSeries("Producer Price", false);
		stdDevSeries[1] = new XYSeries("Consumer Price", false);
		stdDevSeries[2] = new XYSeries("Mean Price", false);

		stdDevChart.addSeries(stdDevSeries[0], null);
		stdDevChart.addSeries(stdDevSeries[1], null);
		stdDevChart.addSeries(stdDevSeries[2], null);

		scheduleImmediateRepeat(true, new Steppable() {
			@Override
			public void step(SimState state) {
				double period = state.schedule.time();
				startTimer((int) (params.getUpdateFrequency() * 1000.0));

				if(SHOULD_CHART) {
					if (period >= Schedule.EPOCH && period < Schedule.AFTER_SIMULATION &&
							period % params.getSkipChartFrames() == 0) {

						BarterEconomy be = (BarterEconomy) state;
						stdDevSeries[0].add(period, be.getProducerPriceStdDev(), false);
						stdDevSeries[1].add(period, be.getConsumerPriceStdDev(), false);
						stdDevSeries[2].add(period, be.getMeanPriceStdDev(), false);
					}
				}
			}
			private static final long serialVersionUID = -4996696249867007516L;
		});
	}

	private void initAvgRelPriceSeries() {
		avgRelPriceChart.removeAllSeries();
		avgRelPriceSeries = new XYSeries[modelParams().getNumGoods()];

		for (int good = 0; good < modelParams().getNumGoods(); good++) {
			avgRelPriceSeries[good] = new XYSeries("Good #" + good, false);
			avgRelPriceChart.addSeries(avgRelPriceSeries[good], null);
		}

		// TODO: merge three different chart updates into one steppable
		scheduleImmediateRepeat(true, new Steppable() {
			@Override
			public void step(SimState state) {
				double period = state.schedule.time();
				startTimer((int) (params.getUpdateFrequency() * 1000.0));

				if (period >= Schedule.EPOCH && period < Schedule.AFTER_SIMULATION &&
						period % params.getSkipChartFrames() == 0) {

					BarterEconomy be = (BarterEconomy) state;
					logState(be);
					if(SHOULD_CHART) {
						double[] prices = be.getAvgRelPrice();
						for (int good = 0; good < prices.length; good++) {
							// * 100 to get percentage
							avgRelPriceSeries[good].add(period, prices[good] * 100, false);
						}
					}
				}
			}
			private static final long serialVersionUID = -2411284194729429063L;
		});
	}

	private void initAvgScoreSeries() {
		avgScoreChart.removeAllSeries();
		avgScoreSeries = new XYSeries[modelParams().getNumGoods() + 1];

		for (int good = 0; good < modelParams().getNumGoods(); good++) {
			avgScoreSeries[good] = new XYSeries("Good #" + good, false);
			avgScoreChart.addSeries(avgScoreSeries[good], null);
		}
		avgScoreSeries[modelParams().getNumGoods()] = new XYSeries("Average", false);
		avgScoreChart.addSeries(avgScoreSeries[modelParams().getNumGoods()], null);

		scheduleImmediateRepeat(true, new Steppable() {
			@Override
			public void step(SimState state) {
				double period = state.schedule.time();
				startTimer((int) (params.getUpdateFrequency() * 1000.0));

				if(SHOULD_CHART) {
					if (period >= Schedule.EPOCH && period < Schedule.AFTER_SIMULATION && 
							period % params.getSkipChartFrames() == 0) {
						// TODO: Increase getSkipChartFrames? (Pelle)

						BarterEconomy be = (BarterEconomy) state;
						double[] scores = be.getAvgScore();
						for (int good = 0; good < scores.length; good++) {
							avgScoreSeries[good].add(period, scores[good]
							                                        / modelParams().getReproducePeriod()
							                                        / be.getCommonPriceScore(), false);
						}

						avgScoreSeries[scores.length].add(period, be.getTotalAvgScore()
								/ modelParams().getReproducePeriod()
								/ be.getCommonPriceScore(), false);
						// Some data reduction would maybe be appropriate here.
					}
				}
			}
			private static final long serialVersionUID = 1869903530277103205L;
		});
	}

	private void initPopulationSeries() {
		populationChart.removeAllSeries();
		populationSeries = new XYSeries[modelParams().getNumGoods()];

		for (int good = 0; good < modelParams().getNumGoods(); good++) {
			populationSeries[good] = new XYSeries("Share of Good #" + good,
					false);
			populationChart.addSeries(populationSeries[good], null);
		}

		scheduleImmediateRepeat(true, new Steppable() {
			@Override
			public void step(SimState state) {
				double period = state.schedule.time();
				startTimer((int) (params.getUpdateFrequency() * 1000.0));

				if(SHOULD_CHART) {
					if (period >= Schedule.EPOCH && period < Schedule.AFTER_SIMULATION &&
							period % params.getSkipChartFrames() == 0) {

						BarterEconomy be = (BarterEconomy) state;
						for (int good = 0; good < modelParams().getNumGoods(); good++) {
							double share = (double) be.getProducers(good).size()
							/ modelParams().getTotalAgents();
							populationSeries[good].add(period, share * 100, false);
						}
					}
				}
			}
			private static final long serialVersionUID = -3579162485552952960L;
		});
	}

	/**
	 * Returns a BarterParams object, which MASON uses to create a GUI.
	 */
	@Override
	public Object getSimulationInspectedObject() {
		return params;
	}

	// TODO: Make a "main-substitute" returning all base parameters and number of steps till equilibrium.
	// numGoods-numAgents-stoppedAfter-reproducePeriod-replacementRate-mutationRateconsume-
	// maxTries-prodShiftRate-varSupply-checkEff-equiInit-seed.log
	private void logState(BarterEconomy be) {
		if(SHOULD_LOG && be.schedule.time() > 0) {
			System.out.print(be.schedule.time() + " ");

			for (double relPrice : be.getAvgRelPrice()) {
				System.out.print(relPrice + " ");
			}

			for (double score : be.getAvgScore()) {
				System.out.print(score / modelParams().getReproducePeriod()
						/ be.getCommonPriceScore() + " ");
			}

			int numGoods = be.getAvgScore().length;
			for (int good = 0; good < numGoods; good++) {
				System.out.print((double) be.getProducers(good).size()
						/ modelParams().getTotalAgents() + " ");
			}
			System.out.println();
		}
	}

	private BarterParams modelParams() {
		return ((BarterEconomy) state).getParams();
	}

	private void initParamsMonitor() {
		scheduleImmediateRepeat(true, new Steppable() {
			@Override
			public void step(SimState state) {
				BarterEconomy be = (BarterEconomy) state;
				if (!params.equals(be.getParams())) {
					be.setParams(params.clone());
				}
			}
			private static final long serialVersionUID = 1466228111947016532L;
		});
	}

	/**
	 * Called by MASON when the program quits.
	 */
	@Override
	public void quit() {
		super.quit();
		if (displayFrame != null) {
			displayFrame.dispose();
		}
		displayFrame = null;
		display = null;
	}

	/**
	 * Creates the GUI and sets it visible.
	 */
	public void create() {
		final Console c = new Console(this);
		c.setVisible(true);

		if(params._isShouldQuit()) {
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					c.pressPlay();
				}
			});
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					c.setExtendedState(Frame.ICONIFIED);
				}
			});
		}
	}

	/**
	 * Called by MASON when a simulation state is loaded from file.
	 */
	@Override
	public void load(SimState state) {
		/*If a checkpoint is loaded and resumed, the start() method is not called
		  and thus we need to initialize the GUI elements here. */
	 	super.load(state);

		params = ((BarterEconomy) state).getParams();

		display.detatchAll();
		BarterPortrayalSetup.setupPortrayals(super.state, display);

		initStdDevSeries();
		initAvgRelPriceSeries();
		initAvgScoreSeries();
		initPopulationSeries();
		initParamsMonitor();
	}
}