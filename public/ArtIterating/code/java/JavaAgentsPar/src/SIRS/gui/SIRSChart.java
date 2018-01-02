package SIRS.gui;

import SIRS.agent.SIRSAgent;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.xy.XYDataItem;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

import javax.swing.*;
import java.awt.*;
import java.text.DecimalFormat;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * Created by jonathan on 13/02/17.
 */
public class SIRSChart extends JPanel {
    private XYSeries susceptibleSeries;
    private XYSeries infectedSeries;
    private XYSeries recoveredSeries;

    private XYSeries meanSeries;
    private XYSeries stdSeries;

    private JLabel meanLabel;
    private JLabel stdLabel;

    private int timeStep;

    private final static DecimalFormat DF = new DecimalFormat("#.##");

    public SIRSChart() {
        JFreeChart sirsChart = createSIRSChart();
        JFreeChart dynamicsChart = createDynamicsChart();

        meanLabel = new JLabel( "Infected Mean: 0.0");
        stdLabel = new JLabel( "Infected Std: 0.0");

        JPanel labelPanel = new JPanel( new GridLayout( 2, 1 ));
        labelPanel.add( meanLabel );
        labelPanel.add( stdLabel );

        this.setLayout( new BorderLayout());
        this.add( new ChartPanel( sirsChart ), BorderLayout.WEST );
        this.add( labelPanel, BorderLayout.CENTER );
        this.add( new ChartPanel( dynamicsChart ), BorderLayout.EAST );
    }

    private JFreeChart createSIRSChart() {
        susceptibleSeries = new XYSeries("Susceptible");
        infectedSeries = new XYSeries("Infected");
        recoveredSeries = new XYSeries("Recovered");

        final XYSeriesCollection dataset = new XYSeriesCollection();
        dataset.addSeries(susceptibleSeries);
        dataset.addSeries(infectedSeries);
        dataset.addSeries(recoveredSeries);

        final JFreeChart chart = ChartFactory.createXYLineChart(
                "SIRS Chart",      // chart title
                "Time-Steps",                      // x axis label
                "Number Of Agents",                      // y axis label
                dataset,                  // data
                PlotOrientation.VERTICAL,
                true,                     // include legend
                true,                     // tooltips
                false                     // urls
        );

        XYPlot plot = chart.getXYPlot();
        plot.setDataset(0, dataset);

        XYLineAndShapeRenderer renderer = new XYLineAndShapeRenderer();
        renderer.setSeriesShapesVisible(0, false);
        renderer.setSeriesShapesVisible(1, false);
        renderer.setSeriesShapesVisible(2, false);

        renderer.setSeriesPaint(0, Color.GREEN);
        renderer.setSeriesPaint(1, Color.RED);
        renderer.setSeriesPaint(2, Color.BLUE);

        plot.setRenderer(0, renderer);

        return chart;
    }

    private JFreeChart createDynamicsChart() {
        meanSeries = new XYSeries("Median Infected");
        stdSeries = new XYSeries("Std Infected");

        final XYSeriesCollection dataset = new XYSeriesCollection();
        dataset.addSeries(meanSeries);
        dataset.addSeries(stdSeries);

        final JFreeChart chart = ChartFactory.createXYLineChart(
                "Dynamics Chart",      // chart title
                "Time-Steps",                      // x axis label
                "Number Of Agents",                      // y axis label
                dataset,                  // data
                PlotOrientation.VERTICAL,
                true,                     // include legend
                true,                     // tooltips
                false                     // urls
        );

        XYPlot plot = chart.getXYPlot();
        plot.setDataset(0, dataset);

        XYLineAndShapeRenderer renderer = new XYLineAndShapeRenderer();
        renderer.setSeriesShapesVisible(0, false);
        renderer.setSeriesShapesVisible(1, false);

        renderer.setSeriesPaint(0, Color.BLUE );
        renderer.setSeriesPaint(1, Color.GREEN);

        plot.setRenderer(0, renderer);

        return chart;
    }

    public void update(LinkedHashMap<Integer, SIRSAgent> as) {
        int s = 0;
        int i = 0;
        int r = 0;

        for ( SIRSAgent a : as.values() ) {
            if (SIRSAgent.SIRSState.Susceptible == a.getState() ) {
                s++;
            } else if ( SIRSAgent.SIRSState.Infected == a.getState()  ) {
                i++;
            } else if ( SIRSAgent.SIRSState.Recovered == a.getState()  ) {
                r++;
            }
        }

        susceptibleSeries.add( timeStep, s );
        infectedSeries.add( timeStep, i );
        recoveredSeries.add( timeStep, r );

        //double sm = mean(susceptibleSeries);
        double im = mean(infectedSeries);
        //double rm = mean(recoveredSeries);

        //double sDev = std(susceptibleSeries, sm);
        double iDev = std(infectedSeries, im);
        //double rDev = std(recoveredSeries, rm);

        meanSeries.add( timeStep, im);
        stdSeries.add( timeStep, iDev);

        meanLabel.setText( "Infected Mean: " + DF.format( im ) );
        stdLabel.setText( "Infected Std: " + DF.format( iDev ) );

        timeStep++;
    }

    private static double mean(XYSeries series) {
        double m = 0;

        List items = series.getItems();
        for ( Object oi : items ) {
            XYDataItem item = (XYDataItem) oi;
            m += item.getYValue();
        }

        m /= (double) series.getItemCount();

        return m;
    }

    private static double std(XYSeries series, double mean) {
        double s = 0;

        List items = series.getItems();
        for ( Object oi : items ) {
            XYDataItem item = (XYDataItem) oi;
            s += Math.pow( item.getYValue() - mean, 2 );
        }

        s /= (double) series.getItemCount();
        s = Math.sqrt( s );

        return s;
    }
}
