package SIRS.gui;

import SIRS.agent.SIRSAgent;
import agent.ISimulationObserver;

import javax.swing.*;
import java.awt.*;
import java.util.List;

/**
 * Created by jonathan on 05/12/16.
 */
public class SIRSFrontend extends JFrame implements ISimulationObserver<SIRSAgent> {
    private SIRSRenderer renderer;
    private SIRSChart chart;

    public SIRSFrontend(int cols, int rows) {
        super("SIRS Seq Immediate");

        this.renderer = new SIRSRenderer(cols, rows);
        this.chart = new SIRSChart();

        JPanel contentPane = new JPanel();
        contentPane.setLayout(new BorderLayout());
        contentPane.add( renderer, BorderLayout.CENTER);
        contentPane.add( chart, BorderLayout.SOUTH);

        this.setDefaultCloseOperation( EXIT_ON_CLOSE );
        this.setContentPane( contentPane );
        this.setSize( new Dimension( 1920, 1080 ));

        this.setVisible(true);
    }

    @Override
    public boolean simulationStep(List<SIRSAgent> as) {
        this.renderer.render(as);
        this.chart.update(as);

        try {
            Thread.sleep( 10 );
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        return true;
    }
}
