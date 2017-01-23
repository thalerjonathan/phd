package SIRS.gui;

import SIRS.agent.SIRSAgent;
import agent.ISimulationObserver;

import javax.swing.*;
import java.awt.*;
import java.util.LinkedHashMap;

/**
 * Created by jonathan on 05/12/16.
 */
public class SIRSFrontend extends JFrame implements ISimulationObserver<SIRSAgent> {
    private SIRSRenderer renderer;

    public SIRSFrontend(int cols, int rows) {
        super("SIRS Conc");

        this.renderer = new SIRSRenderer(cols, rows);

        this.setDefaultCloseOperation( EXIT_ON_CLOSE );
        this.setContentPane( renderer );
        this.setSize( new Dimension( 1000, 1000 ));
        this.setVisible(true);
    }

    @Override
    public boolean simulationStep(LinkedHashMap<Integer,SIRSAgent> as) {
        this.renderer.render(as);

        try {
            Thread.sleep( 30 );
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        return true;
    }
}
