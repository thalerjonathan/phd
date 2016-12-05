package hac.gui;

import hac.backend.agent.Agent;
import hac.backend.simulation.ISimulationObserver;
import hac.backend.simulation.WorldType;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.List;

/**
 * Created by jonathan on 05/12/16.
 */
public class HACFrontend extends JFrame implements ISimulationObserver, WindowListener {
    private HACRenderer renderer = new HACRenderer();
    private boolean continueSimulation;
    private long lastUpdate;

    public HACFrontend() {
        super("Heroes & Cowards");

        this.continueSimulation = true;
        this.lastUpdate = System.currentTimeMillis();

        this.addWindowListener( this );
        this.setContentPane( renderer );
        this.setSize( new Dimension( 1920, 1000 ));
        this.setVisible(true);
    }

    @Override
    public double startSimulation() {
        this.lastUpdate = System.currentTimeMillis();
        return 0;
    }

    @Override
    public double getDt() {
        long curr = System.currentTimeMillis();
        return (double) (curr - this.lastUpdate) / 1000.0;
    }

    @Override
    public boolean simulationStep(List<Agent> as, WorldType wt) {
        this.renderer.render(as, wt);
        this.lastUpdate = System.currentTimeMillis();
        return this.continueSimulation;
    }

    @Override
    public void windowOpened(WindowEvent e) {

    }

    @Override
    public void windowClosing(WindowEvent e) {
        this.continueSimulation = false;
    }

    @Override
    public void windowClosed(WindowEvent e) {
    }

    @Override
    public void windowIconified(WindowEvent e) {

    }

    @Override
    public void windowDeiconified(WindowEvent e) {

    }

    @Override
    public void windowActivated(WindowEvent e) {

    }

    @Override
    public void windowDeactivated(WindowEvent e) {

    }
}
