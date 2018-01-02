package HAC.gui;

import HAC.agent.HACAgent;
import agent.ISimulationObserver;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.LinkedHashMap;

/**
 * Created by jonathan on 05/12/16.
 */
public class HACFrontend extends JFrame implements ISimulationObserver<HACAgent> {
    private HACRenderer renderer;

    public HACFrontend(int agentSize) {
        super("Heroes & Cowards Par");

        this.renderer = new HACRenderer(agentSize);

        this.setDefaultCloseOperation( EXIT_ON_CLOSE );
        this.setContentPane( renderer );
        this.setSize( new Dimension( 1000, 1000 ));
        this.setVisible(true);

        this.addKeyListener(new KeyListener() {
            @Override
            public void keyTyped(KeyEvent e) {

            }

            @Override
            public void keyPressed(KeyEvent e) {

            }

            @Override
            public void keyReleased(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_SPACE) {
                    HACFrontend.this.renderer.toggleRenderMode();
                }
            }
        });
    }

    public void render(LinkedHashMap<Integer, HACAgent> am) {
        this.renderer.render(am);
    }

    @Override
    public boolean simulationStep(LinkedHashMap<Integer, HACAgent> am) {
        this.renderer.render(am);
        return true;
    }
}
