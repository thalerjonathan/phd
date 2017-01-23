package HAC.gui;

import HAC.agent.HACAgent;
import agent.ISimulationObserver;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.List;

/**
 * Created by jonathan on 05/12/16.
 */
public class HACFrontend extends JFrame implements ISimulationObserver<HACAgent> {
    private HACRenderer renderer;

    public HACFrontend(int agentSize) {
        super("Heroes & Cowards Seq");

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

    @Override
    public boolean simulationStep(List<HACAgent> as) {
        this.renderer.render(as);
        return true;
    }
}
