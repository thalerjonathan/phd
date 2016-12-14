package hac.gui;

import hac.backend.agent.Agent;
import hac.backend.simulation.WorldType;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.*;
import java.util.List;

/**
 * Created by jonathan on 05/12/16.
 */
public class HACRenderer extends JPanel {
    private List<Agent> as;

    private final static int BORDER_X = 10;
    private final static int BORDER_Y = 10;

    private int agentSize;

    private RenderMode rm;

    public HACRenderer( int agentSize ) {
        this.agentSize = agentSize;
        this.as = new ArrayList<>();
        this.rm = RenderMode.ALL;
    }

    public void toggleRenderMode() {
        int nextRmIdx = (HACRenderer.this.rm.ordinal() + 1) % RenderMode.values().length;
        HACRenderer.this.rm = RenderMode.values()[ nextRmIdx ];
    }

    @Override
    public void paint(Graphics g) {
        Graphics2D g2 = (Graphics2D) g;

        g2.setColor(Color.WHITE);
        g2.fillRect(0, 0, this.getWidth(), this.getHeight());

        double width = this.getWidth() - 2 * BORDER_X;
        double height = this.getHeight() - 2 * BORDER_Y;

        double x;
        double y;

        for (Agent a : as) {
            x = BORDER_X + a.getPos().getX() * width;
            y = BORDER_Y + a.getPos().getY() * height;

            if (a.isHero() && (RenderMode.ALL == this.rm || RenderMode.HEROES == this.rm)) {
                g2.fillRect((int) x, (int) y, agentSize, agentSize);
                g2.setColor(Color.GREEN);

            } else if ( ! a.isHero() && (RenderMode.ALL == this.rm || RenderMode.COWARDS == this.rm) ) {
                g2.fillRect((int) x, (int) y, agentSize, agentSize);
                g2.setColor(Color.RED);
            }
        }
    }

    public void render(List<Agent> as) {
        this.as = as;
        this.repaint();
    }
}
