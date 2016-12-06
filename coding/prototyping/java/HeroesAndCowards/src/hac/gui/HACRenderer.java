package hac.gui;

import hac.backend.agent.Agent;
import hac.backend.simulation.WorldType;

import javax.swing.*;
import java.awt.*;
import java.util.*;
import java.util.List;

/**
 * Created by jonathan on 05/12/16.
 */
public class HACRenderer extends JPanel {
    private List<Agent> as;
    private WorldType wt;

    private final static int BORDER_X = 10;
    private final static int BORDER_Y = 10;

    private int agentSize;

    public HACRenderer( int agentSize ) {
        this.agentSize = agentSize;
        this.as = new ArrayList<>();
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

            if (a.isHero()) {
                g2.setColor(Color.GREEN);
            } else {
                g2.setColor(Color.RED);
            }

            g2.fillRect((int) x, (int)y, agentSize, agentSize);
        }
    }

    public void render(List<Agent> as, WorldType wt) {
        this.as = as;
        this.wt = wt;

        this.repaint();
    }
}
