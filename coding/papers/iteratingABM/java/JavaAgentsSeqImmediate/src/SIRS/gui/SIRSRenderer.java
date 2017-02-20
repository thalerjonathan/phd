package SIRS.gui;

import SIRS.agent.SIRSAgent;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by jonathan on 05/12/16.
 */
public class SIRSRenderer extends JPanel {
    private List<SIRSAgent> as;

    private final static int BORDER_X = 10;
    private final static int BORDER_Y = 10;

    private int columns;
    private int rows;

    public SIRSRenderer(int columns, int rows) {
        this.columns = columns;
        this.rows = rows;
        this.as = new ArrayList<>();
    }

    @Override
    public void paint(Graphics g) {
        Graphics2D g2 = (Graphics2D) g;

        g2.setColor(Color.WHITE);
        g2.fillRect(0, 0, this.getWidth(), this.getHeight());

        double width = this.getWidth() - 2 * BORDER_X;
        double height = this.getHeight() - 2 * BORDER_Y;

        double cellWidth = width / columns;
        double cellHeight = height / rows;

        for (SIRSAgent a : this.as) {
            double x = BORDER_X + (a.getCell().getX() * cellWidth);
            double y = BORDER_Y + (a.getCell().getY() * cellHeight);

            if ( SIRSAgent.SIRSState.Susceptible == a.getState() ) {
                g2.setColor(Color.GREEN);

            } else if ( SIRSAgent.SIRSState.Infected == a.getState() ) {
                g2.setColor(Color.RED);

            } else if ( SIRSAgent.SIRSState.Recovered == a.getState() ) {
                g2.setColor(Color.BLUE);
            }

            Shape r = new Rectangle2D.Double(x, y, cellWidth, cellHeight);
            g2.fill(r);
        }
    }

    public void render(List<SIRSAgent> as) {
        this.as = as;
        this.repaint();
    }
}
