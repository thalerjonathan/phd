package SGEnv.gui;

import SGEnv.agent.SGAgent;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by jonathan on 05/12/16.
 */
public class SGRenderer extends JPanel {
    private List<SGAgent> as;

    private final static int BORDER_X = 10;
    private final static int BORDER_Y = 10;

    private int columns;
    private int rows;

    public SGRenderer(int columns, int rows) {
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

        for (SGAgent a : this.as) {
            double x = BORDER_X + (a.getCell().getX() * cellWidth);
            double y = BORDER_Y + (a.getCell().getY() * cellHeight);

            if ( SGAgent.SGState.Cooperator == a.getPrevState() && SGAgent.SGState.Cooperator == a.getCurrState() ) {
                g2.setColor(Color.blue);

            } else if ( SGAgent.SGState.Defector == a.getPrevState() && SGAgent.SGState.Defector == a.getCurrState() ) {
                g2.setColor(Color.red);

            } else if ( SGAgent.SGState.Defector == a.getPrevState() && SGAgent.SGState.Cooperator == a.getCurrState() ) {
                g2.setColor(Color.yellow);

            } else if ( SGAgent.SGState.Cooperator == a.getPrevState() && SGAgent.SGState.Defector == a.getCurrState() ) {
                g2.setColor(Color.green);

            }

            Shape r = new Rectangle2D.Double(x, y, cellWidth, cellHeight);
            g2.fill(r);
        }
    }

    public void render(List<SGAgent> as) {
        this.as = as;
        this.repaint();
    }
}
