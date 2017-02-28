package gui;

import cell.SGCell;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;


/**
 * Created by jonathan on 05/12/16.
 */
public class SGRenderer extends JPanel {
    private List<SGCell> cells;

    private final static int BORDER_X = 10;
    private final static int BORDER_Y = 10;

    private int columns;
    private int rows;

    private final static Color BLUE = new Color(0.0f,0.0f, 0.7f);
    private final static Color GREEN = new Color(0.0f,0.4f, 0.0f);
    private final static Color YELLOW = new Color(1.0f,0.9f, 0.0f);
    private final static Color RED = new Color(0.7f,0.0f, 0.0f);

    public SGRenderer(int columns, int rows) {
        this.columns = columns;
        this.rows = rows;
        this.cells = new ArrayList<>();
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

        for ( SGCell c : cells) {
            double x = BORDER_X + (c.getX() * cellWidth);
            double y = BORDER_Y + (c.getY() * cellHeight);

            if ( SGCell.SGState.Cooperator == c.getPrevState() && SGCell.SGState.Cooperator == c.getCurrState() ) {
                g2.setColor( BLUE );

            } else if ( SGCell.SGState.Defector == c.getPrevState() && SGCell.SGState.Defector == c.getCurrState() ) {
                g2.setColor( RED );

            } else if ( SGCell.SGState.Defector == c.getPrevState() && SGCell.SGState.Cooperator == c.getCurrState() ) {
                g2.setColor( GREEN );

            } else if ( SGCell.SGState.Cooperator == c.getPrevState() && SGCell.SGState.Defector == c.getCurrState() ) {
                g2.setColor( YELLOW );

            }

            Shape r = new Rectangle2D.Double(x, y, cellWidth, cellHeight);
            g2.fill(r);
        }
    }

    public void render(List<SGCell> cells) {
        this.cells = cells;
        this.repaint();
    }
}
