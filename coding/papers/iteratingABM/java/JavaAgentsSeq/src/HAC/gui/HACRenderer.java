package HAC.gui;

import HAC.agent.HACAgent;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by jonathan on 05/12/16.
 */
public class HACRenderer extends JPanel implements MouseListener {
    private List<HACAgent> as;
    private int selectedAgentId;

    private final static int BORDER_X = 10;
    private final static int BORDER_Y = 10;

    private int agentSize;

    private RenderMode rm;

    public HACRenderer( int agentSize ) {
        this.agentSize = agentSize;
        this.as = new ArrayList<>();
        this.rm = RenderMode.ALL;
        this.selectedAgentId = -1;

        this.addMouseListener( this );
    }

    public void toggleRenderMode() {
        int nextRmIdx = (HACRenderer.this.rm.ordinal() + 1) % RenderMode.values().length;
        HACRenderer.this.rm = RenderMode.values()[ nextRmIdx ];
    }

    public void resetAgentSelection() {
        this.selectedAgentId = -1;
    }

    public void switchAgentSelection(boolean enemy) {
        if ( this.selectedAgentId == -1 )
            return;

        HACAgent a = this.as.get( this.selectedAgentId );
        if ( enemy ) {
            this.selectedAgentId = a.getEnemy().getId();
        } else {
            this.selectedAgentId = a.getFriend().getId();
        }
    }

    @Override
    public void paint(Graphics g) {
        Graphics2D g2 = (Graphics2D) g;

        g2.setColor(Color.WHITE);
        g2.fillRect(0, 0, this.getWidth(), this.getHeight());

        double width = this.getWidth() - 2 * BORDER_X;
        double height = this.getHeight() - 2 * BORDER_Y;

        if ( -1 != this.selectedAgentId ) {
            HACAgent a = this.as.get(this.selectedAgentId);

            renderAgent(a, g2, width, height);
            renderAgent(a.getEnemy(), g2, width, height);
            renderAgent(a.getFriend(), g2, width, height);
            a = null;
        } else {
            for (HACAgent a : this.as) {
                renderAgent(a, g2, width, height);
            }
        }
    }

    private void renderAgent(HACAgent a, Graphics2D g2, double width, double height) {
        double x = BORDER_X + a.getPos().getX() * width;
        double y = BORDER_Y + a.getPos().getY() * height;

        if (a.isHero() && (RenderMode.ALL == this.rm || RenderMode.HEROES == this.rm)) {
            g2.setColor(Color.GREEN);
            g2.fillRect((int) x, (int) y, agentSize, agentSize);


        } else if (!a.isHero() && (RenderMode.ALL == this.rm || RenderMode.COWARDS == this.rm)) {
            g2.setColor(Color.RED);
            g2.fillRect((int) x, (int) y, agentSize, agentSize);

        }
    }

    public void render(List<HACAgent> as) {
        this.as = as;
        this.repaint();
    }

    @Override
    public void mouseClicked(MouseEvent e) {

    }

    @Override
    public void mousePressed(MouseEvent e) {
        int mouseX = e.getX();
        int mouseY = e.getY();

        double width = this.getWidth() - 2 * BORDER_X;
        double height = this.getHeight() - 2 * BORDER_Y;

        this.selectedAgentId = -1;

        double x;
        double y;

        for (HACAgent a : this.as) {
            x = BORDER_X + a.getPos().getX() * width;
            y = BORDER_Y + a.getPos().getY() * height;

            if ( ( mouseX >= x ) && ( mouseX <= (x + agentSize) ) ) {
                if ( ( mouseY >= y ) && ( mouseY <= (y + agentSize) ) ) {
                    this.selectedAgentId = a.getId();
                    break;
                }
            }
        }
    }

    @Override
    public void mouseReleased(MouseEvent e) {

    }

    @Override
    public void mouseEntered(MouseEvent e) {

    }

    @Override
    public void mouseExited(MouseEvent e) {

    }
}
