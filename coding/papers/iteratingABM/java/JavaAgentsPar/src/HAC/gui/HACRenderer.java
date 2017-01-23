package HAC.gui;

import HAC.agent.HACAgent;

import javax.swing.*;
import java.awt.*;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Created by jonathan on 05/12/16.
 */
public class HACRenderer extends JPanel {
    private LinkedHashMap<Integer, HACAgent> am;

    private final static int BORDER_X = 10;
    private final static int BORDER_Y = 10;

    private int agentSize;

    private RenderMode rm;

    public HACRenderer( int agentSize ) {
        this.agentSize = agentSize;
        this.am = new LinkedHashMap<>();
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

        Iterator<Map.Entry<Integer, HACAgent>> iter = am.entrySet().iterator();
        while ( iter.hasNext() ) {
            Map.Entry<Integer, HACAgent> e = iter.next();
            renderAgent(e.getValue(), g2, width, height);
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

    public void render(LinkedHashMap<Integer, HACAgent> am) {
        this.am = am;
        this.repaint();
    }
}
