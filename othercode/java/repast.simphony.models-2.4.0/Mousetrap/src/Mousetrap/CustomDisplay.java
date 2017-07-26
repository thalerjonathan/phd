package Mousetrap;

import repast.simphony.context.Context;
import repast.simphony.query.PropertyEquals;
import repast.simphony.render.RenderListener;
import repast.simphony.render.RendererListenerSupport;
import repast.simphony.visualization.*;

import javax.swing.*;
import java.awt.*;

/**
 * @author Nick Collier
 */
public class CustomDisplay implements IDisplay {

  private JPanel panel = new JPanel();
  private JLabel label = new JLabel();
  private Context context;
  private int count;
  private RendererListenerSupport support = new RendererListenerSupport();

  public CustomDisplay(Context context) {
    this.context = context;
  }


  /**
   * Updates the state of the display to reflect whatever it is that it is displaying.
   */
  public void update() {
    count = query();
  }

  /**
   * Initializes the display. Called once before the display is made visible.
   */
  public void init() {
    update();
  }

  private int query() {
    PropertyEquals<MouseTrap> query = new PropertyEquals<MouseTrap>(context, "triggered", Boolean.TRUE);
    int count = 0;
    for (MouseTrap trap : query.query()) {
      count++;
    }
    return count;
  }

  /**
   * Sets the Layout for the display to use.
   *
   * @param layout the layout to use
   */
  public void setLayout(Layout layout) {
  }

  /**
   * Sets the frequency of the layout.
   *
   * @param frequency the frequency of the layout
   * @param interval  the interval if the frequency is AT_INTERVAL. The interval is in terms
   *                  of number of calls to update()
   */
  public void setLayoutFrequency(LayoutFrequency frequency, int interval) {
  }

  /**
   * Gets a panel that contains the actual gui for visualization.
   *
   * @return a panel that contains the actual gui for visualization.
   */
  public JPanel getPanel() {
    label.setForeground(Color.GREEN);
    label.setFont(label.getFont().deriveFont(18f));
    panel.add(label);
    return panel;
  }

  /**
   * Destroys the display, allowing it to free any resources it may have acquired.
   */
  public void destroy() {
  }

  /**
   * Notifies this IDisplay that its associated gui widget has been iconified.
   */
  public void iconified() {
  }

  /**
   * Notifies this IDisplay that its associated gui widget has been deIconified.
   */
  public void deIconified() {
  }

  /**
   * Notifies this IDisplay that its associated gui widget has been closed.
   */
  public void closed() {
  }

  /**
   * Adds a probe listener to listen for probe events produced by this IDisplay.
   *
   * @param listener the listener to add
   */
  public void addProbeListener(ProbeListener listener) {
  }

  /**
   * Gets the layout the display uses
   *
   * @return a layout object
   */
  public Layout getLayout() {
    return null;
  }

  public DisplayEditorLifecycle createEditor(JPanel panel) {
    return null;
  }

  public void render() {
    label.setText("Triggered Count: " + count);
    support.fireRenderFinished(this);
  }

  public void addRenderListener(RenderListener listener) {
    support.addListener(listener);
  }

  public void setPause(boolean pause) {
  }


  public void resetHomeView() {
  }

  public void registerToolBar(JToolBar bar) {
  }

  public void addDisplayListener(DisplayListener listener) {
  }


	public void toggleInfoProbe() {
		// TODO Auto-generated method stub
		
	}
}
