/**
 * 
 */
package repast.model.heatbugs;

import repast.simphony.context.DefaultContext;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.valueLayer.BufferedGridValueLayer;
import repast.simphony.valueLayer.IGridValueLayer;
import repast.simphony.valueLayer.ValueLayer;
import repast.simphony.valueLayer.ValueLayerDiffuser;

/**
 * Context for heatbugs. We use this so we can add the swap method.
 * 
 * @author Nick Collier
 */
public class HeatbugContext extends DefaultContext<Heatbug> {
  
  private ValueLayerDiffuser diffuser;
  
  /* (non-Javadoc)
   * @see repast.simphony.context.AbstractContext#addValueLayer(repast.simphony.valueLayer.ValueLayer)
   */
  @Override
  public void addValueLayer(ValueLayer valueLayer) {
    // TODO Auto-generated method stub
    super.addValueLayer(valueLayer);
    diffuser = new ValueLayerDiffuser((IGridValueLayer)valueLayer, .99, 1.0, true);
  }

  /**
   * Swaps the buffered heat layers.
   */
  // priority = -1 so that the heatbugs action occurs first
  @ScheduledMethod(start = 1, interval = 1, priority = -1)
  public void swap() {
    BufferedGridValueLayer grid = (BufferedGridValueLayer)getValueLayer("Heat Layer");
    grid.swap();
    diffuser.diffuse();
  }
  

}
