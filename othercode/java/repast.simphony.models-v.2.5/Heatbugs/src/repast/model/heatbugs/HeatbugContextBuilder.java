/**
 * 
 */
package repast.model.heatbugs;

import repast.simphony.context.Context;
import repast.simphony.context.space.grid.GridFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.parameter.Parameters;
import repast.simphony.random.RandomHelper;
import repast.simphony.space.grid.GridBuilderParameters;
import repast.simphony.space.grid.RandomGridAdder;
import repast.simphony.space.grid.WrapAroundBorders;
import repast.simphony.valueLayer.BufferedGridValueLayer;

/**
 * @author Nick Collier
 */
public class HeatbugContextBuilder implements ContextBuilder<Heatbug> {
  
  /* (non-Javadoc)
   * @see repast.simphony.dataLoader.ContextBuilder#build(repast.simphony.context.Context)
   */
  public Context<Heatbug> build(Context<Heatbug> context) {
    GridFactoryFinder.createGridFactory(null).createGrid(
        "Bug Grid",
        context,
        new GridBuilderParameters<Heatbug>(new WrapAroundBorders(), new RandomGridAdder<Heatbug>(),
            false, new int[]{80, 80}, new int[]{5, 5}));
    
    BufferedGridValueLayer heat = new BufferedGridValueLayer("Heat Layer", 0, true, new WrapAroundBorders(), new int[]{80, 80}, new int[]{5, 5});
    context.addValueLayer(heat);
    
    Parameters params = RunEnvironment.getInstance().getParameters();
    
    int maxIdealTemp = (Integer)params.getValue("maxIdealTemp");
    int minIdealTemp = (Integer)params.getValue("minIdealTemp");
    
    int maxOutputTemp = (Integer)params.getValue("maxOutputTemp");
    int minOutputTemp = (Integer)params.getValue("minOutputTemp");
    int numAgents = (Integer) params.getValue("initialNumAgents");
    
    float rndMoveProb = (Float)params.getValue("rndMoveProb");
    
    for (int i = 0; i < numAgents; i++) {
      int idealTemp = RandomHelper.nextIntFromTo(minIdealTemp, maxIdealTemp);
      int outputTemp = RandomHelper.nextIntFromTo(minOutputTemp, maxOutputTemp);
      Heatbug bug = new Heatbug(idealTemp, outputTemp, rndMoveProb, context);
      context.add(bug);
    }
 
    return context;
  }
  
  @ScheduledMethod(start = 1, interval = 1, priority = -1)
  public void swapHeatBuffers() {
    System.out.println("swap");
    //System.out.println("swapping");
    //heat.swap();
  }
}
