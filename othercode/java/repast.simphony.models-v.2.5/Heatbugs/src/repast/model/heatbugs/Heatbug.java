/**
 * 
 */
package repast.model.heatbugs;

import repast.simphony.context.Context;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.random.RandomHelper;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridPoint;
import repast.simphony.valueLayer.AbstractGridFunction;
import repast.simphony.valueLayer.BufferedGridValueLayer;
import repast.simphony.valueLayer.MaxGridFunction;
import repast.simphony.valueLayer.MinGridFunction;
import repast.simphony.valueLayer.ValueLayerDiffuser;
import repast.simphony.valueLayer.BufferedGridValueLayer.Buffer;

/**
 * Heatbug agent class.
 * 
 * @author Nick Collier
 */
public class Heatbug {

  private double unhappiness = 0;
  private int idealTemp, outputHeat;
  private float randomMoveProb;
  private BufferedGridValueLayer heat;
  private Grid<Heatbug> grid;

  @SuppressWarnings("unchecked")
  public Heatbug(int idealTemp, int outputHeat, float randomMoveProb, Context<Heatbug> context) {
    this.idealTemp = idealTemp;
    this.outputHeat = outputHeat;
    this.randomMoveProb = randomMoveProb;
    
    heat = (BufferedGridValueLayer) context.getValueLayer("Heat Layer");
    grid = (Grid<Heatbug>) context.getProjection("Bug Grid");
  }

  @ScheduledMethod(start = 1, interval = 1, priority = 0)
  public void step() {
    GridPoint pt = grid.getLocation(this);
    double heatHere = heat.get(pt.getX(), pt.getY());
    if (heatHere < idealTemp) {
      unhappiness = (double) (idealTemp - heatHere) / ValueLayerDiffuser.DEFAULT_MAX;
    } else {
      unhappiness = (double) (heatHere - idealTemp) / ValueLayerDiffuser.DEFAULT_MAX;
    }

    GridPoint newLocation = new GridPoint(pt.getX(), pt.getY());
    if (RandomHelper.nextDouble() < randomMoveProb) {
      int x = newLocation.getX() + RandomHelper.nextIntFromTo(-1, 1);
      int y = newLocation.getY() + RandomHelper.nextIntFromTo(-1, 1);
      newLocation = new GridPoint(x, y);

    } else if (unhappiness != 0) {
      AbstractGridFunction func = heatHere < idealTemp ? new MaxGridFunction()
          : new MinGridFunction();
      heat.forEach(func, pt, Buffer.READ, 1, 1);
      GridPoint desiredPt = func.getResults().get(0).getLocation();
      // not on the hottest or coldest place at the moment
      // then try to move to the desired point, if that's full then
      // choose oone of the 8 neighboring cells at random.
      if (!desiredPt.equals(pt)) {
        int x = desiredPt.getX();
        int y = desiredPt.getY();
        int tries = 0;
        while (grid.getObjectAt(x, y) != null && tries < 10) {
          x = newLocation.getX() + RandomHelper.nextIntFromTo(-1, 1);
          y = newLocation.getY() + RandomHelper.nextIntFromTo(-1, 1);
          tries++;
        }

        if (tries < 10) {
          newLocation = new GridPoint(x, y);
        }
      }
    }

    //System.out.printf("current heat val: %f%n", heat.get(pt.getX(), pt.getY()));
    //System.out.println("outputting heat: " + outputHeat);
    heat.set(outputHeat + heat.get(pt.getX(), pt.getY()), pt.getX(), pt.getY());
    grid.moveTo(this, newLocation.getX(), newLocation.getY());
  }
}
