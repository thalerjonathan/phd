package SugarScape;

import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.parameter.Parameters;
import repast.simphony.random.RandomHelper;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridPoint;
import repast.simphony.util.ContextUtils;

/**
 * The agent for the sugar scape simulation. This agent implements
 * movement rule M, pg. 182, where best is defined by most sugar
 * at the closest location.
 * <p/>
 * The source is annotated so see that for more info.
 *
 * @author Eric Tatara
 * @author Nick Collier
 */

public class SugarAgent {
  int metabolism;           // how much sugar eaten per turn
  int sugar;                // amount of sugar owned
  int vision;               // how far can see
  int maxAge, currentAge;

  boolean dead;

  public SugarAgent() {
    dead = false;

    Parameters p = RunEnvironment.getInstance().getParameters();

    int maxMetabolism = (Integer) p.getValue("maxMetabolism");
    int maxVision = (Integer) p.getValue("maxVision");
    int maxInitialSugar = (Integer) p.getValue("maxInitialSugar");
    int minInitialSugar = (Integer) p.getValue("minInitialSugar");
    int minDeathAge = (Integer) p.getValue("minDeathAge");
    int maxDeathAge = (Integer) p.getValue("maxDeathAge");

    setMetabolism(RandomHelper.nextIntFromTo(1, maxMetabolism));
    setVision(RandomHelper.nextIntFromTo(1, maxVision));
    setSugar(RandomHelper.nextIntFromTo(minInitialSugar, maxInitialSugar));
    setMaxAge(RandomHelper.nextIntFromTo(minDeathAge, maxDeathAge));
  }

  @ScheduledMethod(start = 0, interval = 1)
  public void step() {
    SugarSpace sugarSpace = (SugarSpace) ContextUtils.getContext(this);
    Grid grid = (Grid) sugarSpace.getProjection("Grid");

    GridPoint point = grid.getLocation(this);

    int x = point.getX();
    int y = point.getY();

    // move to the best spot (movement rule M)
    moveToBestSpot();

    // get the sugar at the best spot
    sugar += sugarSpace.takeSugarAt(x, y);

    // consume an amount of sugar == to my metabolism
    sugar -= metabolism;

    // increae my age
    currentAge++;

    // die if no sugar or my age is greater than my max age
    if (sugar <= 0 || currentAge >= maxAge)
      this.die();
  }


  // implements movement rule M - p. 182 where best spot is most sugar
  // at nearest location.
  private void moveToBestSpot() {
    int bestSugar = -1;
    int bestDistance = -9999;
    int goodx[] = new int[16];
    int goody[] = new int[16];
    int bestSpots = 0;

    int xLook, yLook;

    SugarSpace sugarSpace = (SugarSpace) ContextUtils.getContext(this);
    Grid<Object> grid = (Grid) sugarSpace.getProjection("Grid");

    GridPoint point = grid.getLocation(this);

    int x = point.getX();
    int y = point.getY();

    boolean neighborExists = true;
    yLook = y;

    for (xLook = x - vision; xLook <= x + vision; xLook++) {

      Iterable neighbors = grid.getObjectsAt(xLook, yLook);

      if (!neighbors.iterator().hasNext())
        neighborExists = false;

      else for (Object o : neighbors) {
        if (o instanceof SugarAgent)
          break;

        neighborExists = false;
      }

      if (neighborExists == false) {
        if (sugarSpace.getSugarAt(xLook, yLook) > bestSugar) {
          bestSugar = sugarSpace.getSugarAt(xLook, yLook);
          bestDistance = Math.abs(x - xLook);
          bestSpots = 0;
          goodx[0] = xLook;
          goody[0] = yLook;
          bestSpots++;
        } else if (sugarSpace.getSugarAt(xLook, yLook) == bestSugar) {
          if (Math.abs(x - xLook) < bestDistance) {
            bestDistance = Math.abs(x - xLook);
            bestSpots = 0;
            goodx[0] = xLook;
            goody[0] = yLook;
            bestSpots++;
          } else if (Math.abs(x - xLook) == bestDistance) {
            goodx[bestSpots] = xLook;
            goody[bestSpots] = yLook;
            bestSpots++;
          }
        }
      }
    }

    neighborExists = true;
    xLook = x;

    for (yLook = y - vision; yLook <= y + vision; yLook++) {

      Iterable neighbors = grid.getObjectsAt(xLook, yLook);

      if (!neighbors.iterator().hasNext())
        neighborExists = false;

      else for (Object o : neighbors) {
        if (o instanceof SugarAgent)
          break;

        neighborExists = false;
      }

      if (neighborExists == false) {
        if (sugarSpace.getSugarAt(xLook, yLook) > bestSugar) {
          bestSugar = sugarSpace.getSugarAt(xLook, yLook);
          bestDistance = Math.abs(y - yLook);
          bestSpots = 0;
          goodx[0] = xLook;
          goody[0] = yLook;
          bestSpots++;
        } else if (sugarSpace.getSugarAt(xLook, yLook) == bestSugar) {
          if (Math.abs(y - yLook) < bestDistance) {
            bestDistance = Math.abs(y - yLook);
            bestSpots = 0;
            goodx[0] = xLook;
            goody[0] = yLook;
            bestSpots++;
          } else if (Math.abs(y - yLook) == bestDistance) {
            goodx[bestSpots] = xLook;
            goody[bestSpots] = yLook;
            bestSpots++;
          }
        }
      }
    }

    int chosenSpotIndex = 0;
    // agent go to the best spot
    if (bestSpots != 0) {
      if (bestSpots == 1) {
        chosenSpotIndex = 0;
      } else {
        chosenSpotIndex = RandomHelper.nextIntFromTo(0, bestSpots - 1);
      }

      grid.moveTo(this, goodx[chosenSpotIndex], goody[chosenSpotIndex]);
    }
  }

  //Kill the agent
  public void die() {
    // Get the context in which the agent resides.
    SugarSpace sugarSpace = (SugarSpace) ContextUtils.getContext(this);

    // Remove the agent from the context 
    sugarSpace.remove(this);

    // Spawn a new agent and add to the sugar space
    SugarAgent child = new SugarAgent();

    sugarSpace.add(child);
    dead = true;
  }

  // for logging purposes only
  public int getX() {
    if (dead) return -1;
    SugarSpace sugarSpace = (SugarSpace) ContextUtils.getContext(this);
    Grid grid = (Grid) sugarSpace.getProjection("Grid");

    return grid.getLocation(this).getX();
  }

  public boolean isDead() {
    return dead;
  }

  //for logging purposes only
  public int getY() {

    if (dead) return -1;
    SugarSpace sugarSpace = (SugarSpace) ContextUtils.getContext(this);
    Grid grid = (Grid) sugarSpace.getProjection("Grid");
    return grid.getLocation(this).getY();
  }

  public void setMetabolism(int meta) {
    metabolism = meta;
  }

  public void setVision(int vis) {
    vision = vis;
  }

  public int getMetabolism() {
    return metabolism;
  }

  public int getVision() {
    return vision;
  }

  public void setSugar(int sugar) {
    this.sugar = sugar;
  }

  public int getSugar() {
    return sugar;
  }

  public int getMaxAge() {
    return maxAge;
  }

  public void setMaxAge(int age) {
    maxAge = age;
  }

  public int getCurrentAge() {
    return currentAge;
  }
}
