package Mousetrap;

import repast.simphony.engine.schedule.IAction;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.engine.schedule.ScheduleParameters;
import repast.simphony.ui.probe.ProbeID;
import repast.simphony.valueLayer.GridValueLayer;
import repast.simphony.random.RandomHelper;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridDimensions;
import repast.simphony.space.grid.GridPoint;
import repast.simphony.parameter.Parameter;

/**
 * @author Nick Collier
 * @version $Revision$ $Date$
 */
public class MouseTrap {

	private boolean triggered = false;
	private Grid<MouseTrap> grid;
	private GridValueLayer layer;

  public MouseTrap(Grid<MouseTrap> grid, GridValueLayer layer) {
		this.grid = grid;
		this.layer = layer;
	}

  @ProbeID
  public String name() {
    return "Mouse Trap at " + grid.getLocation(this);
  }

  @Parameter(displayName="Trap Sprung", usageName="triggered")
  public boolean isTriggered() {
		return triggered;
	}


	public void trigger() {
		if (!triggered) {
			GridPoint point = grid.getLocation(this);
			layer.set(0.0, point.getX(), point.getY());
			triggered = true;
			for (int i = 0; i < 3; i++) {
				
				int timeDelta = RandomHelper.nextIntFromTo(2, 5);
				// query would be good for this
				int xMin = point.getX() - 3;
				int xMax = point.getX() + 3;
				xMin = xMin < 0 ? 0 : xMin;
				GridDimensions dimensions = grid.getDimensions();
				int xSize = (int) dimensions.getWidth() - 1;
				xMax = xMax > xSize ? xSize : xMax;

				int yMin = point.getY() - 3;
				int yMax = point.getY() + 3;
				yMin = yMin < 0 ? 0 : yMin;
				int ySize = (int)dimensions.getHeight() - 1;
				yMax = yMax > ySize ? ySize : yMax;

				int x = RandomHelper.nextIntFromTo(xMin, xMax);
				int y = RandomHelper.nextIntFromTo(yMin, yMax);

				MouseTrap trap = grid.getObjectAt(x, y);
				ISchedule schedule = repast.simphony.engine.environment.RunEnvironment.getInstance().getCurrentSchedule();
				double triggerTime = schedule.getTickCount() + timeDelta;
				schedule.schedule(ScheduleParameters.createOneTime(triggerTime), new TrapTrigger(trap));
			}
		}
	}

	private static class TrapTrigger implements IAction {

		private MouseTrap trap;

		public TrapTrigger(MouseTrap trap) {
			this.trap = trap;
		}

		public void execute() {
			trap.trigger();
		}
	}
}
