package GridGameOfLife;

import repast.simphony.context.Context;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.query.space.grid.MooreQuery;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridPoint;
import repast.simphony.util.ContextUtils;

public class Living {

	private Grid<Object> grid;
	private int state;

	public Living(Grid<Object> grid) {
		this.grid = grid;
	}

	// calculate the state for the next time tick for living cells
	@ScheduledMethod(start = 1, interval = 1, priority = 3)
	public void step1() {
		MooreQuery<Living> query = new MooreQuery(grid, this);
		int neighbours = 0;
		for (Object o : query.query()) {
			if (o instanceof Living) {
				neighbours++;
			}
		}

		if (neighbours == 2 || neighbours == 3) {
			state = 1;
			} else {
			state = 0;
		}
	}

	// visualise the change into the grid
	@ScheduledMethod(start = 1, interval = 1, priority = 2)
	public void step2() {
		if (state == 0) {
			GridPoint gpt = grid.getLocation(this);
			Context<Object> context = ContextUtils.getContext(this);
			context.remove(this);
			Dead deadCell = new Dead(grid);
			context.add(deadCell);
			grid.moveTo(deadCell, gpt.getX(), gpt.getY());
			context.add(deadCell);
		}

	}


}
