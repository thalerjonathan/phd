package GlidersGrid;

import java.util.Iterator;

import repast.simphony.context.Context;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.query.space.grid.MooreQuery;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridPoint;
import repast.simphony.util.ContextUtils;

public class Dead {

	private Grid<Object> grid;
	private int state;

	public Dead(Grid<Object> grid) {
		this.grid = grid;
	}

	// calculate the state for the next time tick for dead cells
	@ScheduledMethod(start = 1, interval = 1, priority = 4)
	public void step1() {
		MooreQuery<Dead> query = new MooreQuery(grid, this);
		int neighbours = 0;
		for (Object o : query.query()) {
			if (o instanceof Living) {
				neighbours++;
				if (neighbours ==3) {
				}
			}
		}
		if (neighbours == 3) {
			state = 1;
		} else {
			state = 0;
		}
	}

	// visualise the change into the underlay and grid
	@ScheduledMethod(start = 1, interval = 1, priority = 1)
	public void step2() {
		if (state == 1) {
			GridPoint gpt = grid.getLocation(this);
			Context<Object> context = ContextUtils.getContext(this);
			context.remove(this);
			Living livingCell = new Living(grid);
			context.add(livingCell);
			grid.moveTo(livingCell, gpt.getX(), gpt.getY());
			context.add(livingCell);
		}
	}

}