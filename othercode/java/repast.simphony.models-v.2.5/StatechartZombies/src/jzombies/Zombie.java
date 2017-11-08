/**
 * 
 */
package jzombies;

import java.util.ArrayList;
import java.util.List;

import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.query.space.grid.GridCell;
import repast.simphony.query.space.grid.GridCellNgh;
import repast.simphony.random.RandomHelper;
import repast.simphony.space.SpatialMath;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.space.continuous.NdPoint;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridPoint;
import repast.simphony.util.SimUtilities;

/**
 * @author nick
 * 
 */
public class Zombie {

	private ContinuousSpace<Object> space;
	private Grid<Object> grid;

	public Zombie(ContinuousSpace<Object> space, Grid<Object> grid) {
		this.space = space;
		this.grid = grid;
	}

	@ScheduledMethod(start = 1, interval = 1)
	public void step() {
		// get the grid location of this Zombie
		GridPoint pt = grid.getLocation(this);

		// use the GridCellNgh class to create GridCells for
		// the surrounding neighborhood.
		GridCellNgh<Human> nghCreator = new GridCellNgh<Human>(grid, pt,
				Human.class, 1, 1);
		List<GridCell<Human>> gridCells = nghCreator.getNeighborhood(true);
		SimUtilities.shuffle(gridCells, RandomHelper.getUniform());

		GridPoint pointWithMostHumans = null;
		int maxCount = -1;
		for (GridCell<Human> cell : gridCells) {
			if (cell.size() > maxCount) {
				pointWithMostHumans = cell.getPoint();
				maxCount = cell.size();
			}
		}
		moveTowards(pointWithMostHumans);
		infect();
	}

	public void moveTowards(GridPoint pt) {
		// only move if we are not already in this grid location
		if (!pt.equals(grid.getLocation(this))) {
			NdPoint myPoint = space.getLocation(this);
			NdPoint otherPoint = new NdPoint(pt.getX(), pt.getY());
			double angle = SpatialMath.calcAngleFor2DMovement(space, myPoint,
					otherPoint);
			space.moveByVector(this, 1, angle, 0);
			myPoint = space.getLocation(this);
			grid.moveTo(this, (int) myPoint.getX(), (int) myPoint.getY());
		}
	}

	public void infect() {
		GridPoint pt = grid.getLocation(this);
		List<Human> humans = new ArrayList<Human>();
		for (Object obj : grid.getObjectsAt(pt.getX(), pt.getY())) {
			if (obj instanceof Human) {
				humans.add((Human)obj);
			}
		}

		if (humans.size() > 0) {
			int index = RandomHelper.nextIntFromTo(0, humans.size() - 1);
			Human human = humans.get(index);
			human.bitten(this);
		}
	}
}
