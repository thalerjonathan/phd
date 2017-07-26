package jzombies;

import java.util.List;

import jzombies.chart.DiseaseStatechart;
import repast.simphony.context.Context;
import repast.simphony.query.space.grid.GridCell;
import repast.simphony.query.space.grid.GridCellNgh;
import repast.simphony.query.space.projection.Within;
import repast.simphony.random.RandomHelper;
import repast.simphony.space.SpatialMath;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.space.continuous.NdPoint;
import repast.simphony.space.graph.Network;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridPoint;
import repast.simphony.ui.probe.ProbedProperty;
import repast.simphony.util.ContextUtils;
import repast.simphony.util.SimUtilities;

/**
 * @author nick
 *
 */
public class Human {
	
	private ContinuousSpace<Object> space;
	private Grid<Object> grid;
	private int energy, startingEnergy;
	
	@ProbedProperty(displayName="Biter")
	private Zombie biter;	// the zombie, if any, that bites this human
	
	@ProbedProperty(displayName="DiseaseStatechart")
	DiseaseStatechart diseaseStatechart = DiseaseStatechart.createStateChart(this, 0);
	
	public String getDiseaseStatechartState(){
		if (diseaseStatechart == null) return "";
		Object result = diseaseStatechart.getCurrentSimpleState();
		return result == null ? "" : result.toString();
	}

	public Human(ContinuousSpace<Object> space, Grid<Object> grid, int energy) {
		this.space = space;
		this.grid = grid;
		this.energy = startingEnergy = energy;
	}
	
	public void move() {
		// is zombie within 1 unit of me
		boolean zombieNear = false;
		for (Object obj : space.getObjects()) {
			if (obj instanceof Zombie) {
				Within within = new Within(this, obj, 1);
				if (space.evaluate(within)) {
					zombieNear = true;
					break;
				}
			}
		}
		if (zombieNear) flee();
		else walk();
	}
	
	private void walk() {
		NdPoint pt = space.getLocation(this);
		double xOffset = RandomHelper.getUniform().nextDoubleFromTo(-1, 1);
		double yOffset = RandomHelper.getUniform().nextDoubleFromTo(-1, 1);
		space.moveTo(this, pt.getX() + xOffset, pt.getY() + yOffset);
	}
	
	public void run() {
		NdPoint pt = space.getLocation(this);
		double xOffset = RandomHelper.getUniform().nextDoubleFromTo(-5, 5);
		double yOffset = RandomHelper.getUniform().nextDoubleFromTo(-5, 5);
		space.moveTo(this, pt.getX() + xOffset, pt.getY() + yOffset);
	}
	
	private void flee() {
		// get the grid location of this Human
		GridPoint pt = grid.getLocation(this);

		// use the GridCellNgh class to create GridCells for
		// the surrounding neighborhood.
		GridCellNgh<Zombie> nghCreator = new GridCellNgh<Zombie>(grid, pt,
				Zombie.class, 1, 1);
		List<GridCell<Zombie>> gridCells = nghCreator.getNeighborhood(true);
		SimUtilities.shuffle(gridCells, RandomHelper.getUniform());

		GridPoint pointWithLeastZombies = null;
		int minCount = Integer.MAX_VALUE;
		for (GridCell<Zombie> cell : gridCells) {
			if (cell.size() < minCount) {
				pointWithLeastZombies = cell.getPoint();
				minCount = cell.size();
			}
		}
		
		if (energy > 0) {
			moveTowards(pointWithLeastZombies);
		} else {
			energy = startingEnergy;
		}
	}
	
	public void moveTowards(GridPoint pt) {
		// only move if we are not already in this grid location
		if (!pt.equals(grid.getLocation(this))) {
			NdPoint myPoint = space.getLocation(this);
			NdPoint otherPoint = new NdPoint(pt.getX(), pt.getY());
			double angle = SpatialMath.calcAngleFor2DMovement(space, myPoint, otherPoint);
			space.moveByVector(this, 2, angle, 0);
			myPoint = space.getLocation(this);
			grid.moveTo(this, (int)myPoint.getX(), (int)myPoint.getY());
			energy--;
		}
	}
	
	public void bitten(Zombie zombie) {
		this.biter = zombie;
		diseaseStatechart.receiveMessage("Brains");
	}
	
	public void die() {
		GridPoint pt = grid.getLocation(this);
		NdPoint spacePt = space.getLocation(this);
		Context<Object> context = ContextUtils.getContext(this);
		context.remove(this);
		Zombie zombie = new Zombie(space, grid);
		context.add(zombie);
		space.moveTo(zombie, spacePt.getX(), spacePt.getY());
		grid.moveTo(zombie, pt.getX(), pt.getY());
		
		Network<Object> net = (Network<Object>)context.getProjection("infection network");
		net.addEdge(biter, zombie);
	}
}
