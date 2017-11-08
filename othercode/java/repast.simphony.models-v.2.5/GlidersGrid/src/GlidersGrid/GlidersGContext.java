package GlidersGrid;

import java.awt.Point;
import java.util.ArrayList;

import repast.simphony.context.Context;
import repast.simphony.context.space.grid.GridFactory;
import repast.simphony.context.space.grid.GridFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridBuilderParameters;
import repast.simphony.space.grid.SimpleGridAdder;
import repast.simphony.space.grid.StickyBorders;

public class GlidersGContext implements ContextBuilder<Object> {
	public Context build(Context<Object> context) {
		context.setId("GlidersGrid");

		GridFactory gridFactory = GridFactoryFinder.createGridFactory(null);

//		implement strict borders so there is no wrapping
//		multiple occupancy of grid cells is still set to false
		Grid<Object> grid = gridFactory.createGrid("grid", context,
				new GridBuilderParameters<Object>(new StickyBorders(),
						new SimpleGridAdder<Object>(), false, 50, 50));
		
		//set the number of living cells equal to those required for the gun
		int livingCount = 36;
		
		//create an xy array list of cells for the gun, used to place the living agents
		//see solution by stdunbar at http://forums.hotjoe.com/posts/list/2898.page
//		note: row count starts at bottom, but column count starts at left
		ArrayList<Point> points = new ArrayList<Point>();
		Point point1 = new Point (1,(50-5));
		Point point2 = new Point (1,(50-6));
		Point point3 = new Point (2,(50-5));
		Point point4 = new Point (2,(50-6));
		Point point5 = new Point (11,(50-5));
		Point point6 = new Point (11,(50-6));
		Point point7 = new Point (11,(50-7));
		Point point8 = new Point (12,(50-4));
		Point point9 = new Point (12,(50-8));
		Point point10 = new Point (13,(50-3));
		Point point11 = new Point (13,(50-9));
		Point point12 = new Point (14,(50-3));
		Point point13 = new Point (14,(50-9));
		Point point14 = new Point (15,(50-6));
		Point point15 = new Point (16,(50-4));
		Point point16 = new Point (16,(50-8));
		Point point17 = new Point (17,(50-5));
		Point point18 = new Point (17,(50-6));
		Point point19 = new Point (17,(50-7));
		Point point20 = new Point (18,(50-6));
		Point point21 = new Point (21,(50-3));
		Point point22 = new Point (21,(50-4));
		Point point23 = new Point (21,(50-5));
		Point point24 = new Point (22,(50-3));
		Point point25 = new Point (22,(50-4));
		Point point26 = new Point (22,(50-5));
		Point point27 = new Point (23,(50-2));
		Point point28 = new Point (23,(50-6));
		Point point29 = new Point (25,(50-1));
		Point point30 = new Point (25,(50-2));
		Point point31 = new Point (25,(50-6));
		Point point32 = new Point (25,(50-7));
		Point point33 = new Point (35,(50-3));
		Point point34 = new Point (35,(50-4));
		Point point35 = new Point (36,(50-3));
		Point point36 = new Point (36,(50-4));
		
		points.add(point1);
		points.add(point2);
		points.add(point3);
		points.add(point4);
		points.add(point5);
		points.add(point6);
		points.add(point7);
		points.add(point8);
		points.add(point9);
		points.add(point10);
		points.add(point11);
		points.add(point12);
		points.add(point13);
		points.add(point14);
		points.add(point15);
		points.add(point16);
		points.add(point17);
		points.add(point18);
		points.add(point19);
		points.add(point20);
		points.add(point21);
		points.add(point22);
		points.add(point23);
		points.add(point24);
		points.add(point25);
		points.add(point26);
		points.add(point27);
		points.add(point28);
		points.add(point29);
		points.add(point30);
		points.add(point31);
		points.add(point32);
		points.add(point33);
		points.add(point34);
		points.add(point35);
		points.add(point36);
		
		//add the living cells to the context and visualisation
		for (Point nextPoint: points) {
			Living liveCell = new Living(grid);
			context.add(liveCell);
			grid.moveTo(liveCell, (int) nextPoint.getX(), (int) nextPoint.getY());
			context.add(liveCell);
		}

		int deadCount = (int) (50 * 50) - livingCount;
		Dead deadCell = new Dead(grid);
		context.add(deadCell);
		for (int x = 0; x < 50 && deadCount > 0; x++) {
			for (int y = 0; y < 50 && deadCount > 0; y++) {
				if (grid.moveTo(deadCell, x, y)) {
					deadCount--;
					if (deadCount > 0) {
						deadCell = new Dead(grid);
						context.add(deadCell);
					}
				}
			}
		}

		return context;
	}
}
