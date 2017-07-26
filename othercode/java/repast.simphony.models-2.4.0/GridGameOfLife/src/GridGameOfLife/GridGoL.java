package GridGameOfLife;

import repast.simphony.context.Context;
import repast.simphony.context.space.grid.GridFactory;
import repast.simphony.context.space.grid.GridFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.random.RandomHelper;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridBuilderParameters;
import repast.simphony.space.grid.SimpleGridAdder;
import repast.simphony.space.grid.WrapAroundBorders;

public class GridGoL implements ContextBuilder<Object> {
	
	public Context build(Context<Object> context) {
		context.setId("GridGameOfLife");

		GridFactory gridFactory = GridFactoryFinder.createGridFactory(null);

		Grid<Object> grid = gridFactory.createGrid("grid", context,
				new GridBuilderParameters<Object>(new WrapAroundBorders(),
						new SimpleGridAdder<Object>(), false, 50, 50));

		int livingCount = (int) (50 * 50) / 4;

		for (int i = 0; i < livingCount; i++) {
			Living liveCell = new Living(grid);
			context.add(liveCell);
			int x = RandomHelper.nextIntFromTo(0, 50 - 1);
			int y = RandomHelper.nextIntFromTo(0, 50 - 1);
			while (!grid.moveTo(liveCell, x, y)) {
				x = RandomHelper.nextIntFromTo(0, 50 - 1);
				y = RandomHelper.nextIntFromTo(0, 50 - 1);
			}
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
