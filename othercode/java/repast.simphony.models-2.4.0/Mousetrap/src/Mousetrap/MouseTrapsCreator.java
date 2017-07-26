package Mousetrap;

import repast.simphony.context.Context;
import repast.simphony.context.space.grid.GridFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.valueLayer.GridValueLayer;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridAdder;
import repast.simphony.space.grid.GridBuilderParameters;
import repast.simphony.space.grid.StrictBorders;

/**
 * @author Nick Collier
 * @version $Revision: 1.1 $ $Date: 2006/01/10 17:08:05 $
 */
public class MouseTrapsCreator implements ContextBuilder<MouseTrap> {

	/**
	 * Builds and returns a context. Building a context consists of filling it with
	 * agents, adding projects and so forth. When this is called for the master context
	 * the system will pass in a created context based on information given in the
	 * model.score file. When called for subcontexts, each subcontext that was added
	 * when the master context was built will be passed in.
	 *
	 * @param context
	 * @return the built context.
	 */
	public Context build(Context<MouseTrap> context) {
		Parameters p = RunEnvironment.getInstance().getParameters();
		Integer width = (Integer) p.getValue("width");
		Integer height = (Integer) p.getValue("height");
		Grid grid = GridFactoryFinder.createGridFactory(null).createGrid("Gym Floor",
		context, GridBuilderParameters.singleOccupancy2D(new MouseGridAdder(width - 1, height - 1),
								new StrictBorders(), width, height));

		GridValueLayer vl = new GridValueLayer("Trap Values", 1.0, true, width,
				height);
		context.addValueLayer(vl);

		for (int i = 0, n = width * height; i < n; i++) {
			context.add(new MouseTrap(grid, vl));
		}

		return context;
	}

	class MouseGridAdder implements GridAdder<MouseTrap> {

		int xLim, yLim;

		int x, y;

		public MouseGridAdder(int xLim, int yLim) {
			this.xLim = xLim;
			this.yLim = yLim;
		}

		public void add(Grid grid, MouseTrap obj) {
			grid.moveTo(obj, x, y);
			if (++x > xLim) {
				x = 0;
				y++;
			}
		}
	}
}
