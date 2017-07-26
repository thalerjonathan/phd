/**
 * 
 */
package Cells;

import repast.simphony.context.Context;
import repast.simphony.data2.AggregateDataSource;
import repast.simphony.space.grid.Grid;

/**
 * @author nick
 *
 */
public class CoverageDataSource implements AggregateDataSource {

	@Override
	public String getId() {
		return "Coverage Percent";
	}

	@Override
	public Class<?> getDataType() {
		return double.class;
	}

	@Override
	public Class<?> getSourceType() {
		return Context.class;
	}

	@SuppressWarnings("rawtypes")
	@Override
	public Object get(Iterable<?> objs, int size) {
		Context context = (Context) objs.iterator().next();
		Grid grid = (Grid)context.getProjection("Grid");
		double numPoints = grid.getDimensions().getHeight() * grid.getDimensions().getWidth();
		double numCells = context.size() - 1;
		return (numCells / numPoints) * 100;
	}

	@Override
	public void reset() {}
}
