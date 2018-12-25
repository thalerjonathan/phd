package barter.display;

import java.util.List;

import sim.display.Display2D;
import sim.engine.SimState;
import sim.field.grid.SparseGrid2D;
import sim.portrayal.grid.SparseGridPortrayal2D;
import barter.BarterEconomy;
import barter.TradeAgent;

/**
 * Helper class to set up the portrayal for the agent visualistaion.
 * 
 * @Immutable This class is immutable.
 */
public class BarterPortrayalSetup {

	static void setupPortrayals(SimState state, Display2D display) {
		List<TradeAgent> agents = ((BarterEconomy) state).getAgents();
		SparseGridPortrayal2D barterPortrayal = new SparseGridPortrayal2D();
		final double TRIANGLE_HEIGHT = Math.sqrt(1 - 1.0 / 4.0);
		int width = display.getWidth();
		int height = display.getHeight();
		SparseGrid2D agentPositions = new SparseGrid2D(width, height);
		int idx = 0;
//		System.out.println("width: " + width + "\theight: " + height);
//		System.out.println("gridWidth: " + agentPositions.getWidth() + "\tgridHeight: " + agentPositions.getHeight());
		
		// int columns = (int) (Math.ceil(Math.sqrt(agents.size())));
		double area = Math.sqrt(width * height / (agents.size() * TRIANGLE_HEIGHT) * 0.95);
		int columns = (int) (width / area);
		double xd = area;
		double yd = xd * TRIANGLE_HEIGHT;
		
		for(TradeAgent e : agents) {
			double xp = idx % columns + 0.5;
			int yp = idx / columns;
			int x;
			int y;

			if(true) {
				x = (int) ((xp + 0.5 * (yp % 2)) * xd);
				y = (int) (yp * yd + xd * 0.5);
			} else {
				x = (int) (xp * xd);
				y = (int) (yp * yd);
			}
			agentPositions.setObjectLocation(e, x, y);
			idx++;
		}
		barterPortrayal.setField(agentPositions);
		
		display.attach(barterPortrayal, "Agents");
		barterPortrayal.setPortrayalForClass(
				TradeAgent.class,
				new BasicAgentPortrayal((BarterEconomy) state));

		// display.reset();
		// display.repaint();
	}
}
