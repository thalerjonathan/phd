package barter.display;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.Arc2D;
import java.awt.geom.Ellipse2D;
import java.util.HashMap;
import java.util.Map;

import sim.display.GUIState;
import sim.portrayal.DrawInfo2D;
import sim.portrayal.Inspector;
import sim.portrayal.LocationWrapper;
import sim.portrayal.SimplePortrayal2D;
import barter.BarterEconomy;
import barter.BarterParams;
import barter.TradeAgent;
import barter.Util;

/**
 * Handles the portraying of an agent.
 * 
 * @NotThreadSafe This class is NOT thread safe!
 */
public class BasicAgentPortrayal extends SimplePortrayal2D {

	private static final long serialVersionUID = 7930637379836254748L;

	// private TradeAgent agent;
	private double size;
	private TradeAgent selected;
	private BarterEconomy economy;
	private static final Palette scorePalette =
		Palette.newContinuousPalette(20, new Color(1.0f, 1.0f, 1.0f),
					new Color(0, 0, 0));
	private static final Stroke STROKE_1P = new BasicStroke(1.0f);
	private static final Stroke STROKE_2P = new BasicStroke(2.0f);
	private BarterParams params;
	private Map<Object,double[]> scores;

	public BasicAgentPortrayal(BarterEconomy economy) {
		size = 260.0 / Math.sqrt(economy.getParams().getTotalAgents());
		selected = null;
		params = null;
		this.economy = economy;
	}
	
	private void checkChangedParams(BarterParams p) {
		if(p != null && !p.equals(params)) {
//			System.out.println("Updating params!");
			params = p.clone();
			scores = new HashMap<Object,double[]>();
		}
	}
	
	private double updateScore(BarterParams p, TradeAgent agent) {
		checkChangedParams(p);
		// Cast agent to object to use object's id instead of TradeAgent's hashCode().
		Object agentObj = (Object) agent;
		double[] scoreSeries = scores.get(agentObj);
		int reproducePeriod = params.getReproducePeriod();

		if(scoreSeries == null) {
			scoreSeries = new double[reproducePeriod + 1];
			scores.put(agentObj, scoreSeries);
		}
		
		// Index for the circular array.
		int idx = (int) (economy.schedule.getSteps() % reproducePeriod);
		
		// Do a full summation every reproducePeriod to avoid rounding errors.
		if(idx == 0) {
			double sum = 0.0;
			for(int i = 1; i <= reproducePeriod; i++) {
				sum += scoreSeries[i];
			}
			// Store the sum in the first element.
			scoreSeries[0] = sum;
		}
		
		// Update the circular array...
		double curScore = agent.getScore();
		double oldScore = scoreSeries[idx + 1];
		scoreSeries[idx + 1] = curScore;
		// ... and then the sum.
		scoreSeries[0] += curScore - oldScore;

		return scoreSeries[0] / reproducePeriod;
	}
	
	@Override
	public void draw(Object o, Graphics2D g, DrawInfo2D info) {
		if(true) {
		g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
		 
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		if(o == null || g == null) {
			return;
		}
		// TODO: MASON is buggy; we occasionally get a width that is 0.
		info.draw.height = info.draw.width = Math.max(info.draw.width, info.draw.height);
		double width = info.draw.width * size;
		
		if(width < 1.0) {
			int a;
			//			System.err.println("Naughtiness; DrawInfo2D: " + info);
			//			Thread.dumpStack();
		}

		double height = width;
		int x = (int) (info.draw.x - width / 2);
		int y = (int) (info.draw.y - height / 2);
		TradeAgent agent = (TradeAgent) o;

		double score = updateScore(economy.getParams(), agent);
		double palVal = Util.threshold(0.0, score, Util.ALMOST_ONE);
		drawMiniPie(agent.getPrices(), g, info);
		if(o == selected) {
			double sw = width / 4.0;
			double sRadius = width * 2.0;
			double sx = (info.draw.x - sRadius / 2.0);
			double sy = (info.draw.y - sRadius / 2.0);
			g.setColor(Color.white);
			g.setStroke(new BasicStroke((float) sw));
			g.draw(new Ellipse2D.Double(sx, sy, sRadius, sRadius));
		}
		g.setColor(scorePalette.getColor(palVal));
		g.fillOval(x, y, (int) width, (int) height);
		g.setColor(Color.white);
		g.setStroke(STROKE_1P);
		g.drawOval(x, y, (int) width, (int) height);
		x = (int) (info.draw.x - width / 4);
		y = (int) (info.draw.y - height / 4);
		g.setColor(Palette.getIndexColor(agent.getProduceGood()));
		g.fillOval(x, y, (int) (width / 2.0), (int) (height / 2.0));
		g.setColor(Color.black);
		g.drawOval(x, y, (int) (width / 2.0), (int) (height / 2.0));
		}
	}

	private void drawMiniPie(double[] vals, Graphics2D g, DrawInfo2D info) {
		double sum = 0.0;
		final double haloSize = 1.7;
		double width = info.draw.width * size * haloSize;
		double height = width;
		for(double e : vals) {
			sum += e;
		}
		
		double x = (info.draw.x - width / 2);
		double y = (info.draw.y - height / 2);
/*		double x = info.draw.x;
		double y = info.draw.y;*/
		double endSlice = 0.0;
		Paint p = g.getPaint();
		g.setStroke(STROKE_1P);
		for(int i = 0; i < vals.length; i++) {
			double sliceDeg = vals[i] / sum * 360; 
			Shape slice = new Arc2D.Double(x, y, width, height, -endSlice + 90, -sliceDeg, Arc2D.PIE);
			Color sliceColor = Palette.getIndexColor(i);
			g.setColor(sliceColor);
			g.fill(slice);
			g.setColor(Color.black);
			g.draw(slice);
			endSlice += sliceDeg;
		}
		g.setPaint(p);
	}
	
    /** If drawing area intersects selected area, add last portrayed object to the bag */
	@Override
	public boolean hitObject(Object object, DrawInfo2D range) {
		final double SLOP = 1.0;  // need a little extra area to hit objects
		final double width = range.draw.width * size * 2.0;
		final double height = width;
		Ellipse2D.Double ellipse = new Ellipse2D.Double( range.draw.x-width/2-SLOP, range.draw.y-height/2-SLOP, width+SLOP*2,height+SLOP*2 );
		boolean hit = ellipse.intersects(range.clip.x, range.clip.y, range.clip.width, range.clip.height);
		return hit;
	}

    @Override
	public Inspector getInspector(LocationWrapper wrapper, GUIState state) {
    	return super.getInspector(wrapper, state);
    }
 	/*Provide an inspector for an object.
java.lang.String 	getName(LocationWrapper wrapper)
     Returns a static, one-line name for the given object that is useful for a human to distinguish it from other objects.
java.lang.String 	getStatus(LocationWrapper wrapper)
     Returns a simple, informative one-line description of the current status of the object, which may change at any time to reflect how the object is changing.
void 	move(LocationWrapper wrapper, java.awt.geom.Dimension2D distance)
*/
	@Override
	public boolean setSelected(LocationWrapper wrapper, boolean selected) {
		super.setSelected(wrapper, selected);
		
		if(selected) {
			this.selected = (TradeAgent) wrapper.getObject();
		} else {
			this.selected = null;
		}
		return true;
	}
}
