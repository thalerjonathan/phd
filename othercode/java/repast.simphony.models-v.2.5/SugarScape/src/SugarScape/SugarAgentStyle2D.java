package SugarScape;

import java.awt.Color;

import repast.simphony.visualizationOGL2D.DefaultStyleOGL2D;

/**
 * The 2D style class for sugar agents.   
 *
 * @author Eric Tatara
 */

public class SugarAgentStyle2D extends DefaultStyleOGL2D {

	@Override
	public Color getColor(Object o) {
			return Color.RED;
	}
	
}
