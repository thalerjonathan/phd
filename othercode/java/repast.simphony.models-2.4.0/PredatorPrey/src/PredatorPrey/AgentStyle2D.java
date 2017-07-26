package PredatorPrey;

import java.awt.Color;

import repast.simphony.visualizationOGL2D.DefaultStyleOGL2D;

/**
 * Style for wolf and sheep in 2D displays.  Use the circle shape 
 * specified in DefaultStyle2D.  Here we change the color and size of the shape
 * depending on if the agent is a Wolf or a Sheep.
 * 
 * @author Eric Tatara
 *
 */
public class AgentStyle2D extends DefaultStyleOGL2D {

	@Override
	public Color getColor(Object o){
		
		if (o instanceof Wolf)
			return Color.DARK_GRAY;
		
		else if (o instanceof Sheep)
			return Color.WHITE;
		
		return null;
	}
	
	@Override
	public float getScale(Object o) {
		if (o instanceof Wolf)
			return 2f;
		
		else if (o instanceof Sheep)
			return 1f;
		
		return 1f;
	}
}