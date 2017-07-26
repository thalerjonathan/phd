package Mousetrap;

import java.awt.Color;

import repast.simphony.visualization.visualization3D.AppearanceFactory;
import repast.simphony.visualization.visualization3D.style.DefaultStyle3D;
import repast.simphony.visualization.visualization3D.style.TaggedAppearance;

/**
 * @author Nick Collier
 * @version $Revision: 1.3 $ $Date: 2006/01/06 22:54:29 $
 */
public class MouseTrapNodeStyle extends DefaultStyle3D<MouseTrap> {

	public TaggedAppearance getAppearance(MouseTrap agent, TaggedAppearance taggedAppearance, Object shapeID) {
		
		if (taggedAppearance == null) {
			taggedAppearance = new TaggedAppearance("R");
			AppearanceFactory.setMaterialAppearance(taggedAppearance.getAppearance(), Color.RED);
		}

		if (agent.isTriggered() && taggedAppearance.getTag().equals("R")) {
			taggedAppearance.setTag("G");
			AppearanceFactory.setMaterialAppearance(taggedAppearance.getAppearance(), Color.GREEN);
		}

		return taggedAppearance;
	}
}
