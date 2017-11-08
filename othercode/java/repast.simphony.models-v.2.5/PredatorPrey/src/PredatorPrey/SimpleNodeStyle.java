package PredatorPrey;


import java.awt.Color;
import java.awt.Font;

import javax.media.j3d.Shape3D;

import repast.simphony.visualization.visualization3D.AppearanceFactory;
import repast.simphony.visualization.visualization3D.ShapeFactory;
import repast.simphony.visualization.visualization3D.style.Style3D;
import repast.simphony.visualization.visualization3D.style.TaggedAppearance;
import repast.simphony.visualization.visualization3D.style.TaggedBranchGroup;

/**
 * @author Eric Tatara
 * @auhtor Nick Collier
 * 
 */
public class SimpleNodeStyle implements Style3D<SimpleAgent> {

	public TaggedBranchGroup getBranchGroup(SimpleAgent agent, TaggedBranchGroup taggedGroup) {

		if (taggedGroup == null || taggedGroup.getTag() == null) {
			taggedGroup = new TaggedBranchGroup("DEFAULT");
			Shape3D sphere = ShapeFactory.createSphere(.03f, "DEFAULT");
			sphere.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
			taggedGroup.getBranchGroup().addChild(sphere);

			return taggedGroup;
		}
		return null;
	}

	public float[] getRotation(SimpleAgent o) {
		return null;
	}

	public String getLabel(SimpleAgent o, String currentLabel) {
		return null; //return currentLabel.length() > 0 ? currentLabel : String.valueOf(o.getId());
//		return o.toString();
	}

	public Color getLabelColor(SimpleAgent t, Color currentColor) {
		return Color.YELLOW;
	}

	public Font getLabelFont(SimpleAgent t, Font currentFont) {
		return null;
	}

	public LabelPosition getLabelPosition(SimpleAgent o, LabelPosition curentPosition) {
		return LabelPosition.NORTH;
	}

	public float getLabelOffset(SimpleAgent t) {
		return .035f;
	}

	public TaggedAppearance getAppearance(SimpleAgent agent, TaggedAppearance taggedAppearance, Object shapeID) {
		if (taggedAppearance == null) {
			taggedAppearance = new TaggedAppearance();
		}

		if (agent instanceof Wolf)
		  AppearanceFactory.setMaterialAppearance(taggedAppearance.getAppearance(), Color.darkGray);
		if (agent instanceof Sheep)
		  AppearanceFactory.setMaterialAppearance(taggedAppearance.getAppearance(), Color.white);
		
		return taggedAppearance;
	}

	public float[] getScale(SimpleAgent o) {
		return null;
	}
}
