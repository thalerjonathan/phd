package SugarScape;

import java.awt.Color;
import java.awt.Font;

import javax.media.j3d.Shape3D;

import repast.simphony.visualization.visualization3D.AppearanceFactory;
import repast.simphony.visualization.visualization3D.ShapeFactory;
import repast.simphony.visualization.visualization3D.style.Style3D;
import repast.simphony.visualization.visualization3D.style.TaggedAppearance;
import repast.simphony.visualization.visualization3D.style.TaggedBranchGroup;

/**
 * The 3D style class for sugar agents.   
 * The style is a red sphere. 
 *
 * @author Eric Tatara
 * @author Nick Collier
 * @version 
 */

public class SugarAgentStyle implements Style3D<SugarAgent> {

	public TaggedBranchGroup getBranchGroup(SugarAgent agent, TaggedBranchGroup taggedGroup) {

		if (taggedGroup == null || taggedGroup.getTag() == null) {
			taggedGroup = new TaggedBranchGroup("DEFAULT");
			Shape3D sphere = ShapeFactory.createSphere(.03f, "DEFAULT");
			sphere.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
			taggedGroup.getBranchGroup().addChild(sphere);

			return taggedGroup;
		}
		return null;
	}

	public float[] getRotation(SugarAgent agent) {
		return null;
	}

	public String getLabel(SugarAgent agent, String currentLabel) {
		return null; 
	}

	public Color getLabelColor(SugarAgent agent, Color currentColor) {
		return Color.YELLOW;
	}

	public Font getLabelFont(SugarAgent agent, Font currentFont) {
		return null;
	}

	public LabelPosition getLabelPosition(SugarAgent agent, LabelPosition curentPosition) {
		return LabelPosition.NORTH;
	}

	public float getLabelOffset(SugarAgent agent) {
		return .035f;
	}

	public TaggedAppearance getAppearance(SugarAgent agent, TaggedAppearance taggedAppearance, Object shapeID) {
		if (taggedAppearance == null) {
			taggedAppearance = new TaggedAppearance();
		}

		if (agent instanceof SugarAgent)
		  AppearanceFactory.setMaterialAppearance(taggedAppearance.getAppearance(), Color.red);
		
		return taggedAppearance;
	}

	public float[] getScale(SugarAgent agent) {
		return null;
	}
}
