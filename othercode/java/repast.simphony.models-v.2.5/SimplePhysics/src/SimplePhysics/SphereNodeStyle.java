package SimplePhysics;


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
 * 
 */
public class SphereNodeStyle implements Style3D<PhysicsAgent> {
	
	public TaggedBranchGroup getBranchGroup(PhysicsAgent agent, 
			TaggedBranchGroup taggedGroup) {
		
		if (taggedGroup == null || taggedGroup.getTag() == null) {
			taggedGroup = new TaggedBranchGroup("DEFAULT");
			// note radius = 2 * diameter
			Shape3D cube = ShapeFactory.createSphere(.5f, "DEFAULT");
		  taggedGroup.getBranchGroup().addChild(cube);

			return taggedGroup;
		}
		return null;
	}
	
	public float[] getRotation(PhysicsAgent o) {
		return o.getRot();
	}
	
	public String getLabel(PhysicsAgent o, String currentLabel) {
		return null; 
	}
	
	public Color getLabelColor(PhysicsAgent t, Color currentColor) {
		return Color.YELLOW;
	}
	
	public Font getLabelFont(PhysicsAgent t, Font currentFont) {
		return null;
	}
	
	public LabelPosition getLabelPosition(PhysicsAgent o, 
			LabelPosition curentPosition) {
		return LabelPosition.NORTH;
	}
	
	public float getLabelOffset(PhysicsAgent t) {
		return .035f;
	}
	
	public TaggedAppearance getAppearance(PhysicsAgent agent, 
			TaggedAppearance taggedAppearance, Object shapeID) {
		if (taggedAppearance == null) {
			taggedAppearance = new TaggedAppearance();
		}
		AppearanceFactory.setMaterialAppearance(taggedAppearance.getAppearance(), 
				agent.getColor());
		
		return taggedAppearance;
		
	}
	
	public float[] getScale(PhysicsAgent agent) {
		return agent.getScale();
	}
}
