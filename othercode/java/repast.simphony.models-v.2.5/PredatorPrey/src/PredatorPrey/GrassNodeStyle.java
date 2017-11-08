package PredatorPrey;


import java.awt.Color;
import java.awt.Font;

import javax.media.j3d.Shape3D;
import javax.media.j3d.Transform3D;
import javax.media.j3d.TransformGroup;
import javax.vecmath.Vector3d;
import javax.vecmath.Vector3f;

import repast.simphony.visualization.visualization3D.AppearanceFactory;
import repast.simphony.visualization.visualization3D.ShapeFactory;
import repast.simphony.visualization.visualization3D.style.Style3D;
import repast.simphony.visualization.visualization3D.style.TaggedAppearance;
import repast.simphony.visualization.visualization3D.style.TaggedBranchGroup;

/**
 * Style for grass in 3D displays
 * 
 * @author Eric Tatara
 * @author Nick Collier
 */
public class GrassNodeStyle implements Style3D<SimpleAgent> {
	
	Color tan = new Color(205, 133, 63);
	
	public TaggedBranchGroup getBranchGroup(SimpleAgent agent, TaggedBranchGroup taggedGroup) {
		
		if (taggedGroup == null || taggedGroup.getTag() == null) {
			taggedGroup = new TaggedBranchGroup("DEFAULT");
			Shape3D cube = ShapeFactory.createCube(.03f, "DEFAULT");

			Transform3D trans = new Transform3D();
   		trans.set(new Vector3f(0, 0, -.05f));
   		trans.setScale(new Vector3d(1, 1, 0.5));
  		TransformGroup grp = new TransformGroup(trans);

  		grp.addChild(cube);
		  taggedGroup.getBranchGroup().addChild(grp);

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
		
		Grass grass = (Grass)agent;
		if (grass.isAlive())
			AppearanceFactory.setMaterialAppearance(taggedAppearance.getAppearance(), Color.green);
		else
			AppearanceFactory.setMaterialAppearance(taggedAppearance.getAppearance(), tan);
		
//		AppearanceFactory.setTransparentAppearance(taggedAppearance.getAppearance(), TransparencyAttributes.SCREEN_DOOR, 0.5f);
		
		return taggedAppearance;
		
	}
	
	public float[] getScale(SimpleAgent o) {
		return null;
	}
}
