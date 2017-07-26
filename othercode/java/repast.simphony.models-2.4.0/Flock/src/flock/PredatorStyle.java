package flock;

import java.awt.Color;
import java.awt.Font;

import javax.media.j3d.Shape3D;

import repast.simphony.visualization.visualization3D.AppearanceFactory;
import repast.simphony.visualization.visualization3D.ShapeFactory;
import repast.simphony.visualization.visualization3D.style.Style3D;
import repast.simphony.visualization.visualization3D.style.TaggedAppearance;
import repast.simphony.visualization.visualization3D.style.TaggedBranchGroup;

public class PredatorStyle<T> implements Style3D<T> {

  public TaggedBranchGroup getBranchGroup(T o, TaggedBranchGroup taggedGroup) {
    if (taggedGroup == null || taggedGroup.getTag() == null) {
      taggedGroup = new TaggedBranchGroup("DEFAULT");
      Shape3D sphere = ShapeFactory.createSphere(.02f, "DEFAULT");
      taggedGroup.getBranchGroup().addChild(sphere);
      return taggedGroup;
    }
    
    return null;
  }

  public float[] getRotation(T o) {
    return null;
  }
  
  public String getLabel(T o, String currentLabel) {
    return null;
  }

  public Color getLabelColor(T t, Color currentColor) {
    return null; 
  }

  public Font getLabelFont(T t, Font currentFont) {
    return null; 
  }

  public LabelPosition getLabelPosition(T o, Style3D.LabelPosition curentPosition) {
    return Style3D.LabelPosition.NORTH;
  }

  public float getLabelOffset(T t) {
    return .035f;
  }

  public TaggedAppearance getAppearance(T t, TaggedAppearance taggedAppearance, Object shapeID) {
    if (taggedAppearance == null || taggedAppearance.getTag() == null) {
      taggedAppearance = new TaggedAppearance("DEFAULT");
      AppearanceFactory.setMaterialAppearance(taggedAppearance.getAppearance(), Color.RED);
    }
    return taggedAppearance;
    
  }

  public float[] getScale(T o) {
    return null;
  }
}
