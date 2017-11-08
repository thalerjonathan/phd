package flock;

import java.awt.Color;
import java.awt.Font;

import javax.media.j3d.Appearance;
import javax.media.j3d.PointArray;
import javax.media.j3d.PointAttributes;
import javax.media.j3d.Shape3D;
import javax.vecmath.Point3f;

import repast.simphony.visualization.visualization3D.AppearanceFactory;
import repast.simphony.visualization.visualization3D.ShapeFactory;
import repast.simphony.visualization.visualization3D.style.Style3D;
import repast.simphony.visualization.visualization3D.style.TaggedAppearance;
import repast.simphony.visualization.visualization3D.style.TaggedBranchGroup;

import com.sun.j3d.utils.picking.PickTool;

public class PreyStyle<T> implements Style3D<T> {

  public TaggedBranchGroup getBranchGroup(T o, TaggedBranchGroup taggedGroup) {
    if (taggedGroup == null || taggedGroup.getTag() == null) {
      taggedGroup = new TaggedBranchGroup("DEFAULT");
      
      // Use a point for the Prey shape since points render faster than volume shapes.
      Point3f coords[] = new Point3f[1];
      coords[0] = new Point3f(0.0f, 0.0f, 0.0f);

      PointArray pa = new PointArray(1, PointArray.COORDINATES);
      pa.setCoordinates(0, coords);
     
      Shape3D shape = new Shape3D(pa);
      
      if (!(pa.isLive() || pa.isCompiled())) {
          PickTool.setCapabilities(shape, PickTool.INTERSECT_FULL);
        }
      
        pa.setCapability(PointArray.ALLOW_COLOR_WRITE);
        pa.setCapability(PointArray.ALLOW_NORMAL_WRITE);
      
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        shape.setUserData("DEFAULT");
      
      taggedGroup.getBranchGroup().addChild(shape);
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
      AppearanceFactory.setColoredAppearance(taggedAppearance.getAppearance(), Color.GREEN);
      
      float pointSize = 1f;
      boolean antiAlias = false;
      PointAttributes pattr = new PointAttributes(pointSize, antiAlias);
      
      taggedAppearance.getAppearance().setPointAttributes(pattr);
      
    }
    return taggedAppearance;
  }

  public float[] getScale(T o) {
    return null;
  }
}