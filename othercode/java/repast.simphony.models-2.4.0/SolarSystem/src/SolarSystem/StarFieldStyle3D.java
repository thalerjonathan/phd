package SolarSystem;

import java.awt.Color;
import java.awt.Container;
import java.awt.Font;

import javax.media.j3d.Appearance;
import javax.media.j3d.Shape3D;
import javax.media.j3d.Texture;
import javax.media.j3d.TextureAttributes;

import repast.simphony.visualization.visualization3D.style.Style3D;
import repast.simphony.visualization.visualization3D.style.TaggedAppearance;
import repast.simphony.visualization.visualization3D.style.TaggedBranchGroup;

import com.sun.j3d.utils.geometry.Primitive;
import com.sun.j3d.utils.geometry.Sphere;
import com.sun.j3d.utils.image.TextureLoader;

public class StarFieldStyle3D implements Style3D<Starfield> {

	Texture texture;
	
	public StarFieldStyle3D (){
		super();
		
		TextureLoader loader = new TextureLoader("icons/stars3.jpg", "RGB", new Container());
    texture = loader.getTexture();
    texture.setBoundaryModeS(Texture.WRAP);
    texture.setBoundaryModeT(Texture.WRAP);
	}
	
	public TaggedBranchGroup getBranchGroup(Starfield o, TaggedBranchGroup taggedGroup) {
		if (taggedGroup == null || taggedGroup.getTag() == null) {
			taggedGroup = new TaggedBranchGroup("DEFAULT");
						
			TextureAttributes texAttr = new TextureAttributes();
	    texAttr.setTextureMode(TextureAttributes.MODULATE);
	    Appearance ap = new Appearance();
	    ap.setCapability(Appearance.ALLOW_TEXTURE_WRITE);
	    ap.setCapability(Appearance.ALLOW_TEXTURE_READ);
	    ap.setTexture(texture);
	    ap.setTextureAttributes(texAttr);
			
			int primflags = Primitive.GENERATE_NORMALS_INWARD + Primitive.GENERATE_TEXTURE_COORDS;
			Sphere sphere = new Sphere(5.0f, primflags,60);

			Shape3D shape = new Shape3D(sphere.getShape().getGeometry(),ap);
			shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
			shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
			shape.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
			
			taggedGroup.getBranchGroup().addChild(shape);
			
			return taggedGroup;
		}
		return null;
	}

	public float[] getRotation(Starfield s) {
		return s.getRot(); 
	}
	public String getLabel(Starfield s, String currentLabel) {		
		return null;
	}
	public Color getLabelColor(Starfield s, Color currentColor) {
		return Color.YELLOW;
	}
	public Font getLabelFont(Starfield s, Font currentFont) {
		return null;
	}
	public LabelPosition getLabelPosition(Starfield s, LabelPosition curentPosition) {
		return LabelPosition.NORTH;
	}
	public float getLabelOffset(Starfield s) {
		return .75f;
	}
	public TaggedAppearance getAppearance(Starfield s, TaggedAppearance taggedAppearance, Object shapeID) {
		return taggedAppearance;
	}
	public float[] getScale(Starfield s) {
		return null;
	}
}
