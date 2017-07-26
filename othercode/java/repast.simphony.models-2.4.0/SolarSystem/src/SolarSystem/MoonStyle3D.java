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

public class MoonStyle3D implements Style3D<Moon> {

	Texture texture;
	
	public MoonStyle3D (){
		super();
		
		TextureLoader loader = new TextureLoader("icons/moon_1024.jpg", "RGB", new Container());
    texture = loader.getTexture();
    texture.setBoundaryModeS(Texture.WRAP);
    texture.setBoundaryModeT(Texture.WRAP);
	}
	
	public TaggedBranchGroup getBranchGroup(Moon moon, TaggedBranchGroup taggedGroup) {
		if (taggedGroup == null || taggedGroup.getTag() == null) {
			taggedGroup = new TaggedBranchGroup("DEFAULT");
						
			TextureAttributes texAttr = new TextureAttributes();
	    texAttr.setTextureMode(TextureAttributes.MODULATE);
	    Appearance ap = new Appearance();
	    ap.setCapability(Appearance.ALLOW_TEXTURE_WRITE);
	    ap.setCapability(Appearance.ALLOW_TEXTURE_READ);
	    ap.setTexture(texture);
	    ap.setTextureAttributes(texAttr);
			
			int primflags = Primitive.GENERATE_NORMALS + Primitive.GENERATE_TEXTURE_COORDS;
			Sphere sphere = new Sphere(0.01f, primflags,60);

			Shape3D shape = new Shape3D(sphere.getShape().getGeometry(),ap);
			shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
			shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
			shape.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
			
			taggedGroup.getBranchGroup().addChild(shape);
			
			return taggedGroup;
		}
		return null;
	}

	public float[] getRotation(Moon moon) {
		return moon.getRot(); 
	}
	public String getLabel(Moon moon, String currentLabel) {		
		//return moon.getName();
		return null;
	}
	public Color getLabelColor(Moon moon, Color currentColor) {
		return Color.YELLOW;
	}
	public Font getLabelFont(Moon moon, Font currentFont) {
		return null;
	}
	public LabelPosition getLabelPosition(Moon moon, LabelPosition curentPosition) {
		return LabelPosition.NORTH;
	}
	public float getLabelOffset(Moon moon) {
		return .75f;
	}
	public TaggedAppearance getAppearance(Moon moon, TaggedAppearance taggedAppearance, Object shapeID) {
		
//		if (taggedAppearance == null) {
//			taggedAppearance = new TaggedAppearance();
//		}
//		AppearanceFactory.setMaterialAppearance(taggedAppearance.getAppearance(), Color.blue);
		
		return taggedAppearance;
	}
	public float[] getScale(Moon moon) {
		return null;
	}
}
