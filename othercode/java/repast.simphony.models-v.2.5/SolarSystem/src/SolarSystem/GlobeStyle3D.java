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

public class GlobeStyle3D implements Style3D<Planet> {

	Texture texture;
	
	public GlobeStyle3D (){
		super();
		
		TextureLoader loader = new TextureLoader("icons/earth_1024.jpg", "RGB", new Container());
    texture = loader.getTexture();
    texture.setBoundaryModeS(Texture.WRAP);
    texture.setBoundaryModeT(Texture.WRAP);
	}
	
	public TaggedBranchGroup getBranchGroup(Planet o, TaggedBranchGroup taggedGroup) {
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
			Sphere sphere = new Sphere(0.05f, primflags,60);

			Shape3D shape = new Shape3D(sphere.getShape().getGeometry(),ap);
			shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
			shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
			shape.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
			
			taggedGroup.getBranchGroup().addChild(shape);
			
			return taggedGroup;
		}
		return null;
	}

	public float[] getRotation(Planet globe) {
		return globe.getRot(); 
	}
	public String getLabel(Planet globe, String currentLabel) {		
		//return globe.getName();
		return null;
	}
	public Color getLabelColor(Planet globe, Color currentColor) {
		return Color.YELLOW;
	}
	public Font getLabelFont(Planet globe, Font currentFont) {
		return null;
	}
	public LabelPosition getLabelPosition(Planet globe, LabelPosition curentPosition) {
		return LabelPosition.NORTH;
	}
	public float getLabelOffset(Planet globe) {
		return .75f;
	}
	public TaggedAppearance getAppearance(Planet globe, TaggedAppearance taggedAppearance, Object shapeID) {
		
//		if (taggedAppearance == null) {
//			taggedAppearance = new TaggedAppearance();
//		}
//		AppearanceFactory.setMaterialAppearance(taggedAppearance.getAppearance(), Color.blue);
		
		return taggedAppearance;
	}
	public float[] getScale(Planet globe) {
		return null;
	}
}
