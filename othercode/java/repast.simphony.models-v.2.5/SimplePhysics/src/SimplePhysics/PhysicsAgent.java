package SimplePhysics;

import java.awt.Color;

import javax.vecmath.AxisAngle4f;

import repast.simphony.context.Context;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.space.physics.PhysicsSpace;
import repast.simphony.util.ContextUtils;

import com.bulletphysics.linearmath.Transform;

/**
 * 
 * @author Eric Tatara
 *
 */
public class PhysicsAgent {

	public float[] scale;
	public float[] rot = new float[4];
	public Color color;
	
	@ScheduledMethod(start=1, interval=1)
	public void step(){
		
		Context context = ContextUtils.getContext(this);
		PhysicsSpace space = (PhysicsSpace)context.getProjection("Physics Space");
		
		Transform trans = space.getTransformForObject(this);
		AxisAngle4f angle = new AxisAngle4f();
		angle.set(trans.basis);
		angle.get(rot);
	}
	
	public float[] getScale() {
		return scale;
	}

	public void setScale(float[] scale) {
		this.scale = scale;
	}

	public Color getColor() {
		return color;
	}

	public void setColor(Color color) {
		this.color = color;
	}

	public float[] getRot() {
		return rot;
	}
	
}
