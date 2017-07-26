package SimplePhysics;

import java.awt.Color;

import repast.simphony.context.Context;
import repast.simphony.context.space.physics.PhysicsSpaceFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.continuous.SimpleCartesianAdder;
import repast.simphony.space.physics.PhysicsSpace;
import repast.simphony.space.physics.RigidBodyFactory;

import com.bulletphysics.dynamics.RigidBody;

/**
 * @author Eric Tatara
 */
public class SimplePhysicsContextCreator implements ContextBuilder {

	public Context build(Context context) {

		double[] size = {1000, 1000, 1000};
		
		Parameters p = RunEnvironment.getInstance().getParameters();
		float targetParticleSize = ((Double)p.getValue("targetParticleSize")).floatValue();
		float targetParticleMass = ((Double)p.getValue("targetParticleMass")).floatValue();
		float projectileRadius = ((Double)p.getValue("projectileRadius")).floatValue();
		float projectileMass = ((Double)p.getValue("projectileMass")).floatValue();
		float projectileVelocity = ((Double)p.getValue("projectileVelocity")).floatValue();
		boolean gravityOn = (Boolean)p.getValue("gravityOn");
		boolean platformOn = (Boolean)p.getValue("platformOn");
		
		// create the physics space
		PhysicsSpace space = PhysicsSpaceFactoryFinder.createPhysicsSpaceFactory(null)
		.createPhysicsSpace("Physics Space", context, new SimpleCartesianAdder(),
						new repast.simphony.space.continuous.WrapAroundBorders(), size);
		
		if (!gravityOn)
		  space.setGravity(0, 0, 0);
		
		// 3D style size / phsyics body size = Display cell size (0.01)
		float scale = targetParticleSize * 0.01f;
		
		// build the target blocks
		int platformOffset = 400;
		for (float x=1; x<=10; x+=targetParticleSize){
			for (float y=1; y<=10; y+=targetParticleSize){
				for (float z=1; z<=10; z+=targetParticleSize){
					PhysicsAgent agent = new PhysicsAgent();
					agent.setColor(pickColor(x));
					context.add(agent);
					agent.setScale(new float[]{scale,scale,scale});
					
					float[] loc = new float[]{platformOffset+x, platformOffset+y, 
							platformOffset+z};
					
					RigidBody body = RigidBodyFactory.createCubeBody(targetParticleSize, 
							targetParticleMass, loc);	
					space.addObject(agent, body);
				}
			}
		}
				
		// create a projectile
		scale = 2* projectileRadius * 0.01f;
		Projectile projectile = new Projectile();
		projectile.setColor(Color.CYAN);
		context.add(projectile);
		projectile.setScale(new float[]{scale,scale,scale});
		float[] loc = new float[]{450,405,450f};
		RigidBody body = RigidBodyFactory.createSphereBody(projectileRadius, projectileMass, loc);		
		space.addObject(projectile, body);
		space.setLinearVelocity(projectile, -projectileVelocity,0,-projectileVelocity);
		
		// optionally create a target platform
		if (platformOn){
			PhysicsAgent platform = new PhysicsAgent();
			platform.setColor(new Color(0,100,0));
			context.add(platform);
			float offset = platformOffset - targetParticleSize / 2;
			loc = new float[]{400,offset,400};
			platform.setScale(new float[]{1,0.02f,1});
			body = RigidBodyFactory.createBoxBody(100, 2, 100, 0, loc);	
			space.addObject(platform, body);
		}
		
		return context;
	}

	private Color pickColor(float x){
		if (x<=2)
			return Color.RED;
		else if (x>2 && x<=4)
			return Color.ORANGE;
		else if (x>4 && x<=6)
			return Color.YELLOW;
		else if (x>6 && x<=8)
			return Color.GREEN;
		else return Color.BLUE;
	}
}
