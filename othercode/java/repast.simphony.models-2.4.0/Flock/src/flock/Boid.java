package flock;

import javax.vecmath.Vector3d;

/**
 * Boid stores a velocity vector that is updated by implementing classes.
 * 
 * Adopted from the C# Swarm model by Daniel Greenheck: 
 *   http://greenhecktech.com/2014/04/03/throwback-thursday-swarm-intelligence/
 * 
 * @author Eric Tatara
 * 
 */
public abstract class Boid{
	protected Vector3d velocity = new Vector3d();
	protected Vector3d lastPosition = new Vector3d();
	
	public Vector3d getLastPosition() {
		return lastPosition;
	}
}