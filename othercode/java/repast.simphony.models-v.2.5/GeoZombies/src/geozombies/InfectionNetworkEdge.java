package geozombies;

import repast.simphony.space.graph.RepastEdge;

/**
 * Infection network edge
 * 
 * @author Eric Tatara
 *
 */
public class InfectionNetworkEdge extends RepastEdge {

	public InfectionNetworkEdge(Object source, Object target){
		super(source, target, false);
	}
	
}
