package socialForce;

import repast.simphony.context.Context;
import repast.simphony.context.space.continuous.ContinuousSpaceFactory;
import repast.simphony.context.space.continuous.ContinuousSpaceFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.space.continuous.RandomCartesianAdder;

public class SocialForceBuilder implements ContextBuilder<Object> {

	public final static String CONTEXT_ID = "SocialForce";
	public final static String SPACE_ID = "SocialForceSpace";
	
	@Override
	public Context build(Context<Object> context) {
		context.setId(CONTEXT_ID);

		ContinuousSpaceFactory spaceFactory = ContinuousSpaceFactoryFinder
				.createContinuousSpaceFactory(null);
		ContinuousSpace<Object> space = spaceFactory.createContinuousSpace(
				SPACE_ID, context, new RandomCartesianAdder<Object>(),
				new repast.simphony.space.continuous.WrapAroundBorders(), 50,
				50);

		SocialForce main = new SocialForce(space);
		main.initAgents(context);
		
		context.add(main);
		
		return context;
	}
}
