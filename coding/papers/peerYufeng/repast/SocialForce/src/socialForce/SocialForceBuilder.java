package socialForce;

import repast.simphony.context.Context;
import repast.simphony.context.space.continuous.ContinuousSpaceFactory;
import repast.simphony.context.space.continuous.ContinuousSpaceFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.parameter.Parameters;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.space.continuous.SimpleCartesianAdder;
import socialForce.rendering.SocialForceToRePastTranslator;
import socialForce.scene.hall.Hall;
import socialForce.scene.museum.Museum;

public class SocialForceBuilder implements ContextBuilder<Object> {

	public final static String CONTEXT_ID = "SocialForce";
	public final static String SPACE_ID = "SocialForceSpace";
	
	public final static double SPACE_WIDTH = 1000;
	public final static double SPACE_HEIGHT = 600;
	
	private final static String HALLSCENE_PARAM_ID = "socialforce_hall_scene";
	
	@Override
	public Context<Object> build(Context<Object> context) {
		context.setId(CONTEXT_ID);

		ContinuousSpaceFactory spaceFactory = ContinuousSpaceFactoryFinder.createContinuousSpaceFactory(null);

		double[] dimensions = { SPACE_WIDTH, SPACE_HEIGHT };
		
		ContinuousSpace<Object> space = 
				spaceFactory.createContinuousSpace(
						SPACE_ID, 
						context,
						new SimpleCartesianAdder<Object>(),
						new SocialForceToRePastTranslator(),
						dimensions );

		Parameters params = RunEnvironment.getInstance().getParameters();
		boolean hallScene = (Boolean) params.getValue(HALLSCENE_PARAM_ID);
		
		if (hallScene) {
			Hall main = new Hall(space);
			main.initAgents(context);
			
			context.add(main);
		} else {
			Museum museum = new Museum(space);
			museum.initAgents(context);
		}
		
		return context;
	}
}
