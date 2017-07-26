package SolarSystem;

import repast.simphony.context.Context;
import repast.simphony.context.space.continuous.ContinuousSpaceFactoryFinder;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.space.continuous.RandomCartesianAdder;

public class SolarSystemBuilder implements ContextBuilder {
	
	public Context build(Context mainContext) {
		int xdim = 100;
		int ydim = 100;
		int zdim = 100;
		 
		ContinuousSpace space = ContinuousSpaceFactoryFinder.createContinuousSpaceFactory(null)
		.createContinuousSpace("Universe", mainContext, new RandomCartesianAdder<Planet>(),
						new repast.simphony.space.continuous.StickyBorders(), xdim, ydim, zdim);
		
		
		double[] loc = {50,50,50};
		
		Star star = new Star("Sun");
		mainContext.add(star);
		space.moveTo(star, loc);
		
		Planet planet = new Planet("Earth",star);
		mainContext.add(planet);
		
		Moon moon = new Moon("Moon", planet);
		mainContext.add(moon);
		
		Starfield stars = new Starfield();
		mainContext.add(stars);
		space.moveTo(stars, loc);
		
		return mainContext;
	}
	
}
