package herosandcowards;

import static repast.simphony.relogo.Utility.*;
import static repast.simphony.relogo.UtilityG.*;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import groovy.lang.Closure;
import repast.simphony.relogo.*;
import repast.simphony.relogo.builder.GeneratedByReLogoBuilder;
import repast.simphony.relogo.builder.ReLogoBuilderGeneratedFor;

@GeneratedByReLogoBuilder
@SuppressWarnings({"unused","rawtypes","unchecked"})
public class ReLogoLink<T> extends BaseLink<T>	{

	/**
	 * Returns an agentset of heroOrCowards on a given patch.
	 * 
	 * @param p
	 *            a patch
	 * @return agentset of heroOrCowards on patch p
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.HeroOrCoward")
	public AgentSet<herosandcowards.relogo.HeroOrCoward> heroOrCowardsOn(Patch p){
		AgentSet<herosandcowards.relogo.HeroOrCoward> result = new AgentSet<herosandcowards.relogo.HeroOrCoward>();						
		for (Turtle t : Utility.getTurtlesOnGridPoint(p.getGridLocation(),getMyObserver(),"heroOrCoward")){
			if (t instanceof herosandcowards.relogo.HeroOrCoward)
			result.add((herosandcowards.relogo.HeroOrCoward)t);
		}
		return result;
	}

	/**
	 * Returns an agentset of heroOrCowards on the same patch as a turtle.
	 * 
	 * @param t
	 *            a turtle
	 * @return agentset of heroOrCowards on the same patch as turtle t
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.HeroOrCoward")
	public AgentSet<herosandcowards.relogo.HeroOrCoward> heroOrCowardsOn(Turtle t){
		AgentSet<herosandcowards.relogo.HeroOrCoward> result = new AgentSet<herosandcowards.relogo.HeroOrCoward>();						
		for (Turtle tt : Utility.getTurtlesOnGridPoint(Utility.ndPointToGridPoint(t.getTurtleLocation()),getMyObserver(),"heroOrCoward")){
			if (tt instanceof herosandcowards.relogo.HeroOrCoward)
			result.add((herosandcowards.relogo.HeroOrCoward)tt);
		}
		return result;
	}

	/**
	 * Returns an agentset of heroOrCowards on the patches in a collection or on the patches
	 * that a collection of turtles are.
	 * 
	 * @param a
	 *            a collection
	 * @return agentset of heroOrCowards on the patches in collection a or on the patches
	 *         that collection a turtles are
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.HeroOrCoward")
	public AgentSet<herosandcowards.relogo.HeroOrCoward> heroOrCowardsOn(Collection c){

		if (c == null || c.isEmpty()){
			return new AgentSet<herosandcowards.relogo.HeroOrCoward>();
		}

		Set<herosandcowards.relogo.HeroOrCoward> total = new HashSet<herosandcowards.relogo.HeroOrCoward>();
		if (c.iterator().next() instanceof Turtle){
			for (Object o : c){
				if (o instanceof Turtle){
					Turtle t = (Turtle) o;
					total.addAll(heroOrCowardsOn(t));
				}
			}
		}
		else {
			for (Object o : c){
				if (o instanceof Patch){
					Patch p = (Patch) o;
					total.addAll(heroOrCowardsOn(p));
				}
			}
		}
		return new AgentSet<herosandcowards.relogo.HeroOrCoward>(total);
	}

	/**
	 * Queries if object is a heroOrCoward.
	 * 
	 * @param o
	 *            an object
	 * @return true or false based on whether the object is a heroOrCoward
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.HeroOrCoward")
	public boolean isHeroOrCowardQ(Object o){
		return (o instanceof herosandcowards.relogo.HeroOrCoward);
	}

	/**
	 * Returns the heroOrCoward with the given who number.
	 * 
	 * @param number
	 *            a number
	 * @return turtle number
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.HeroOrCoward")
	public herosandcowards.relogo.HeroOrCoward heroOrCoward(Number number){
		Turtle turtle = Utility.turtleU(number.intValue(), getMyObserver());
		if (turtle instanceof herosandcowards.relogo.HeroOrCoward)
			return (herosandcowards.relogo.HeroOrCoward) turtle;
		return null;
	}

	/**
	 * Returns an agentset containing all heroOrCowards.
	 * 
	 * @return agentset of all heroOrCowards
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.HeroOrCoward")
	public AgentSet<herosandcowards.relogo.HeroOrCoward> heroOrCowards(){
		AgentSet<herosandcowards.relogo.HeroOrCoward> a = new AgentSet<herosandcowards.relogo.HeroOrCoward>();
		for (Object e : this.getMyObserver().getContext().getObjects(herosandcowards.relogo.HeroOrCoward.class)) {
			if (e instanceof herosandcowards.relogo.HeroOrCoward){
				a.add((herosandcowards.relogo.HeroOrCoward)e);
			}
		}
		return a;
	}

	/**
	 * Returns an agentset of userTurtles on a given patch.
	 * 
	 * @param p
	 *            a patch
	 * @return agentset of userTurtles on patch p
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserTurtle")
	public AgentSet<herosandcowards.relogo.UserTurtle> userTurtlesOn(Patch p){
		AgentSet<herosandcowards.relogo.UserTurtle> result = new AgentSet<herosandcowards.relogo.UserTurtle>();						
		for (Turtle t : Utility.getTurtlesOnGridPoint(p.getGridLocation(),getMyObserver(),"userTurtle")){
			if (t instanceof herosandcowards.relogo.UserTurtle)
			result.add((herosandcowards.relogo.UserTurtle)t);
		}
		return result;
	}

	/**
	 * Returns an agentset of userTurtles on the same patch as a turtle.
	 * 
	 * @param t
	 *            a turtle
	 * @return agentset of userTurtles on the same patch as turtle t
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserTurtle")
	public AgentSet<herosandcowards.relogo.UserTurtle> userTurtlesOn(Turtle t){
		AgentSet<herosandcowards.relogo.UserTurtle> result = new AgentSet<herosandcowards.relogo.UserTurtle>();						
		for (Turtle tt : Utility.getTurtlesOnGridPoint(Utility.ndPointToGridPoint(t.getTurtleLocation()),getMyObserver(),"userTurtle")){
			if (tt instanceof herosandcowards.relogo.UserTurtle)
			result.add((herosandcowards.relogo.UserTurtle)tt);
		}
		return result;
	}

	/**
	 * Returns an agentset of userTurtles on the patches in a collection or on the patches
	 * that a collection of turtles are.
	 * 
	 * @param a
	 *            a collection
	 * @return agentset of userTurtles on the patches in collection a or on the patches
	 *         that collection a turtles are
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserTurtle")
	public AgentSet<herosandcowards.relogo.UserTurtle> userTurtlesOn(Collection c){

		if (c == null || c.isEmpty()){
			return new AgentSet<herosandcowards.relogo.UserTurtle>();
		}

		Set<herosandcowards.relogo.UserTurtle> total = new HashSet<herosandcowards.relogo.UserTurtle>();
		if (c.iterator().next() instanceof Turtle){
			for (Object o : c){
				if (o instanceof Turtle){
					Turtle t = (Turtle) o;
					total.addAll(userTurtlesOn(t));
				}
			}
		}
		else {
			for (Object o : c){
				if (o instanceof Patch){
					Patch p = (Patch) o;
					total.addAll(userTurtlesOn(p));
				}
			}
		}
		return new AgentSet<herosandcowards.relogo.UserTurtle>(total);
	}

	/**
	 * Queries if object is a userTurtle.
	 * 
	 * @param o
	 *            an object
	 * @return true or false based on whether the object is a userTurtle
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserTurtle")
	public boolean isUserTurtleQ(Object o){
		return (o instanceof herosandcowards.relogo.UserTurtle);
	}

	/**
	 * Returns the userTurtle with the given who number.
	 * 
	 * @param number
	 *            a number
	 * @return turtle number
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserTurtle")
	public herosandcowards.relogo.UserTurtle userTurtle(Number number){
		Turtle turtle = Utility.turtleU(number.intValue(), getMyObserver());
		if (turtle instanceof herosandcowards.relogo.UserTurtle)
			return (herosandcowards.relogo.UserTurtle) turtle;
		return null;
	}

	/**
	 * Returns an agentset containing all userTurtles.
	 * 
	 * @return agentset of all userTurtles
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserTurtle")
	public AgentSet<herosandcowards.relogo.UserTurtle> userTurtles(){
		AgentSet<herosandcowards.relogo.UserTurtle> a = new AgentSet<herosandcowards.relogo.UserTurtle>();
		for (Object e : this.getMyObserver().getContext().getObjects(herosandcowards.relogo.UserTurtle.class)) {
			if (e instanceof herosandcowards.relogo.UserTurtle){
				a.add((herosandcowards.relogo.UserTurtle)e);
			}
		}
		return a;
	}

	/**
	 * Queries if object is a userLink.
	 * 
	 * @param o
	 *            an object
	 * @return true or false based on whether the object is a userLink
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public boolean isUserLinkQ(Object o){
		return (o instanceof herosandcowards.relogo.UserLink);
	}

	/**
	 * Returns an agentset containing all userLinks.
	 * 
	 * @return agentset of all userLinks
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public AgentSet<herosandcowards.relogo.UserLink> userLinks(){
		AgentSet<herosandcowards.relogo.UserLink> a = new AgentSet<herosandcowards.relogo.UserLink>();
		for (Object e : this.getMyObserver().getContext().getObjects(herosandcowards.relogo.UserLink.class)) {
			if (e instanceof herosandcowards.relogo.UserLink){
				a.add((herosandcowards.relogo.UserLink)e);
			}
		}
		return a;
	}

	/**
	 * Returns the userLink between two turtles.
	 * 
	 * @param oneEnd
	 *            an integer
	 * @param otherEnd
	 *            an integer
	 * @return userLink between two turtles
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public herosandcowards.relogo.UserLink userLink(Number oneEnd, Number otherEnd) {
		return (herosandcowards.relogo.UserLink)(this.getMyObserver().getNetwork("UserLink").getEdge(turtle(oneEnd),turtle(otherEnd)));
	}

	/**
	 * Returns the userLink between two turtles.
	 * 
	 * @param oneEnd
	 *            a turtle
	 * @param otherEnd
	 *            a turtle
	 * @return userLink between two turtles
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public herosandcowards.relogo.UserLink userLink(Turtle oneEnd, Turtle otherEnd) {
		return userLink(oneEnd.getWho(), otherEnd.getWho());
	}

	/**
	 * Returns the value of the global variable heroesDistribution.
	 *
	 * @return the value of the global variable heroesDistribution
	 */
	@ReLogoBuilderGeneratedFor("global: heroesDistribution")
	public Object getHeroesDistribution(){
		return repast.simphony.relogo.ReLogoModel.getInstance().getModelParam("heroesDistribution");
	}

	/**
	 * Sets the value of the global variable heroesDistribution.
	 *
	 * @param value
	 *            a value
	 */
	@ReLogoBuilderGeneratedFor("global: heroesDistribution")
	public void setHeroesDistribution(Object value){
		repast.simphony.relogo.ReLogoModel.getInstance().setModelParam("heroesDistribution",value);
	}

	/**
	 * Returns the value of the global variable personCount.
	 *
	 * @return the value of the global variable personCount
	 */
	@ReLogoBuilderGeneratedFor("global: personCount")
	public Object getPersonCount(){
		return repast.simphony.relogo.ReLogoModel.getInstance().getModelParam("personCount");
	}

	/**
	 * Sets the value of the global variable personCount.
	 *
	 * @param value
	 *            a value
	 */
	@ReLogoBuilderGeneratedFor("global: personCount")
	public void setPersonCount(Object value){
		repast.simphony.relogo.ReLogoModel.getInstance().setModelParam("personCount",value);
	}


}