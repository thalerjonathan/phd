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
import repast.simphony.space.SpatialException;
import repast.simphony.space.grid.Grid;
import repast.simphony.space.grid.GridPoint;

@GeneratedByReLogoBuilder
@SuppressWarnings({"unused","rawtypes","unchecked"})
public class ReLogoTurtle extends BaseTurtle{

	/**
	 * Makes a number of new heroOrCowards and then executes a set of commands on the
	 * created heroOrCowards.
	 * 
	 * @param number
	 *            a number
	 * @param closure
	 *            a set of commands
	 * @return created heroOrCowards
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.HeroOrCoward")
	public AgentSet<herosandcowards.relogo.HeroOrCoward> hatchHeroOrCowards(int number, Closure closure) {
		AgentSet<herosandcowards.relogo.HeroOrCoward> result = new AgentSet<>();
		AgentSet<Turtle> createResult = this.hatch(number,closure,"HeroOrCoward");
		for (Turtle t : createResult){
			if (t instanceof herosandcowards.relogo.HeroOrCoward){
				result.add((herosandcowards.relogo.HeroOrCoward)t);
			}
		} 
		return result;
	}

	/**
	 * Makes a number of new heroOrCowards and then executes a set of commands on the
	 * created heroOrCowards.
	 * 
	 * @param number
	 *            a number
	 * @param closure
	 *            a set of commands
	 * @return created heroOrCowards
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.HeroOrCoward")
	public AgentSet<herosandcowards.relogo.HeroOrCoward> hatchHeroOrCowards(int number) {
		return hatchHeroOrCowards(number,null);
	}

	/**
	 * Returns an agentset of heroOrCowards from the patch of the caller.
	 * 
	 * @return agentset of heroOrCowards from the caller's patch
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.HeroOrCoward")
	public AgentSet<herosandcowards.relogo.HeroOrCoward> heroOrCowardsHere(){
	  Grid grid = getMyObserver().getGrid();
	  GridPoint gridPoint = grid.getLocation(this);
	  AgentSet<herosandcowards.relogo.HeroOrCoward> result = new AgentSet<herosandcowards.relogo.HeroOrCoward>();
	  for (Turtle t : Utility.getTurtlesOnGridPoint(gridPoint,getMyObserver(),"heroOrCoward")){
			if (t instanceof herosandcowards.relogo.HeroOrCoward)
			result.add((herosandcowards.relogo.HeroOrCoward)t);
		}
		return result;
	}

	/**
	 * Returns the agentset of heroOrCowards on the patch at the direction (ndx, ndy) from the
	 * caller.
	 * 
	 * @param nX
	 *            a number
	 * @param nY
	 *            a number
	 * @returns agentset of heroOrCowards at the direction (nX, nY) from the caller
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.HeroOrCoward")
	public AgentSet<herosandcowards.relogo.HeroOrCoward> heroOrCowardsAt(Number nX, Number nY){
		double dx = nX.doubleValue();
		double dy = nY.doubleValue();
		double[] displacement = {dx,dy};

		try{
		GridPoint gridPoint = Utility.getGridPointAtDisplacement(getTurtleLocation(),displacement,getMyObserver());
		AgentSet<herosandcowards.relogo.HeroOrCoward> result = new AgentSet<herosandcowards.relogo.HeroOrCoward>();						
		for (Turtle t : Utility.getTurtlesOnGridPoint(gridPoint,getMyObserver(),"heroOrCoward")){
			if (t instanceof herosandcowards.relogo.HeroOrCoward)
			result.add((herosandcowards.relogo.HeroOrCoward)t);
		}
		return result;
		}
		catch(SpatialException e){
			return new AgentSet<herosandcowards.relogo.HeroOrCoward>();
		}
	}

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
	 * Makes a number of new userTurtles and then executes a set of commands on the
	 * created userTurtles.
	 * 
	 * @param number
	 *            a number
	 * @param closure
	 *            a set of commands
	 * @return created userTurtles
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserTurtle")
	public AgentSet<herosandcowards.relogo.UserTurtle> hatchUserTurtles(int number, Closure closure) {
		AgentSet<herosandcowards.relogo.UserTurtle> result = new AgentSet<>();
		AgentSet<Turtle> createResult = this.hatch(number,closure,"UserTurtle");
		for (Turtle t : createResult){
			if (t instanceof herosandcowards.relogo.UserTurtle){
				result.add((herosandcowards.relogo.UserTurtle)t);
			}
		} 
		return result;
	}

	/**
	 * Makes a number of new userTurtles and then executes a set of commands on the
	 * created userTurtles.
	 * 
	 * @param number
	 *            a number
	 * @param closure
	 *            a set of commands
	 * @return created userTurtles
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserTurtle")
	public AgentSet<herosandcowards.relogo.UserTurtle> hatchUserTurtles(int number) {
		return hatchUserTurtles(number,null);
	}

	/**
	 * Returns an agentset of userTurtles from the patch of the caller.
	 * 
	 * @return agentset of userTurtles from the caller's patch
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserTurtle")
	public AgentSet<herosandcowards.relogo.UserTurtle> userTurtlesHere(){
	  Grid grid = getMyObserver().getGrid();
	  GridPoint gridPoint = grid.getLocation(this);
	  AgentSet<herosandcowards.relogo.UserTurtle> result = new AgentSet<herosandcowards.relogo.UserTurtle>();
	  for (Turtle t : Utility.getTurtlesOnGridPoint(gridPoint,getMyObserver(),"userTurtle")){
			if (t instanceof herosandcowards.relogo.UserTurtle)
			result.add((herosandcowards.relogo.UserTurtle)t);
		}
		return result;
	}

	/**
	 * Returns the agentset of userTurtles on the patch at the direction (ndx, ndy) from the
	 * caller.
	 * 
	 * @param nX
	 *            a number
	 * @param nY
	 *            a number
	 * @returns agentset of userTurtles at the direction (nX, nY) from the caller
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserTurtle")
	public AgentSet<herosandcowards.relogo.UserTurtle> userTurtlesAt(Number nX, Number nY){
		double dx = nX.doubleValue();
		double dy = nY.doubleValue();
		double[] displacement = {dx,dy};

		try{
		GridPoint gridPoint = Utility.getGridPointAtDisplacement(getTurtleLocation(),displacement,getMyObserver());
		AgentSet<herosandcowards.relogo.UserTurtle> result = new AgentSet<herosandcowards.relogo.UserTurtle>();						
		for (Turtle t : Utility.getTurtlesOnGridPoint(gridPoint,getMyObserver(),"userTurtle")){
			if (t instanceof herosandcowards.relogo.UserTurtle)
			result.add((herosandcowards.relogo.UserTurtle)t);
		}
		return result;
		}
		catch(SpatialException e){
			return new AgentSet<herosandcowards.relogo.UserTurtle>();
		}
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
	 * Makes a directed userLink from a turtle to the caller then executes a set of
	 * commands on the created userLink.
	 * 
	 * @param t
	 *            a turtle
	 * @param closure
	 *            a set of commands
	 * @return created userLink
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public herosandcowards.relogo.UserLink createUserLinkFrom(Turtle t, Closure closure){
		herosandcowards.relogo.UserLink link = (herosandcowards.relogo.UserLink)this.getMyObserver().getNetwork("UserLink").addEdge(t,this);
		if (closure != null){
			this.ask(link,closure);
		}
		return link;
	}

	/**
	 * Makes a directed userLink from a turtle to the caller.
	 * 
	 * @param t
	 *            a turtle
	 * @return created userLink
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public herosandcowards.relogo.UserLink createUserLinkFrom(Turtle t){
			return createUserLinkFrom(t,null);
	}

	/**
	 * Makes directed userLinks from a collection to the caller then executes a set
	 * of commands on the created userLinks.
	 * 
	 * @param a
	 *            a collection
	 * @param closure
	 *            a set of commands
	 * @return created userLinks
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public AgentSet<herosandcowards.relogo.UserLink> createUserLinksFrom(Collection<? extends Turtle> a, Closure closure){
		AgentSet<herosandcowards.relogo.UserLink> links = new AgentSet<herosandcowards.relogo.UserLink>();
		for(Turtle t : a){
			links.add((herosandcowards.relogo.UserLink)this.getMyObserver().getNetwork("UserLink").addEdge(t,this));
		}
		if (closure != null){
			this.ask(links,closure);
		}
		return links;
	}

	/**
	 * Makes directed userLinks from a collection to the caller.
	 * 
	 * @param a
	 *            a collection
	 * @return created userLinks
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public AgentSet<herosandcowards.relogo.UserLink> createUserLinksFrom(Collection<? extends Turtle> a){
		return createUserLinksFrom(a,null);
	}

	/**
	 * Makes a directed userLink to a turtle from the caller then executes a set of
	 * commands on the created userLink.
	 * 
	 * @param t
	 *            a turtle
	 * @param closure
	 *            a set of commands
	 * @return created userLink
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public herosandcowards.relogo.UserLink createUserLinkTo(Turtle t, Closure closure){
		herosandcowards.relogo.UserLink link = (herosandcowards.relogo.UserLink)this.getMyObserver().getNetwork("UserLink").addEdge(this,t);
		if (closure != null){
			this.ask(link,closure);
		}
		return link;
	}

	/**
	 * Makes a directed userLink to a turtle from the caller.
	 * 
	 * @param t
	 *            a turtle
	 * @return created userLink
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public herosandcowards.relogo.UserLink createUserLinkTo(Turtle t){
			return createUserLinkTo(t,null);
	}

	/**
	 * Makes directed userLinks to a collection from the caller then executes a set
	 * of commands on the created userLinks.
	 * 
	 * @param a
	 *            a collection
	 * @param closure
	 *            a set of commands
	 * @return created userLinks
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public AgentSet<herosandcowards.relogo.UserLink> createUserLinksTo(Collection<? extends Turtle> a, Closure closure){
		AgentSet<herosandcowards.relogo.UserLink> links = new AgentSet<herosandcowards.relogo.UserLink>();
		for(Object t : a){
			if (t instanceof Turtle){
				links.add((herosandcowards.relogo.UserLink)this.getMyObserver().getNetwork("UserLink").addEdge(this,(Turtle)t));
			}
		}
		if (closure != null){
			this.ask(links,closure);
		}
		return links;
	}

	/**
	 * Makes directed userLinks to a collection from the caller.
	 * 
	 * @param a
	 *            a collection
	 * @return created userLinks
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public AgentSet<herosandcowards.relogo.UserLink> createUserLinksTo(Collection<? extends Turtle> a){
		return createUserLinksTo(a,null);
	}

	/**
	 * Queries if there is a directed userLink from a turtle to the caller.
	 * 
	 * @param t
	 *            a turtle
	 * @return true or false based on whether there is a directed userLink from
	 *         turtle t to the caller
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public boolean inUserLinkNeighborQ(Turtle t){
		return this.getMyObserver().getNetwork("UserLink").isPredecessor(t, this);
	}

	/**
	 * Returns the agentset with directed userLinks to the caller.
	 * 
	 * @return agentset with directed userLinks to the caller
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public AgentSet inUserLinkNeighbors(){
		AgentSet result = new AgentSet();
		for(Object o : this.getMyObserver().getNetwork("UserLink").getPredecessors(this)){
			result.add(o);
		}
		return result;
	}

	/**
	 * Returns the directed userLink from a turtle to the caller.
	 * 
	 * @param t
	 *            a turtle
	 * @return directed userLink from turtle t to the caller
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public herosandcowards.relogo.UserLink inUserLinkFrom(Turtle t){
		return (herosandcowards.relogo.UserLink)this.getMyObserver().getNetwork("UserLink").getEdge(t,this);
	}

	/**
	 * Returns an agentset of directed userLinks from other turtles to the caller.
	 * 
	 * @return agentset of directed userLinks from other turtles to the caller
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public AgentSet<herosandcowards.relogo.UserLink> myInUserLinks(){
		AgentSet<herosandcowards.relogo.UserLink> result = new AgentSet<herosandcowards.relogo.UserLink>();
		for(Object o :  this.getMyObserver().getNetwork("UserLink").getInEdges(this)){
			if (o instanceof herosandcowards.relogo.UserLink){
				result.add((herosandcowards.relogo.UserLink) o);
			}
		}
		return result;
	}

	/**
	 * Returns an agentset of directed userLinks to other turtles from the caller.
	 * 
	 * @return agentset of directed userLinks to other turtles from the caller
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public AgentSet<herosandcowards.relogo.UserLink> myOutUserLinks(){
		AgentSet<herosandcowards.relogo.UserLink> result = new AgentSet<herosandcowards.relogo.UserLink>();
		for(Object o :  this.getMyObserver().getNetwork("UserLink").getOutEdges(this)){
			if (o instanceof herosandcowards.relogo.UserLink){
				result.add((herosandcowards.relogo.UserLink) o);
			}
		}
		return result;
	}

	/**
	 * Queries if there is a directed userLink to a turtle from the caller.
	 * 
	 * @param t
	 *            a turtle
	 * @return true or false based on whether there is a directed userLink to
	 *         turtle t from the caller
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public boolean outUserLinkNeighborQ(Turtle t){
		return this.getMyObserver().getNetwork("UserLink").isPredecessor(this, t);
	}

	/**
	 * Returns the agentset with directed userLinks from the caller.
	 * 
	 * @return agentset with directed userLinks from the caller
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public AgentSet outUserLinkNeighbors(){
		AgentSet result = new AgentSet();
		for(Object o : this.getMyObserver().getNetwork("UserLink").getSuccessors(this)){
			result.add(o);
		}
		return result;
	}

	/**
	 * Returns the directed userLink to a turtle from the caller.
	 * 
	 * @param t
	 *            a turtle
	 * @return directed userLink to turtle t from the caller
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public herosandcowards.relogo.UserLink outUserLinkTo(Turtle t){
		return (herosandcowards.relogo.UserLink)this.getMyObserver().getNetwork("UserLink").getEdge(this, t);
	}

	/**
	 * Reports true if there is a userLink connecting t and the caller.
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public boolean userLinkNeighborQ(Turtle t){
		return this.getMyObserver().getNetwork("UserLink").isAdjacent(this, t);
	}

	/**
	 * Returns the agentset of all turtles found at the other end of
	 * userLinks connected to the calling turtle.
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public AgentSet userLinkNeighbors(){
		AgentSet result = new AgentSet();
		for(Object o : this.getMyObserver().getNetwork("UserLink").getAdjacent(this)){
			result.add(o);
		}
		return result;
	}

	/**
	 * Returns an agentset of the caller's userLinks.
	 * 
	 * @return agentset of the caller's userLinks
	 */
	@ReLogoBuilderGeneratedFor("herosandcowards.relogo.UserLink")
	public AgentSet<herosandcowards.relogo.UserLink> myUserLinks(){
		AgentSet<herosandcowards.relogo.UserLink> result = new AgentSet<herosandcowards.relogo.UserLink>();
		for(Object o : this.getMyObserver().getNetwork("UserLink").getEdges(this)){
			if (o instanceof herosandcowards.relogo.UserLink){
				result.add((herosandcowards.relogo.UserLink)o);
			}
		}
		return result;
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