package zombies.relogo

import static repast.simphony.relogo.Utility.*;
import static repast.simphony.relogo.UtilityG.*;
import repast.simphony.relogo.BasePatch;
import repast.simphony.relogo.BaseTurtle;
import repast.simphony.relogo.Plural;
import repast.simphony.relogo.Stop;
import repast.simphony.relogo.Utility;
import repast.simphony.relogo.UtilityG;
import repast.simphony.relogo.schedule.Go;
import repast.simphony.relogo.schedule.Setup;
import zombies.ReLogoTurtle;

class Zombie extends ReLogoTurtle {

	def step(){
		def winner = maxOneOf(neighbors()){
			count(humansOn(it))
		}
		
		face(winner)
		fd(0.5)
		
		if (count(humansHere()) > 0){
			label = "Brains!"
			def infectee = oneOf(humansHere())
			infect(infectee)
			createInfectionTo(infectee)
		}
		else {
			label = ""
		}
	}
	
	def infect(human){
		human.infected = true
	}
	
}
