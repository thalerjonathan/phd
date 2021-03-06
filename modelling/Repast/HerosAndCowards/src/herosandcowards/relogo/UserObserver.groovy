package herosandcowards.relogo

import static repast.simphony.relogo.Utility.*;
import static repast.simphony.relogo.UtilityG.*;
import repast.simphony.relogo.Stop;
import repast.simphony.relogo.Utility;
import repast.simphony.relogo.UtilityG;
import repast.simphony.relogo.schedule.Go;
import repast.simphony.relogo.schedule.Setup;
import herosandcowards.ReLogoObserver;

class UserObserver extends ReLogoObserver{

	/**
	 * Add observer methods here. For example:
*/
		@Setup
		def setup(){
			clearAll()
			setDefaultShape(HeroOrCoward, "person")
			createHeroOrCowards( personCount ) {
				initialize()
			}
		}
	/**	
	 *
	 * or
	 * 	
	*/
		@Go
		def go(){
			ask(heroOrCowards()) {
				step()
			}
		}
		/**
	 */

}