package Mousetrap;

import repast.simphony.context.DefaultContext;
import repast.simphony.engine.schedule.ScheduledMethod;


/**
 * Used to test context creation from within the data loader.
 *
 * @author Nick Collier
 * @version $Revision$ $Date$
 */
public class MouseTrapContext extends DefaultContext {

	public MouseTrapContext() {
		super();
		System.out.println("MouseTrapContext created");
	}

	public MouseTrapContext(Object name) {
		super(name);
		System.out.println("MouseTrapContext created");
	}

	public MouseTrapContext(Object name, Object typeID) {
		super(name, typeID);
		System.out.println("MouseTrapContext created");
	}

	@ScheduledMethod (start = .5)
	public void step() {
		System.out.println("STEP");
	}
}
