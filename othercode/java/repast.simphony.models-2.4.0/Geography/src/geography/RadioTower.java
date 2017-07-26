package geography;

/**
 * RadioTower agents are static non-moving point feature agents.
 * 
 * @author Eric Tatara
 *
 */
public class RadioTower {

	private String name;
	
	public RadioTower(String name){
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
}
