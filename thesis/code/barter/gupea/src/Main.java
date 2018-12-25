import barter.BarterEconomy;
import barter.BarterEconomyOptions;
import barter.BarterParams;
import barter.CopyAndMutateImprovementStrategy;
import barter.DelphiReplacementStrategy;
import barter.GenerousBarterStrategy;
import barter.OriginalBarterStrategy;
import barter.display.BarterEconomyGUI;

public class Main {

	public static void main(String[] args) {
		BarterParams params = new BarterParams();
		BarterEconomyOptions.handleOptions(args, params);

		BarterEconomy eco = new BarterEconomy();
		eco.setParams(params);

		eco.addBarterStrategy(OriginalBarterStrategy.class, 1.0);
		eco.addBarterStrategy(GenerousBarterStrategy.class, 0.0);

		eco.setImprovementStrategy(CopyAndMutateImprovementStrategy.class);
		eco.setReplacementStrategy(DelphiReplacementStrategy.class);

		//eco.start();
		BarterEconomyGUI gui = new BarterEconomyGUI(eco);
		gui.create();
	}
}