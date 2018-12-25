package barter.test;

import org.junit.Test;
import static junit.framework.Assert.*;

import barter.BarterEconomy;
import barter.BarterParams;
import barter.OriginalBarterStrategy;

public class ModificationSafenessTest {
	@Test
	public void testModifications() {
		BarterEconomy be = new BarterEconomy();
		BarterParams params = new BarterParams();
		params._setSeed(11);
		be.setParams(params);
		be.addBarterStrategy(OriginalBarterStrategy.class, 1.0);

		be.start();
		for (int i = 0; i < 100; i++) {
			be.schedule.step(be);
		}

		double[] p = be.getAvgRelPrice();

//		System.out.println(Double.doubleToRawLongBits(p[0]));
//		System.out.println(Double.doubleToRawLongBits(p[1]));
//		System.out.println(Double.doubleToRawLongBits(p[2]));
//		System.out.println(Double.doubleToRawLongBits(p[3]));
//		System.out.println(Double.doubleToRawLongBits(p[4]));
//		System.out.println(Double.doubleToRawLongBits(p[5]));

		assertEquals(Double.doubleToRawLongBits(p[0]), -4620071376512247240L);
		assertEquals(Double.doubleToRawLongBits(p[1]), -4626126180871669611L);
		assertEquals(Double.doubleToRawLongBits(p[2]), 4598309074521829956L);
		assertEquals(Double.doubleToRawLongBits(p[3]), 4600994879832120059L);
		assertEquals(Double.doubleToRawLongBits(p[4]), 4607208134502429148L);
		assertEquals(Double.doubleToRawLongBits(p[5]), 0L);
	}

	@Test
	public void testVarySupplyModifications() {
		BarterEconomy be = new BarterEconomy();
		BarterParams params = new BarterParams();
		params._setSeed(11);
		params.setVarySupply(true);
		be.setParams(params);
		be.addBarterStrategy(OriginalBarterStrategy.class, 1.0);

		be.start();
		for (int i = 0; i < 100; i++) {
			be.schedule.step(be);
		}

		double[] p = be.getAvgRelPrice();

//		System.out.println(Double.doubleToRawLongBits(p[0]));
//		System.out.println(Double.doubleToRawLongBits(p[1]));
//		System.out.println(Double.doubleToRawLongBits(p[2]));
//		System.out.println(Double.doubleToRawLongBits(p[3]));
//		System.out.println(Double.doubleToRawLongBits(p[4]));
//		System.out.println(Double.doubleToRawLongBits(p[5]));

		assertEquals(Double.doubleToRawLongBits(p[0]), -4620028984057235634L);
		assertEquals(Double.doubleToRawLongBits(p[1]), -4627678756210890117L);
		assertEquals(Double.doubleToRawLongBits(p[2]), 4601783438703719164L);
		assertEquals(Double.doubleToRawLongBits(p[3]), 4606554755990409723L);
		assertEquals(Double.doubleToRawLongBits(p[4]), 4609883640914068800L);
		assertEquals(Double.doubleToRawLongBits(p[5]), 0L);
	}
}