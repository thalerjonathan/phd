package barter.test;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import barter.BarterParams;
import barter.TradeAgent;

public class TradeAgentTest {

	TradeAgent a1, a2, a3, a4;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@Before
	public void setUp() throws Exception {
		double[] consumeVector = new double[] { 1, 2 };
		BarterParams params = new BarterParams();
		params.setConsume(consumeVector);

		a1 = new TradeAgent(0, new double[] { 0.5, 0.8 }, params);
		a2 = new TradeAgent(1, new double[] { 0.5, 0.8 }, params);
		a3 = new TradeAgent(1, new double[] { 0.49, 0.8 }, params);
		a4 = new TradeAgent(0, new double[] { 1.0, 0.4 }, params);
	}

//	@Test
//	public void testConstructor() {
//		assertEquals(0, a1.getProduceGood());
//
//		assertEquals(0, a1.getDemand(0), 0);
//		assertEquals(2, a1.getDemand(1), 0);
//
//		assertEquals(0.5, a1.getPrice(0), 0);
//		assertEquals(0.8, a1.getPrice(1), 0);
//
//		assertEquals(0, a1.getExchangeFor(0), 0);
//		assertEquals(2 * 0.8 / 0.5, a1.getExchangeFor(1), 0);
//
//		assertEquals(1.0 * 2, a1.getInventory(0), 0);
//		assertEquals(0, a1.getInventory(1), 0);
//	}
/*
	@Test
	public void testOffers() {
		// a1 initiates the trade (produces 0, wants 1)
		int wantGood = 1;
		int giveGood = 0;

		// a1 has produced 2.0 * 1.0 units of his produce good 0
		// a1 has demand for 2.0 units of good 1
		// a1 will exchange 0.5 / 0.8 = 0.625 units of good 1 for one unit of his good 0
		// a1 can give 2.0 units of his good for 2 * 0.625 = 1.25 units of good 1
		double[] amounts = new double[] {a1.getDemand(wantGood), a1.getExchangeFor(wantGood)};
		a1.adjustAmounts(wantGood, 0, giveGood, 1, amounts);

		assertEquals(1.25, amounts[0], 0);
		assertEquals(2.0, amounts[1], 0);

		// a1 offers 2.0 units of good 0 for 1.25 units of good 1
		// a2 would receive 0.5 * 2.0 = 1.0 of value and would give 0.8 * 1.25 = 1.0 value, accepted
		boolean accepted2 = a2.acceptOffer(giveGood, amounts[1], amounts[0]);
		assertTrue(accepted2);

		// a3 would receive 0.49 * 2.0 = 0.98 of value and would give 0.8 * 1.25 = 1.0 value, rejected
		boolean accepted3 = a3.acceptOffer(giveGood, amounts[1], amounts[0]);
		assertTrue(!accepted3);

		// a4 has produced 2.0 * 1.0 units of his produce good 0
		// a4 has demand for 2.0 units of good 1
		// a4 wants 1.0 / 0.4 = 2.5 units of good 1 for one unit of his good 0
		// a4 will give 0.8 units of his good for 2.0 units of good 1
		amounts = new double[] {a4.getDemand(wantGood), a4.getExchangeFor(wantGood)};
		a4.adjustAmounts(wantGood, 0, giveGood, 1, amounts);

		assertEquals(2.0, amounts[0], 0);
		assertEquals(0.8, amounts[1], 0);

		// let's offer too much of good 1 to agent1
		// it only wants 2.0 units, so it takes that and gives 1.0 * 2.0 / 4.0 = 0.5 units
		amounts = new double[] {4.0, 1.0};
		a1.adjustAmounts(1, 0, 0, 1, amounts);
		assertEquals(2.0, amounts[0], 0);
		assertEquals(0.5, amounts[1], 0);
	}

	 @Test
	 public void testAmounts() {
		TradeAgent a =
			new TradeAgent(0, new double[] {0.80044694284, 0.31859236745},
										new double[] {1.0, 4.0});
		TradeAgent b = new TradeAgent(1, new double[] {0.8609579131, 0.29537187964},
										new double[] {1.0, 4.0});

		assertEquals(1.5920723806, a.getExchangeFor(1), 0.00000001);
		assertEquals(2.9148269434, b.getExchangeFor(0), 0.00000001);

		Pair<Double, Double> amounts =
			a.getAmounts(1, a.getDemand(1), 0, a.getExchangeFor(1));

		assertEquals(1.5920723806, amounts.second, 0.00000001);
		assertEquals(4.0, amounts.first, 0.00000001);

		amounts = b.getAmounts(0, amounts.second, 1, amounts.first);

		assertEquals(2.512448585, amounts.second, 0.00000001);
		assertEquals(1.0, amounts.first, 0.00000001);
	}

	@Test
	public void testExchange() {
		// a1 initiates the trade (produces 0, wants 1)
		int wantGood = 1;
		int giveGood = 0;

		double[] amounts = new double[] {a1.getDemand(wantGood), a1.getExchangeFor(wantGood)};
		a1.adjustAmounts(wantGood, 0, giveGood, 1, amounts);

		assertEquals(1.25, amounts[0], 0);
		assertEquals(2.0, amounts[1], 0);

		System.out.println(a1.getExchangeFor(1));
		// assume someone is willing to make this transaction
		a1.exchangeGoods(wantGood, amounts[0], giveGood, amounts[1]);

		// gave all of his produce good 0
		assertEquals(0.0, a1.getInventory(0), 0);
		// received 1.25 units of good 1
		assertEquals(1.25, a1.getInventory(1), 0);

		// still wants 0.75 units of good 1
		assertEquals(0.75, a1.getDemand(1), 0);
		// still don't want his production good 0
		assertEquals(0.0, a1.getDemand(0), 0);

		// initially was willing to give 3.2 g0 for 2.0 g1
		// but now gave and 2.0 and is willing to give 3.2 - 2.0 = 1.2 units more of g0
		assertEquals(1.2, a1.getExchangeFor(1), 0.001);
		// gives nothing for his production good 0
		assertEquals(0.0, a1.getExchangeFor(0), 0);
	}
*/
}