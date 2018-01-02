package hac.backend.simulation.rng.impl;

import java.util.Random;

/**
 * Created by jonathan on 09/12/16.
 */
public class DeterministicRandom extends Random {

    public DeterministicRandom(int seed) {
        super(seed);
    }

    public DeterministicRandom() {
    }

    @Override
    protected int next(int bits) {

        return 0;
    }
}
