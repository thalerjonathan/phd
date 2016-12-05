package hac.backend;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

/**
 * Created by jonathan on 05/12/16.
 */
public class Utils {

    public static void main(String[] args) {
        List<Integer> tests = new ArrayList<>();
        tests.add(0);
        tests.add(1);
        tests.add(2);

        // NOTE: this should throw a RuntimeException
        Integer randI = Utils.drawRandomIgnoring( tests, new Integer[] {2, 0, 1} );

        // NOTE: this should return 2
        //Integer randI = Utils.drawRandomIgnoring( tests, new Integer[] {0, 1} );
        //assert (randI == 2);
    }

    // NOTE: this method guarantees that if there is no solution then it will terminate, throwing a runtime exception
    public static <T extends Comparable> T drawRandomIgnoring(List<T> ts, T[] ignoring) {
        int randIdx = (int) (Math.random() * ts.size());
        T rand = ts.get( randIdx );

        for (int j = 0; j < ignoring.length; ++j) {
            if ( rand.equals( ignoring[j])) {
                return drawRandomIgnoring(ts, ignoring);
            }
        }

        return rand;

        /* NOTE: shuffle seems to f*** up the instances somehow, thus using non-shuffle version
        Collections.shuffle( ts, new Random(43) );

        randTLabel:
        for ( T t : ts ) {
            for (int j = 0; j < ignoring.length; ++j) {
                if ( t.equals( ignoring[j])) {
                    continue randTLabel;
                }
            }

            return t;
        }

        // NOTE: at this point there is no solution, throw RuntimeException
        throw new RuntimeException();
        */
    }
}
