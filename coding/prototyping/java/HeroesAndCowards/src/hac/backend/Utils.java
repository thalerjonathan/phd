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
        Random r = new Random( 42 );
        List<Integer> tests = new ArrayList<>();
        tests.add(0);
        tests.add(1);
        tests.add(2);

        // NOTE: this should throw a RuntimeException
        Integer randI = Utils.drawRandomIgnoring( tests, new Integer[] {2, 0, 1}, r );

        // NOTE: this should return 2
        //Integer randI = Utils.drawRandomIgnoring( tests, new Integer[] {0, 1}, r );
        //assert (randI == 2);
    }

    // NOTE: this method guarantees that if there is no solution then it will terminate, throwing a runtime exception
    //       problem: needs too long for large ts.size
    /*
    public static <T extends Comparable> T drawRandomIgnoring(List<T> ts, T[] ignoring, Random r) {
        // NOTE: NEED to copy because otherwise would shuffle passed-in list, which we don't want (e.g. while iterating over it outside!)
        List<T> tsCopies = new ArrayList<T>( ts );
        Collections.shuffle( tsCopies, r );

        randTLabel:
        for ( T t : tsCopies ) {
            for (int j = 0; j < ignoring.length; ++j) {
                if ( t.equals( ignoring[j])) {
                    continue randTLabel;
                }
            }

            return t;
        }

        // NOTE: at this point there is no solution, throw RuntimeException
        throw new RuntimeException();
    }
    */

    // NOTE: this method will not terminate if there is no solution. Use this one if there are many more ts than ignorings and you know there is a solution
    public static <T extends Comparable> T drawRandomIgnoring(List<T> ts, T[] ignoring, Random r) {
        int randIdx = (int)(r.nextDouble() * ts.size());
        T randElem = ts.get( randIdx );

        for ( T i : ignoring ) {
            if (randElem.equals(i)) {
                return drawRandomIgnoring(ts, ignoring, r);
            }
        }

        return randElem;
    }


    public static boolean nearlyEqual(double a, double b, double epsilon) {
        final double absA = Math.abs(a);
        final double absB = Math.abs(b);
        final double diff = Math.abs(a - b);

        // NOTE: taken from http://floating-point-gui.de/errors/comparison/ and changed float to double (is it sill the same then?)
        /*


        if (a == b) { // shortcut, handles infinities
            return true;
        } else if (a == 0 || b == 0 || diff < Double.MIN_NORMAL) {
            // a or b is zero or both are extremely close to it
            // relative error is less meaningful here
            return diff < (epsilon * Double.MIN_NORMAL);
        } else { // use relative error
            return diff / Math.min((absA + absB), Double.MAX_VALUE) < epsilon;
        }
        */

        return diff < epsilon;
    }
}
