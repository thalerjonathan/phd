import HAC.RunHAC;
import SIRS.RunSIRS;

/**
 * Created by jonathan on 20/01/17.
 */
public class Main {

    public static void main(String[] args) {
        /*
        RunHAC hac = new RunHAC();
        hac.run();
        */

        RunSIRS sirs = new RunSIRS();
        sirs.run();
    }
}
