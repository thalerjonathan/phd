import HAC.RunHAC;
import SGEnv.RunSGEnv;
import SGMsg.RunSG;
import SIRS.RunSIRS;

import java.util.concurrent.ExecutionException;

/**
 * Created by jonathan on 20/01/17.
 */
public class Main {

    public static void main(String[] args) throws InterruptedException, CloneNotSupportedException, ExecutionException {
        /*
        RunHAC hac = new RunHAC();
        hac.run();
        */

        /*
        RunSIRS sirs = new RunSIRS();
        sirs.run();
        */

        /*
        RunSG sg = new RunSG();
        sg.run();
*/

        RunSGEnv sgEnv = new RunSGEnv();
        sgEnv.run();
    }
}
