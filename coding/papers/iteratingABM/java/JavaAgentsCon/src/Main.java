import HAC.RunHAC;

import java.util.concurrent.ExecutionException;

/**
 * Created by jonathan on 20/01/17.
 */
public class Main {

    public static void main(String[] args) throws InterruptedException, CloneNotSupportedException, ExecutionException {
        RunHAC hac = new RunHAC();
        hac.run();
    }
}
