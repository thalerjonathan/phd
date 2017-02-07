import SGEnv.RunSGEnv;
import SGMsg.RunSG;

import java.util.concurrent.ExecutionException;

/**
 * Created by jonathan on 20/01/17.
 */
public class Main {

    public static void main(String[] args) throws ExecutionException, InterruptedException {
/*
        RunSG sg = new RunSG();
        sg.run();
*/

        RunSGEnv sgEnv = new RunSGEnv();
        sgEnv.run();

    }
}
