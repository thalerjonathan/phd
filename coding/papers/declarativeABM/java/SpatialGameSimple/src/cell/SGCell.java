package cell;

import java.util.List;

/**
 * Created by jonathan on 23/01/17.
 */
public class SGCell {

    private final static double B = 1.9;
    private final static double S = 0.0;
    private final static double P = 0.0;
    private final static double R = 1.0;

    public enum SGState {
        Defector,
        Cooperator
    }

    private int x;
    private int y;

    private SGState currState;
    private SGState prevState;

    private double localPayoff;
    private SGState localBest;

    private List<SGCell> neighbours;

    public SGCell(SGState state, int x, int y) {
        this.currState = state;
        this.prevState  = state;

        this.x = x;
        this.y = y;

        this.localPayoff = 0.0;
    }

    public void setNeighbours(List<SGCell> neighbours) {
        this.neighbours = neighbours;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public SGState getCurrState() {
        return currState;
    }

    public SGState getPrevState() {
        return prevState;
    }

    public double getLocalPayoff() {
        return this.localPayoff;
    }

    public void playGame() {
        this.localPayoff = 0.0;

        for ( SGCell n : neighbours ) {
            localPayoff += calculatePayoff( this.currState, n.getCurrState() );
        }
    }

    public void selectBest() {
        SGState bestPayoffState = this.currState;
        double bestPayoffValue = this.localPayoff;

        for ( SGCell n : neighbours ) {
            double nPo = n.getLocalPayoff();
            SGState nSt = n.getCurrState();

            if ( nPo > bestPayoffValue) {
                bestPayoffState = nSt;
                bestPayoffValue = nPo;
            }
        }

        this.localBest = bestPayoffState;
    }

    public void commit() {
        this.prevState = this.currState;
        this.currState = this.localBest;
    }

    private static double calculatePayoff(SGState ref, SGState other) {
        if ( SGState.Defector == ref && SGState.Defector == other ) {
            return P;
        } else if ( SGState.Cooperator == ref && SGState.Defector == other ) {
            return S;
        } else if ( SGState.Defector == ref && SGState.Cooperator == other ) {
            return B;
        }

        // NOTE: SGState.Cooperator == ref && SGState.Cooperator == other
        return R;
    }
}
