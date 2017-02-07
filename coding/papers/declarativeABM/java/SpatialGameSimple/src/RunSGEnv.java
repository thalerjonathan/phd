
import cell.SGCell;
import gui.SGFrontend;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ExecutionException;

/**
 * Created by jonathan on 23/01/17.
 */
public class RunSGEnv {
    private Random rng;

    private final static long RNGSEED = 42;

    public RunSGEnv() {
        this.rng = new Random(RNGSEED);
    }

    public static void main(String[] args) throws ExecutionException, InterruptedException {
        RunSGEnv sgEnv = new RunSGEnv();
        sgEnv.run();
    }

    public void run() throws ExecutionException, InterruptedException {
        int rows = 99;
        int cols = 99;
        int steps = 221;

        SGFrontend fe = new SGFrontend( cols, rows );

        List<SGCell> cells = this.createCoopsWithOneDefectorAgents(cols, rows);

        for (int i = 0; i < steps; ++i)
            this.calculateNextStep(cells);

        fe.renderCells(cells);

        /*
        while ( true ) {
            this.calculateNextStep(cells);
            steps++;
            System.out.println(steps);
        }
        */
    }

    private void calculateNextStep(List<SGCell> cells) {
        for ( SGCell c : cells ) {
            c.playGame();
        }

        for ( SGCell c : cells ) {
            c.selectBest();
        }

        for ( SGCell c : cells ) {
            c.commit();
        }
    }

    private List<SGCell> createCoopsWithOneDefectorAgents(int cols, int rows) {
        List<SGCell> cells = new ArrayList<>();

        int halfCols = (int) (cols / 2.0);
        int halfRows = (int) (rows / 2.0);

        // NOTE: need to create them first and then set their enemies and friends because only then all available
        for (int y = 0; y < rows; ++y) {
            for (int x = 0; x < cols; ++x) {
                SGCell c;
                SGCell.SGState state = SGCell.SGState.Cooperator;

                if ( x == halfCols && y == halfRows)
                    state = SGCell.SGState.Defector;

                c = new SGCell(state, x, y);

                cells.add(c);
            }
        }

        for (int i = 0; i < cells.size(); ++i) {
            SGCell c = cells.get( i );
            List<SGCell> ns = getNeighbours(c, cells);

            c.setNeighbours( ns );
        }

        return cells;
    }

    private List<SGCell> getNeighbours(SGCell c, List<SGCell> all) {
        List<SGCell> neighbours = new ArrayList<>();
        List<Cell> nCells = calculateNeighbourhood(c);

        for (SGCell n : all) {
            if ( nCells.contains( new Cell( n.getX(), n.getY() ) ) ) {
                neighbours.add(n);

                if (neighbours.size() == nCells.size()) {
                    break;
                }
            }
        }

        return neighbours;
    }

    private class Cell {
        public Cell(int x, int y) {
            this.x = x;
            this.y = y;
        }

        private int x;
        private int y;

        public int getX() {
            return x;
        }

        public int getY() {
            return y;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Cell cell = (Cell) o;

            if (x != cell.x) return false;
            return y == cell.y;
        }

        @Override
        public int hashCode() {
            int result = x;
            result = 31 * result + y;
            return result;
        }
    }

    private List<Cell> calculateNeighbourhood(SGCell c) {
        List<Cell> n = new ArrayList<>();
        int x = c.getX();
        int y = c.getY();

        n.add( new Cell( x - 1, y - 1 ) );
        n.add( new Cell( x, y - 1) );
        n.add( new Cell( x + 1, y - 1) );

        n.add( new Cell( x - 1, y ) );
        n.add( new Cell( x, y ) );
        n.add( new Cell( x + 1, y ) );

        n.add( new Cell( x - 1, y + 1 ) );
        n.add( new Cell( x, y + 1) );
        n.add( new Cell( x + 1, y + 1) );

        return n;
    }
}
