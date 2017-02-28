package gui;

import cell.SGCell;

import javax.swing.*;
import java.awt.*;
import java.util.List;

/**
 * Created by jonathan on 05/12/16.
 */
public class SGFrontend extends JFrame {
    private SGRenderer renderer;

    public SGFrontend(int cols, int rows) {
        super("Spatial Game Par");

        this.renderer = new SGRenderer(cols, rows);

        this.setDefaultCloseOperation( EXIT_ON_CLOSE );
        this.setContentPane( renderer );
        this.setSize( new Dimension( 1000, 1000 ));
        this.setVisible(true);
    }

    public void renderCells(List<SGCell> cells) {
        this.renderer.render(cells);

        try {
            Thread.sleep( 1000 );
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
