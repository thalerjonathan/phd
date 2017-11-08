package SugarScape;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.StringTokenizer;

/**
 * Class for reading PGM files into a 2D array.
 *
 * @author Eric Tatara
 * @author Nick Collier
 * @version 
 */

public class PGMReader {

	protected int matrix[][];
	protected int xSize;
	protected int ySize;

	public PGMReader(String sugarFile) {
		
		java.io.InputStream stream = null;
		try {
			stream = new FileInputStream(sugarFile);
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}	
		BufferedReader in = new BufferedReader(new InputStreamReader(stream));

		init(in);
	}

	private void init(BufferedReader in) {
		try {
			StringTokenizer tok;

			String str = in.readLine();

			if (!str.equals("P2")) {
				throw new UnsupportedEncodingException("File is not in PGM ascii format");
			}

			str = in.readLine();
			tok = new StringTokenizer(str);
			xSize = Integer.valueOf(tok.nextToken()).intValue();
			ySize = Integer.valueOf(tok.nextToken()).intValue();

			matrix = new int[xSize][ySize];

			in.readLine();

			str = "";
			String line = in.readLine();

			while (line != null) {
				str += line + " ";
				line = in.readLine();
			}
			in.close();

			tok = new StringTokenizer(str);

			for (int i = 0; i < xSize; i++) 
				for (int j = 0; j < ySize; j++) 
					matrix[i][j] = Integer.valueOf(tok.nextToken());
			
		} catch (IOException ex) {
			System.out.println("Error Reading image file");
			ex.printStackTrace();
			System.exit(0);
		}
	}

	public int[][] getMatrix() {
		return matrix;
	}
}
