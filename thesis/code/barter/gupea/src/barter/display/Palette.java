package barter.display;

import java.awt.Color;
import java.util.Map;
import java.util.WeakHashMap;

/**
 * Manages the color palette of the agent visualisation.
 * 
 * @NotThreadSafe This class is NOT thread safe!
 */
public class Palette {
	private final int size;
	private Color[] colors;

	private static final Color[] niceColors = new Color[] {
		Color.decode("#e83c3c"),
		Color.decode("#5080d5"),
		Color.decode("#ae1e4f"),
		Color.decode("#eec837"),
		Color.decode("#9aab05"),
		Color.decode("#f5b225"),
		Color.decode("#6b439c"),
		Color.decode("#c7a893"),
		Color.decode("#687621"),
		Color.decode("#98c3f0"),
		Color.decode("#383838"),
		Color.decode("#9fa291")};
	private static Map<Integer,Color> nicerColors = new WeakHashMap<Integer,Color>();
	
	private Palette(int size) {
		this.size = size;
		colors = new Color[size];
	}

	public static Palette newContinuousPalette(int steps, Color c1, Color c2) {
		Palette p = new Palette(steps);

		for (int i = 0; i < steps; i++) {
			p.colors[i] = blendColors(c1, c2, i / (double) steps);
		}

		return p;
	}

	public static Palette newContinuousPalette(int steps, Color c1, Color c2, Color c3) {
		Palette p = new Palette(steps);

		for (int i = 0; i < steps / 2; i++) {
			p.colors[i] = blendColors(c1, c2, i / (double) steps * 2.0);
		}
		for (int i = steps / 2; i < steps; i++) {
			p.colors[i] = blendColors(c2, c3, i / (double) steps * 2.0);
		}

		return p;
	}

	private static double lerp(double a, double b, double m) {
		return a * (1.0 - m) + b * m;
	}

	public static Color blendColors(Color c1, Color c2, double m) {
		double r = lerp(c1.getRed(), c2.getRed(), m) / 256.0;
		double g = lerp(c1.getGreen(), c2.getGreen(), m) / 256.0;
		double b = lerp(c1.getBlue(), c2.getBlue(), m) / 256.0;
		
		return new Color((float) r, (float) g, (float) b);
	}

	public Color getColor(int idx) {
		return colors[idx];
	}
	
	public Color getColor(double idx) {
		return colors[(int) (idx * size)];
	}

	public static Color createIndexColor(int idx) {
		Color base = niceColors[idx % niceColors.length];
		float[] hsb = Color.RGBtoHSB(base.getRed(), base.getGreen(), base.getBlue(), null);
		double newHue = Math.IEEEremainder((double) idx / niceColors.length + hsb[0], 1.0); 
		double newSat = Math.IEEEremainder((double) idx  * 1.1111 / niceColors.length +
				hsb[1], 1.0) / 2.0 + 0.5; 
		return Color.getHSBColor((float) newHue, (float) newSat, hsb[2]);
	}
	
	public static Color getIndexColor(int idx) {
		if(idx < niceColors.length) {
			return niceColors[idx];
		} else {
			Color c = nicerColors.get(idx);
			if(c == null) {
				c = createIndexColor(idx);
				nicerColors.put(idx, c);
			}
			return c;
		}
	}

	public int getSize() {
		return size;
	}
}
