package SugarScape;

import java.awt.Color;
import java.awt.Paint;
import java.util.HashMap;
import java.util.Map;

import repast.simphony.valueLayer.ValueLayer;
import repast.simphony.visualization.visualization3D.style.ValueLayerStyle3D;

/**
 * 
 * @author Eric Tatara
 *
 */
public class SurfaceStyle3D implements ValueLayerStyle3D {
	ValueLayer layer;
	Map<Integer,Color> colorMap; 
	
	public SurfaceStyle3D(){
		colorMap = new HashMap<Integer,Color>();

		colorMap.put(4, new Color(255, 255, 0));
		colorMap.put(3, new Color(255, 255, 255 / 3));
		colorMap.put(2, new Color(255, 255, 255 / 2));
		colorMap.put(1, new Color(255, 255, (int) (255 / 1.2)));
		colorMap.put(0, Color.white);
	}
	
	@Override
	public float getY(double... coordinates) {
		return 0;
	}
	
	@Override
	public float getCellSize() {
		return 0.06f;
	}

	@Override
	public Paint getPaint(double... coordinates) {
    int sugar = (int)layer.get(coordinates);
		
		return colorMap.get(sugar);
	}

	@Override
	public void addValueLayer(ValueLayer layer) {
		this.layer = layer;
	}

	@Override
	public int getRed(double... coordinates) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getGreen(double... coordinates) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getBlue(double... coordinates) {
		// TODO Auto-generated method stub
		return 0;
	}
}
