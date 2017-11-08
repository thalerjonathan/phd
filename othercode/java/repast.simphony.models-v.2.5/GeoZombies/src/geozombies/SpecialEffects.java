package geozombies;

import java.awt.Color;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.FontUIResource;

import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.environment.RunListener;
import repast.simphony.parameter.Parameters;
import repast.simphony.ui.RSApplication;

/**
 * Create sound and visual effects for the GeoZombies demo.  No code in this
 * class will affect the model behavior and is purely for demonstration of 
 * standard Java media.
 * 
 * @author Eric Tatara
 *
 */
public class SpecialEffects implements RunListener {

	public static String SCREAM_FILE = "data/scream.wav";
	public static String ZOMBIE_MOAN_FILE = "data/zombiemoan.wav";
	public static String ENRAGED_ZOMBIES_FILE = "data/enragedzombies.wav";
	public static String ZOMBIE_GIBBERISH_FILE = "data/zombiegibberish.wav";
	
	protected Clip zombieMoanClip;
	protected Clip screamClip;
	protected Clip enragedZombiesClip;
	protected Clip zombieGibberishClip;
	
	protected List<Clip> clipList;
	
	protected Frame supriseFrame;
	
	public static SpecialEffects instance;

	public static void main(String[] args){
    String fonts[] = 
      GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();

    for ( int i = 0; i < fonts.length; i++ ){
      System.out.println(fonts[i]);
    }
  }
	
	public static void init(){
		instance = new SpecialEffects();
	}
	
	public SpecialEffects(){
		RunEnvironment.getInstance().addRunListener(this);
		clipList = new ArrayList<Clip>();
		initSurprise();
		
		// Load sound clips from files.
		try{
			File file = new File(SCREAM_FILE);
			screamClip = AudioSystem.getClip();
			AudioInputStream ais = AudioSystem.getAudioInputStream(file);
			screamClip.open(ais);
			clipList.add(screamClip);
		}
		catch (Exception e){
			e.printStackTrace();
		}
		
		try{
			File file = new File(ZOMBIE_MOAN_FILE);
			zombieMoanClip = AudioSystem.getClip();
			AudioInputStream ais = AudioSystem.getAudioInputStream(file);
			zombieMoanClip.open(ais);
			clipList.add(zombieMoanClip);
		}
		catch (Exception e){
			e.printStackTrace();
		}
		
		try{
			File file = new File(ENRAGED_ZOMBIES_FILE);
			enragedZombiesClip = AudioSystem.getClip();
			AudioInputStream ais = AudioSystem.getAudioInputStream(file);
			enragedZombiesClip.open(ais);
			clipList.add(enragedZombiesClip);
		}
		catch (Exception e){
			e.printStackTrace();
		}	
		
		try{
			File file = new File(ZOMBIE_GIBBERISH_FILE);
			zombieGibberishClip = AudioSystem.getClip();
			AudioInputStream ais = AudioSystem.getAudioInputStream(file);
			zombieGibberishClip.open(ais);
//			clipList.add(zombieGibberishClip);
		}
		catch (Exception e){
			e.printStackTrace();
		}	
	}

	
	public  void playScream(){
		Parameters params = RunEnvironment.getInstance().getParameters();
		Boolean play = (Boolean)params.getBoolean("playSounds");
		
		// don't play multiple at same time or when play disabled
		if (!play || screamClip.isActive()) return;  
		
		screamClip.setFramePosition(0); // rewind
		screamClip.start();
	}
	
	public  void playZombieMoan(){
		Parameters params = RunEnvironment.getInstance().getParameters();
		Boolean play = (Boolean)params.getBoolean("playSounds");
		
		// don't play multiple at same time or when play disabled
		if (!play || zombieMoanClip.isActive()) return;  
		
		zombieMoanClip.setFramePosition(0); // rewind
		zombieMoanClip.start();
	}
	
	public  void playEnragedZombies(){
		Parameters params = RunEnvironment.getInstance().getParameters();
		Boolean play = (Boolean)params.getBoolean("playSounds");
		
		// don't play multiple at same time or when play disabled
		if (!play || enragedZombiesClip.isActive()) return; 
		
		enragedZombiesClip.setFramePosition(0); // rewind
		enragedZombiesClip.start();
	}
	
	public  void playZombieGibberish(){
		Parameters params = RunEnvironment.getInstance().getParameters();
		Boolean play = (Boolean)params.getBoolean("playSounds");
		
		// don't play multiple at same time or when play disabled
		if (!play || zombieGibberishClip.isActive()) return; 
		
		zombieGibberishClip.setFramePosition(0); // rewind
		zombieGibberishClip.start();
	}
	
	
	/**
	 * RunListener methods below stops any playing sounds when sim is paused or stopped.
	 */
	
	@Override
	public void stopped() {
		for (Clip clip : clipList){
			clip.stop();
		}
	}

	@Override
	public void paused() {
		for (Clip clip : clipList){
			clip.stop();
		}
	}

	@Override
	public void started() {
	}

	@Override
	public void restarted() {
		for (Clip clip : clipList){
			clip.stop();
		}
	}

	public static SpecialEffects getInstance() {
		if (instance == null){
			init();
		}
		
		return instance;
	}
	
	static int FONT_SIZE = 12;
  static String FONT_FAMILY1 = "Showcard Gothic";
  
  /**
   * Change the UI look and feel
   */
  public static void setUIEffects() {
    
    for (Map.Entry<Object, Object> entry : javax.swing.UIManager.getDefaults().entrySet()) {
      Object key = entry.getKey();
      Object value = javax.swing.UIManager.get(key);
            
      if (value != null && value instanceof javax.swing.plaf.FontUIResource) {

        FontUIResource font = new FontUIResource(FONT_FAMILY1, 2, FONT_SIZE);
        UIManager.put(key, font);
      }
      
      if ("Menu.foreground".equals(key) || 
          "PopupMenu.foreground".equals(key) ||
          "TextField.foreground".equals(key) ||
          "Table.foreground".equals(key) ||
          "Label.foreground".equals(key) ||
          "Tree.foreground".equals(key) ||
          "Tree.textForeground".equals(key) ||
          "MenuItem.foreground".equals(key) ||
          "TextPane.foreground".equals(key) ||
          "TabbedPane.foreground".equals(key) ||
          "TextArea.foreground".equals(key) ||
          "List.foreground".equals(key) ||
          "Button.foreground".equals(key)){
        
        ColorUIResource color = new ColorUIResource(Color.RED);
        UIManager.put(key, color);
        
      }
    }

    JFrame frame = RSApplication.getRSApplicationInstance().getGui().getFrame();
    SwingUtilities.updateComponentTreeUI(frame);
  }
  
  public void surprise(){
    supriseFrame.setVisible(true);
    playZombieGibberish();
  }
  
  protected void initSurprise(){
    supriseFrame = new JFrame();
    
    ImageIcon icon = new ImageIcon("data/zombie_1920.jpg");
    
    JLabel label = new JLabel(icon);
    label.addMouseListener(new MouseListener(){

      @Override
      public void mouseClicked(MouseEvent arg0) {
        supriseFrame.dispose();
      }

      @Override
      public void mouseEntered(MouseEvent arg0) {
      }

      @Override
      public void mouseExited(MouseEvent arg0) {
      }

      @Override
      public void mousePressed(MouseEvent arg0) {
      }

      @Override
      public void mouseReleased(MouseEvent arg0) {
      }
    });
    
    supriseFrame.add(label);
    
    supriseFrame.setExtendedState(JFrame.MAXIMIZED_BOTH); 
//    frame.setUndecorated(true);
//    frame.setVisible(true);
    
//    SpecialEffects.getInstance().playZombieGibberish();
  }
}