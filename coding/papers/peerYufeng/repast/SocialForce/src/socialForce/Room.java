package socialForce;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class Room {

	///////////////////////////////////////////////////////////////////////////
	// Basic Properties
	
	private Object alignment;
	private Object exit;
	private Object entrance;
	
	private int roomNo;
	private int screenNum;
	
	private double x;
	private double y;
	
	private final static int CROWD_LVL = 5;
	private final static int INIT_SCREEN_NUM = 2;
	
	private List<Screen> screens;
	
	///////////////////////////////////////////////////////////////////////////
	// Events
	
	// TODO: occurs once at the start
	private void initScreens() {
		x = alignment.getX();
		y = alignment.getY();
		for(int i = 0; i < screenNum; i++){
			add_screens();
			boolean isXaxis = alignment.contains(x+1,y);
			screens.get(i).roomNo = roomNo;
			screens.get(i).alliY = y;
			screens.get(i).alliX = x;
			if(isXaxis){
				screens.get(i).y = 0;
				screens.get(i).isXaxis = true;
				screens.get(i).rotation = -Math.PI / 2;
				screens.get(i).min = 0;
				screens.get(i).max = alignment.length();
				screens.get(i).x = alignment.length()-50;
			}else{
				screens.get(i).x = 0;
				screens.get(i).isXaxis = false;
				screens.get(i).rotation = 0;
				screens.get(i).min = 0;
				screens.get(i).max = alignment.length();
				screens.get(i).y = alignment.length()-50*(i+1);
			}
			if(roomNo==1){
				screens.get(i).rotation = Math.PI;
			}
		}
	}
	
	// TODO: cyclic event, occurs at t=1 seconds and recurrs every 30 seconds
	@ScheduledMethod(start = 1, interval = 30)
	private void monitorScreens() {
		for(int i = 0; i < screenNum; i++){
			if(screenNum > initScreenNum){
				boolean del = true;
				for(Person p :get_Main().people){
					if((p.inState(p.moving) || (p.inState(p.reading))) && p.destScreen == screens.get(i)){
						del = false;
						break;
					}
				}
				if(del){
					remove_screens(screens.get(i));
					screenNum--;
				}
			}
		}
		int screenNum_t = screenNum;
		for(int i = 0; i < screenNum_t; i++){
			if(screens.get(i).calcReadingNum() > crowdLvl && screens.size() < 7){
				createScreen();
			}
			
		}
	}
	
	private void createScreen() {
		Map screenPos = new TreeMap();
		for(Screen s: screens){
			double k = s.y-s.ri;
			double v = s.y+s.ri;
			screenPos.put(k,v);
		}
		TreeMap emptySpace = new TreeMap();
		double before = 0;
		for(Object i : screenPos.keySet()){
			double k = (double)i;
			if(k>before+40){
				double diff = k-before;
				emptySpace.put(diff,before);
			}
			before = (double)screenPos.get(i);
		}
		if(before<alignment.length()-40){
			emptySpace.put(alignment.length()-before,before);
		}
		if(!emptySpace.isEmpty()){
			Object largestDiff = emptySpace.lastKey();
			Object ty = emptySpace.get(largestDiff);
			double finalY = (double)ty + (double)largestDiff/2;
			add_screens();
			screenNum++;
			screens.get(screenNum-1).roomNo = roomNo;
			screens.get(screenNum-1).alliX = x;
			screens.get(screenNum-1).alliY = y;
			screens.get(screenNum-1).isXaxis = false;
			screens.get(screenNum-1).rotation = 0;
			screens.get(screenNum-1).min = 0;
			screens.get(screenNum-1).max = alignment.length();
			screens.get(screenNum-1).x = 0;
			screens.get(screenNum-1).y = finalY;
			if(roomNo==1){
				screens.get(screenNum-1).rotation = Math.PI;
			}
		}
	}
}
