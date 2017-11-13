package socialForce;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import repast.simphony.context.Context;
import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.space.continuous.ContinuousSpace;
import repast.simphony.util.ContextUtils;
import socialForce.geom.Line;

public class Room {

	///////////////////////////////////////////////////////////////////////////
	// Basic Properties
	
	public Line alignment;
	public Object exit;
	public Object entrance;
	
	public int roomNo;
	public int screenNum;
	
	public double x;
	public double y;
	
	public int crowdLvl = 5;
	public int INIT_SCREEN_NUM = 2;
	
	public List<Screen> screens;
	
	private SocialForce main;
	
	public Room(SocialForce main, int crowdLvl, int initScreens) {
		this.main = main;
		this.crowdLvl = crowdLvl;
		
		this.screens = new ArrayList<Screen>();
		this.screenNum = initScreens;
	}
	
	///////////////////////////////////////////////////////////////////////////
	// Events
	
	private Screen addScreen(Context context) {
		Screen screen = new Screen(this.main);
		this.screens.add(screen);
		
		//Context<Object> context = ContextUtils.getContext(this.main);
		context.add(screen);
		ContinuousSpace<Object> space = (ContinuousSpace<Object>) context.getProjection(SocialForceBuilder.SPACE_ID);
		space.moveTo(screen, 0, 0); // add at 0/0, the renderer will take care of correctly rendering
		
		return screen;
	}
	
	// NOTE: occurs once at the start
	public void initScreens(Context context) {
		x = alignment.getX();
		y = alignment.getY();
		for(int i = 0; i < screenNum; i++){
			Screen screen = addScreen(context);
			boolean isXaxis = alignment.contains(x+1,y);
			screen.roomNo = roomNo;
			screen.alliY = y;
			screen.alliX = x;
			if(isXaxis){
				screen.y = 0;
				screen.isXaxis = true;
				screen.rotation = -Math.PI / 2;
				screen.min = 0;
				screen.max = alignment.length();
				screen.x = alignment.length()-50;
			}else{
				screen.x = 0;
				screen.isXaxis = false;
				screen.rotation = 0;
				screen.min = 0;
				screen.max = alignment.length();
				screen.y = alignment.length()-50*(i+1);
			}
			if(roomNo==1){
				screens.get(i).rotation = Math.PI;
			}
		}
	}
	
	// TODO: cyclic event, occurs at t=1 seconds and recurs every 30 seconds
	@ScheduledMethod(start = 1, interval = 30)
	public void monitorScreens() {
		for(int i = 0; i < screenNum; i++){
			if(screenNum > INIT_SCREEN_NUM){
				boolean del = true;
				for(Person p : main.getPeople()){
					if((p.isMoving() || (p.isReading())) && p.destScreen == screens.get(i)){
						del = false;
						break;
					}
				}
				if(del){
					Context<Object> context = ContextUtils.getContext(this);
					context.remove(screens.get(i));
					this.screens.remove(i);
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
			Screen screen = addScreen(ContextUtils.getContext(this));
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
