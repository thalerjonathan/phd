package herosandcowards.relogo

import static repast.simphony.relogo.Utility.*
import static repast.simphony.relogo.UtilityG.*

import bsh.This
import herosandcowards.ReLogoTurtle
import repast.simphony.relogo.Plural
import repast.simphony.relogo.Stop
import repast.simphony.relogo.Utility
import repast.simphony.relogo.UtilityG
import repast.simphony.relogo.schedule.Go
import repast.simphony.relogo.schedule.Setup

class HeroOrCoward extends ReLogoTurtle {

	private boolean hero;
	private HeroOrCoward friend;
	private HeroOrCoward enemy;
	
	def initialize() {
		setxy( randomXcor(), randomYcor() );
		
		HeroOrCoward[] ignore = new HeroOrCoward[ 2 ];
		ignore[ 0 ] = this;
		
		friend = getRandomIgnoring( ignore );
		ignore[ 1 ] = friend;
		
		enemy = getRandomIgnoring( ignore );
		
		hero = Math.random() <= heroesDistribution
	}
	
	def HeroOrCoward getRandomIgnoring( HeroOrCoward[] ignore ) {
		HeroOrCoward r = null;
		
		outerLoop:
		while( true ) {
			int randIdx = Math.random() * heroOrCowards().size();
			r = heroOrCowards().get( randIdx );
			
			for ( HeroOrCoward c : ignore ) {
				if ( null == c )
					continue
					
				if ( r == c ) {
					continue outerLoop;
				}
			}
			
			break; 
		}
		
		return r;
	}
	
	def step() {
		double fX = friend.getXcor();
		double fY = friend.getYcor();
		
		double eX = enemy.getXcor();
		double eY = enemy.getYcor();
		
		double friendToEnemyX = eX - fX;
		double friendToEnemyY = eY - fY;
		double friendToEnemyLength = Math.sqrt( friendToEnemyX * friendToEnemyX + friendToEnemyY * friendToEnemyY );
		
		double directionX = friendToEnemyX / friendToEnemyLength;
		double directionY = friendToEnemyY / friendToEnemyLength;
		
		double myMoveX = 0.0;
		double myMoveY = 0.0;
		
		double distance = 5;
		
		if ( hero ) {
			double protectionPointX = fX + directionX * distance;
			double protectionPointY = fY + directionY * distance;
			
			//protectionPointX = fX + friendToEnemyX * 0.5;
			//protectionPointY = fY + friendToEnemyY * 0.5;
			
			myMoveX = protectionPointX;
			myMoveY = protectionPointY;
			
		} else {
			double hidingPointX = fX - directionX * distance;
			double hidingPointY = fY - directionY * distance;
		
			//hidingPointX = fX - friendToEnemyX * 0.5;
			//hidingPointY = fY - friendToEnemyY * 0.5;
		
			myMoveX = hidingPointX;
			myMoveY = hidingPointY;
		}
		
		this.facexy(myMoveX, myMoveY);
		this.move(0.1);
	}
}
