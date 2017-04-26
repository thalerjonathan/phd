package Sugarscape;

import java.util.*;
import java.awt.Graphics;
import java.awt.Color;
import java.text.*;
import java.lang.System;

/**
 * The Citizen object is modeled on a human being. It possesses attributes like 
 * location, vision, metabolism rates, resource stockpiles, parents, sex etc.<BR>
 * Each Citizen lives a randomly determined lifespan during which they gather 
 * & eat food, seek & select mates, bear children, trade to increase their wealth,
 * inherit wealth upon the death of parents etc.<BR>
 * As the simulation is developed further, these citizens become more capable and 
 * are able to partake in more complex tasks. They function absolutely at a local level 
 * and are unaware of external influences beyond their immediate surroundings.<BR>
 * Collectively however I hope their actions add to our understanding of how communal 
 * behavior, that cannot be explained solely from individual actions, emerges.
 * 
 * @author	abraham kannankeril
 */
class Citizen
{	int id;				//unique identiifer for each citizen
	int homeCol, homeRow;
	int vision;			//Vision Limit in cells in all directions
	int metabSugar;		//Amount of Sugar required to survive each round
	float sugar;		//Amount of Sugar currently left with citizen
	float inheritSugar;	//Initial sugar inherited/assigned at birth
	int metabSpice;		//Amount of Sugar required to survive each round
	float spice;		//Amount of Sugar currently left with citizen
	float inheritSpice;	//Initial sugar inherited/assigned at birth
	int lifeSpan;		//Age at which citizen is expected to die
	int sex;			//Gender of citizen
	int persona;		//Character - conservative or aggressive 
	int father;			//Father of citizen
	int mother;			//Mother of citizen
	java.util.List childList, diseaseList;	//List of children
	String family;		//Family to which citizen belongs
	int lastMating;		//Generation in which citizen last mated
	int birthYear;		//Generation in which citizen was born
	String causeOfDeath;//stores the causeOfDeath
	int ranking;		//score determining citizen suitability for mating
	int spUnits, suUnits, TUD, surplus;
	float MRS;
	/**	list of inherited cultural attributes	*/
	boolean genoMeme[] = new boolean[GoLconst.CITIZEN_CULTURE_TAG];
	/** tag list with mutations occurring over citizen lifespan */
	boolean phenoMeme[] = new boolean[GoLconst.CITIZEN_CULTURE_TAG];
	/** immune system or library of recognized disease patterns */
	//boolean immuneSys[] = new boolean[GoLconst.CITIZEN_IMMUNE_STRING];
	boolean genoImmuneSys[] = new boolean[GoLconst.CITIZEN_IMMUNE_STRING];
	boolean phenoImmuneSys[] = new boolean[GoLconst.CITIZEN_IMMUNE_STRING];
	
	//**Static Constants For This Class*/
	static int citizenID = 0;
	static List surnames = new LinkedList();
	static int HIGH_ENDOWMENT_SUGAR = (int)( GoLconst.SUGAR_MIN_CITIZEN +
		(GoLconst.SUGAR_MAX_CITIZEN - GoLconst.SUGAR_MIN_CITIZEN ) / 2);
	static int HIGH_ENDOWMENT_SPICE = (int)( GoLconst.SPICE_MAX_CITIZEN +
		(GoLconst.SPICE_MAX_CITIZEN - GoLconst.SPICE_MIN_CITIZEN ) / 2);

	static short groupCount = GoLconst.groupCount();	//CITIZEN_GROUP_INTERVAL.length + 1;
	public static int rangeArr[] = new int[groupCount];
	static short rangeColor[] = new short[groupCount*3];
	
	static public java.util.List 
				tmpList = new LinkedList(),
				germList = new LinkedList();	//set of diseases prevalent on the Ss
	
	static short 
		cultTagLen = ( (GoLconst.CITIZEN_CULTURE_TAG < 31)? 
							GoLconst.CITIZEN_CULTURE_TAG : 31),
		immuneTagLen = ( (GoLconst.CITIZEN_IMMUNE_STRING < 63)? 
							GoLconst.CITIZEN_IMMUNE_STRING : 63); 
	static long 
		cultTagMax = (long)Math.pow(2, cultTagLen), 
		immuneTagMax = (long)Math.pow(2, immuneTagLen), 
		immuneTagMin = (long)Math.pow(2, immuneTagLen-1), 
		immuneTagDiff = immuneTagMax - immuneTagMin;
	

	/**
	 * Base constructor for the Citizen object. Used to initialize temporary variables of 
	 * type Citizen.
	 * 
	 */
	public Citizen()
	{	id				= -1;
		homeCol			= -1;
		homeRow			= -1;
		vision 			= 0;
		metabSugar 		= 0;
		metabSpice 		= 0;
		sugar			= 0f;
		spice			= 0f;
		inheritSugar 	= 0;		//ensure no double-inheritance
		inheritSpice 	= 0;
		lifeSpan		= 0;		//set random age within range
		sex				= 0;		//1 for male, 0 for female
		persona			= 0;		//default - conservative
		father 			= 0;
		mother 			= 0;
		childList		= new LinkedList();
		diseaseList		= new LinkedList();
		family			= null;
		lastMating		= 0;		//generation when last mating occurred
		birthYear		= 0;
		causeOfDeath	= "";
		ranking			= 0;

		spUnits 		= 0;
		suUnits 		= 0;
		MRS 			= 0;
		TUD 			= 0;
		surplus			= 0;		
	}
	/**
	 * Constructor used to create new citizen object inside a cell object. 
	 * Citizen object initialized to random attributes.
	 * 
	 * @param	col	column identifying cell where citizen is to be created.
	 * @param	row	row identifying cell where citizen is to be created.
	 */
	public Citizen(int col, int row)
	{	id				= setID();
		homeCol			= col;
		homeRow			= row;
		vision 			= setVision(-1);
		metabSugar 		= setMetabSugar(-1);
		metabSpice		= setMetabSpice(-1);
		inheritSugar 	= setSugar(-1);		//ensure no double-inheritance
		inheritSpice	= setSpice(-1);
		sugar			= inheritSugar;
		spice			= inheritSpice;
		this.setLifeSpan(-1);				//set random age within range
		sex				= setSex();			//1 for male, 0 for female
		persona			= setPersona();		//assign random personality
		father 			= 0;
		mother 			= 0;
		childList		= new LinkedList();	//list of children
		this.setFamily();					//set random family name
		setImmuneSys(null, null);						//generate random immune string
		diseaseList		= new LinkedList();
		setDiseases(); 
		lastMating		= 0;
		birthYear		= GoLconst.timePeriod;
		causeOfDeath	= "";
		this.setRanking();
		this.setCultureTags();
		spUnits 		= 0;
		suUnits 		= 0;
		MRS 			= 0;
		TUD 			= 0;
		surplus			= 0;				
	}
	/**
	 * Constructor used to create citizen object inheriting attributes from a pair of 
	 * parent Citizen objects.
	 * 
	 * @param	citizen	parent of newborn citizen.
	 * @param	mate	parent of newborn citizen.
	 * @param	col	column identifying cell where citizen is to be created.
	 * @param	row	row identifying cell where citizen is to be created.
	 */
	public Citizen(Citizen citizen, Citizen mate, int col, int row)
	{	id				= setID();
		homeCol			= col;
		homeRow			= row;
		this.setInheritAttributes(citizen,mate);
		//inherit 1/2 parents inheritance or 1/2 of each parents wealth
		this.transferWealth(citizen,mate);	//sugar & spice
		this.setLifeSpan(-1);				//set random age within range
		sex				= setSex();			//1 for male, 0 for female
		persona			= setPersona();		//assign random personality
		this.setParents(citizen, mate);
		childList		= new LinkedList();
		this.setFamily(citizen,mate);
		setImmuneSys(citizen, mate);						//generate random immune string
		diseaseList		= new LinkedList();
		setDiseases(); 
		lastMating		= 0;
		birthYear		= GoLconst.timePeriod;
		causeOfDeath	= "";
		this.setRanking();
		spUnits 		= setSpUnits();
		suUnits 		= setSuUnits();
		MRS 			= setMRS();
		TUD 			= setTUD();
		surplus			= setSurplus();				
	}

	/**
	 * @return	Returns a unique identification number for the citizen.
	 */
	public int getID()			{	return id;			}

	/**
	 * @return	Returns the value representing the vision of the citizen.
	 */
	public int getVision()		{	return vision;		}

	/**
	 * @return	Returns the value representing the citizen's metabolic rate for sugar.
	 */
	public int getMetabSugar()	{	return metabSugar;	}
	/**
	 * @return	Returns the value representing the citizen's metabolic rate for spice.
	 */
	public int getMetabSpice()	{	return metabSpice;	}

	/**
	 * @return	Returns the value representing the citizen's sugar stockpile.
	 */
	public float getSugar()		{	return sugar;		}
	/**
	 * @return	Returns the value representing the citizen's spice stockpile.
	 */
	public float getSpice()		{	return spice;		}

	/**
	 * @return	Returns the column on the grid where the citizen is located.
	 */
	public int getCol()			{	return homeCol;		}

	/**
	 * @return	Returns the row on the grid where the citizen is located.
	 */
	public int getRow()			{	return homeRow;		}

	/**
	 *  @return	Returns the sugar inherited by the citizen from its parents.
	 */
	public float getInheritSugar(){	return inheritSugar;	}
	/**
	 *  @return	Returns the spice inherited by the citizen from its parents.
	 */
	public float getInheritSpice(){	return inheritSpice;	}

	/**
	 * @return	Returns the expected lifespan of the citizen.
	 */
	public int getLifeSpan()	{	return lifeSpan;	}

	/**
	 * @return	Returns true if the citizen is of the 'male' sex.
	 */
	public boolean isMale()		{	return (sex == 1);	}
	
	/**
	 * @return	Returns a numeric code uniquely identifying the citizen's father.
	 */
	public int getFather()		{	return father;		}

	/**
	 * @return	Returns a numeric code uniquely identifying the citizen's mother.
	 */
	public int getMother()		{	return mother;		}
	
	/**
	 * @return	personality type; either 'bull' or 'bear'
	 */
	public int getPersona()		{	return persona;		}

	/**
	 *  @return	Returns a string identifying the citizen's family. The family name is usually 
	 * inherited from the father and currently has no relevance.
	 * 
	 */
	public String getFamily()	{	return family;		}

	/**
	 * The cycle or time period when the citizen last mated successfully. Minimum 
	 * gaps between successive mating periods can be configured in the global constants 
	 * (GoLconst.java) file.
	 * 
	 */
	public int getLastMating()	{	return lastMating;	}

	/**
	 *  @return	Returns the cycle or time period when the citizen was born.
	 * 
	 */
	public int getBirthYear()	{	return birthYear;	}

	/**
	 *  @return	Returns a string describing the citizen's cause of death.
	 * 
	 */
	public String getCauseOfDeath()
								{	return causeOfDeath;}

	/**
	 *  @return	Returns the ranking previously calculated for the citizen. This influences 
	 * the likelihood of selection by other citizens looking to mate.
	 * 
	 */
	public int getRanking()		{	return ranking;		}

	/**
	 * @return
	 */
	public boolean[] getGenoMeme()
	{
		return genoMeme;
	}
	/**
	 * @return
	 */
	public boolean[] getPhenoMeme()
	{
		return phenoMeme;
	}
	public boolean getPhenoMeme(int tagIndex)
	{
		return phenoMeme[tagIndex];
	}
	/**
	 * @return
	 */
	public int getMemeTally()
	{
		int trueCount = 0;
		//int cultTagLen = GoLconst.CITIZEN_CULTURE_TAG;
		for(short i=0; i<cultTagLen; i++)
			if(this.phenoMeme[i])
				trueCount++;
		return trueCount;
	}

	
	/**
	 *  @return	Returns the age, defined as the birth year subtracted from the current cycle or 
	 * time period.
	 */
	public int getAge()			{	return (GoLconst.timePeriod - this.birthYear); }
	
	/**
	 * Calculated value, depicting the number of units of spice available to the citizen.
	 * Each unit is equal to the metabolic rate for spice.
	 */
	public int getSpUnits()		{	return spUnits;	}
	/**
	 * Calculates value, depicting the number of units of spice available to the citizen.
	 * Each unit is equal to the metabolic rate for spice.
	 */
	public int setSpUnits()		
	{	spUnits =  (int)spice/metabSpice;
		return spUnits;
	}	
	
	/**
	 * Calculated value, depicting the number of units of sugar available to the citizen.
	 * Each unit is equal to the metabolic rate for sugar.
	 */
	public int getSuUnits()		{	return suUnits;	}
	/**
	 * Calculates value, depicting the number of units of sugar available to the citizen.
	 * Each unit is equal to the metabolic rate for sugar.
	 */
	public int setSuUnits()		
	{	suUnits =  (int)sugar/metabSugar;
		return suUnits;
	}	

	/**
	 * Calculated value. Represents the Marginal Rate of Substitution of spice for 
	 * each unit of sugar.
	 */
	public float getMRS()		{	return MRS; }
	/**
	 * Calculates value. Represents the Marginal Rate of Substitution of spice for 
	 * each unit of sugar. Calculated by deriving the consumption units of spice (spUnits) 
	 * and sugar(suUnits).
	 * 	consumption unit = quantity / metabolism
	 * MRS is then derived by dividing spUnits by suUnits. MRS results less than 1 
	 * signify a preference for spice, results greater than 1 conversely indicate a 
	 * preference for sugar. 
	 */
	public float setMRS()		
	{	MRS = (spice/metabSpice) / (sugar/metabSugar);
		return MRS;
	}

	/**
	 *  @return	Returns an estimated Time Until Death. Based on sugar & spice consumption units 
	 * available. Is taken as the lower of the two values and assumes no more goods are 
	 * added to the citizen's stockpile.
	 */
	public int getTUD()		{	return TUD; }
	/**
	 * Calculates and returns an estimated Time Until Death. Based on sugar & spice 
	 * consumption units available. Is taken as the lower of the two values and assumes 
	 * no more goods are added to the citizen's stockpile.
	 * 
	 */
	public int setTUD()		
	{	TUD = ( suUnits < spUnits ? suUnits : spUnits );
		return TUD;
	}
	
	/**
	 *  @return	Returns the surplus available for trade. Surplus is defined as the amount of any
	 * one good that is in excess of what is required to ensure TUD is not negatively 
	 * affected.
	 * 
	 */
	public int getSurplus()		{	return surplus; }
	/**
	 *@return	Calculates and returns the surplus available for trade. Surplus is defined as the amount of any
	 * one good that is in excess of what is required to maintain current TUD.
	 * 
	 */
	public int setSurplus()		
	{	
		if( MRS > 1  )
		{	surplus = (int)spice - (metabSpice*TUD);
			//If surplus <= metabolism Then Set surplus to Zero
			surplus = surplus > (metabSpice*2) ? surplus : 0;
		} 
		else if( MRS < 1  )
		{	surplus = (int)sugar - (metabSugar*TUD);
			//If surplus <= metabolism Then Set surplus to Zero
			surplus = surplus > (metabSugar*2) ? surplus : 0;
		}
		else
			surplus = 0;	//MRS == 1, 
		
		return surplus;
	}
	
	/**
	 * Resets the variable that is used to generating unique ID's for each new citizen.
	 * 
	 */
	public static synchronized void resetID()
	{	citizenID = 0;	}
	/**
	 *  @return	Returns a unique numeric ID and increments it in preparation for the next request.
	 * 
	 */
	public static synchronized int setID()
	{	return citizenID++;	}

	/**
	 * Stores the column representing a current or designated location within the 
	 * citizen object.
	 * 
	 * @param	col	column identifying cell where citizen is currently located.
	 */
	public synchronized void setCol(int col)
	{	homeCol = col;	}

	/**
	 * Stores the row representing a current or designated location within the 
	 * citizen object.
	 * 
	 * @param	row	row identifying cell where citizen is currently located.
	 */
	public synchronized void setRow(int row)
	{	homeRow = row;	}

	/**
	 * Sets citizen vision to a specified or random value.
	 * 
	 * @param	visionVal	citizen vision
	 */
	public synchronized int setVision(int visionVal)
	{	if (visionVal >= 0 )
		{	this.vision = visionVal;	}
		else	//visionVal = -1 then assign random value to vision
		{	this.vision =  ((int)(Math.random() * GoLconst.VISION_MAX));
			if(this.vision == 0)
				this.vision = 1;	//no blind citizen
		}
		return vision;
	}

	/**
	 * Randomly choses newborn child's genetic and cultural attributes from 
	 * one or the other parent.
	 * @param	citizen	parent #1
	 * @param	citizen	parent #2
	 */
	private synchronized void setInheritAttributes(Citizen citizen, Citizen mate)
	{	if( (int)(Math.random()*2) == 1 )
			this.vision = citizen.vision;
		else this.vision = mate.vision;

		if( (int)(Math.random()*2) == 1 )
			this.metabSugar = citizen.metabSugar;
		else this.metabSugar = mate.metabSugar;

		if( (int)(Math.random()*2) == 1 )
			this.metabSpice = citizen.metabSpice;
		else this.metabSpice = mate.metabSpice;

		this.setCultureTags(citizen,mate);
	}
	
	/**
	 * Sets citizen sugar metabolism to a specified or random value.
	 * 
	 * @param	metabolismVal	sugar metabolism of citizen
	 */
	public synchronized int setMetabSugar(int metabolismVal)
	{	int dCount = getDiseaseCount();
		if (metabolismVal >= 0 )
		{	this.metabSugar = metabolismVal;	}
		else	//metabolismVal = -1 then assign random value to metabolism
		{	this.metabSugar = ((int)((Math.random() * GoLconst.METABOLISM_MAX_SUGAR)
								+ (dCount * GoLconst.DISEASE_COST_SUGAR)));
			if(this.metabSugar == 0)
				this.metabSugar = 1;	//a citizen's gotta eat something
		}
		return metabSugar;
	}

	/**
	 * Sets citizen spice metabolism to a specified or random value.
	 * @param	metabolismVal	spice metabolism of citizen
	 */
	public synchronized int setMetabSpice(int metabolismVal)
	{	int dCount = getDiseaseCount();
		if (metabolismVal >= 0 )
		{	this.metabSpice = metabolismVal;	}
		else	//metabolismVal = -1 then assign random value to metabolism
		{	this.metabSpice = ((int)((Math.random() * GoLconst.METABOLISM_MAX_SPICE)
								+ (dCount * GoLconst.DISEASE_COST_SPICE)));
			if(this.metabSpice == 0)
				this.metabSpice = 1;	//a citizen's gotta eat something
		}
		return metabSpice;
	}

	/**
	 * Sets citizen sugar to a specified or random value.
	 * @param	sugarVal	value indicating how sugar stockpile of citizen is to change.
	 */
	public synchronized  float setSugar(float sugarVal)
	{	if (sugarVal >= 0f )
		{	this.sugar = sugarVal;	}
		else	//sugarVal = -1 then assign random value to sugar
		{	this.sugar = ((float)(( Math.random() *
				(GoLconst.SUGAR_MAX_CITIZEN - GoLconst.SUGAR_MIN_CITIZEN) )
				+ GoLconst.SUGAR_MIN_CITIZEN) );	}
		return sugar;
	}

	/**
	 * Sets citizen spice to a specified or random value.
	 * @param	spiceVal	value indicating how spice stockpile of citizen is to change.
	 */
	public synchronized  float setSpice(float spiceVal)
	{	if (spiceVal >= 0f )
		{	this.spice = spiceVal;	}
		else	//sugarVal = -1 then assign random value to sugar
		{	this.spice = ((float)(( Math.random() *
				(GoLconst.SPICE_MAX_CITIZEN - GoLconst.SPICE_MIN_CITIZEN) )
				+ GoLconst.SPICE_MIN_CITIZEN) );	}
		return spice;
	}

	/**
	 * Subtracts one consumption unit (sugar metabolism) from the sugar stockpile.
	 *  @return	Returns true if sufficient sugar available for consumption
	 */
	public synchronized  boolean eatSugar()
	{	//Consume sugar
		float diseaseCost = 0f;
		if(GoLconst.DISEASE_COST_SUGAR > 0)
			diseaseCost += GoLconst.DISEASE_COST_SUGAR * this.diseaseList.size();
//System.out.print("\nDisease sugar cost = " + diseaseCost);			
		this.sugar -= (this.metabSugar + diseaseCost);
		//If not enough sugar available then return false
		if(this.sugar >= 0f)	{	return true;	}
		else					{	return false;	}
	}

	/**
	 * Subtracts one consumption unit (spice metabolism) from the spice stockpile.
	 *  @return	Returns true if sufficient spice available for consumption
	 */
	public synchronized  boolean eatSpice()
	{	//Consume spice
		float diseaseCost = 0f;
		if(GoLconst.DISEASE_COST_SPICE > 0)
			diseaseCost += GoLconst.DISEASE_COST_SPICE * this.diseaseList.size();
//System.out.println(",\tspice cost = " + diseaseCost);
		this.spice -= (this.metabSpice + diseaseCost);
		//If not enough spice available then return false
		if(this.spice >= 0f)	{	return true;	}
		else					{	return false;	}
	}

	/**
	 * Transfers an initial inheritance of resources from parents to the newborn 
	 * child and subtracts that amount from the parents.
	 * @param citizen	parent #1
	 * @param mate		parent #2
	 * @see	GoLconst#INHERIT_INITIAL_WEALTH	INHERIT_INITIAL_WEALTH
	 */
	private synchronized void transferWealth(Citizen citizen, Citizen mate)
	{
		int cSugar, mSugar, cSpice, mSpice;
		if( GoLconst.INHERIT_INITIAL_WEALTH )
		{	cSugar = (int)(citizen.inheritSugar / 2);	//child inherits 1/2
			mSugar = (int)(mate.inheritSugar / 2);		//inheritance of parents
			cSpice = (int)(citizen.inheritSpice / 2);
			mSpice = (int)(mate.inheritSpice / 2);
		} else
		{	cSugar = (int)(citizen.sugar / 2);			//child inherits 1/2 of
			mSugar = (int)(mate.sugar / 2);				//all wealth of each parent
			cSpice = (int)(citizen.spice / 2);
			mSpice = (int)(mate.spice / 2);
		}
		this.inheritSugar = cSugar + mSugar;
		this.inheritSpice = cSpice + mSpice;
		this.sugar 		  = this.inheritSugar;
		this.spice 		  = this.inheritSpice;
		citizen.sugar	-= cSugar;
		citizen.spice	-= cSpice;
		mate.sugar		-= mSugar;
		mate.spice		-= mSpice;
	}

	/**
	 * Sets citizen lifespan to a specified or random value.
	 * @param	ageVal	if zero, age is determined as a random value within a specified 
	 * life expectancy range. if a positive value, then citizen's age is set to that value.
	 * @see	GoLconst#LIFE_EXPECTANCY_MIN	LIFE_EXPECTANCY_MIN
	 * @see	GoLconst#LIFE_EXPECTANCY_MAX	LIFE_EXPECTANCY_MAX
	 */
	public synchronized  void setLifeSpan(int ageVal)
	{	if (ageVal >= 0 )
		{	this.lifeSpan = ageVal;	}
		else	//assign random value between min & max life expectancy
		{	this.lifeSpan = ((int)(Math.random() *
				 ( GoLconst.LIFE_EXPECTANCY_MAX - GoLconst.LIFE_EXPECTANCY_MIN ) ));
			this.lifeSpan += GoLconst.LIFE_EXPECTANCY_MIN;
		}
	}

	/**
	 * Sets citizen sex to a specified or random value.
	 * @return	returns a flag denoting sex determined for the citizen.
	 */
	public synchronized  int setSex()
	{	if( GoLconst.SEX_RATIO >= (float)Math.random() )
			return 1;	//male
		else return 0;	//female
	}
	
	/**
	 * Sets citizen personality to a random value.
	 * @return	personality as integer, 1 for 'bull', 0 for 'bear'
	 * @see	GoLconst#PERSONALITY_RATIO	PERSONALITY_RATIO
	 */
	public synchronized  int setPersona()
	{	if( GoLconst.PERSONALITY_RATIO > (float)Math.random() )
			return 1;	//gambler
		else return 0;	//cautious or risk averse
	}
	
	/**
	 * Sets the values for a newborn citizen's parents
	 * @param citizen	parent #1 (father or mother depending upon sex)
	 * @param mate		parent #2 (father or mother depending upon sex)
	 */
	private synchronized void setParents(Citizen citizen, Citizen mate)
	{	if( citizen.sex == 1 )	//male
		{	this.father = citizen.id;
			this.mother = mate.id;
		} else
		{	this.father = mate.id;
			this.mother = citizen.id;
		}
	}

	/**
	 * Adds citizen to child list for inclusion into general list of citizens at 
	 * the end of the current cycle.
	 * @param	child	newborn to be added to list of children born in current cycle.
	 * @return	number of children in list
	 */
	public int addChild( Citizen child )
	{	childList.add( child );
		return childList.size();
	}

	/**
	 * Enables accessing the next child on citizen's list of children
	 * @param	childIndex	index pointing to a specific child in the child list.
	 * @return	Returns next child on list, null if no more children.
	 */
	public Citizen getNextChild( int childIndex )
	{	int childCount = childList.size();
		if( childIndex >= childCount )
		{	return null;	}
		return ( (Citizen) childList.get(childIndex) );
	}

	/**
	 * Verifies existence of children on citizens child list. Removes offspring
	 * that may have perished before the death of the parent.
	 * @return	Returns count of children in the child list.
	 */
	public int checkChildren()
	{
		int cntr = 0;
		int childCount = childList.size();
		if( childCount == 0 )
			return 0;

		Citizen child;
		while( cntr < childCount )
		{	child = (Citizen) childList.get(cntr++);
			if( child == null )
			{	//Child is Dead
				childCount--;
				childList.remove(child);
				continue;
			}
		}
		child = null;
		//Return num of children still alive
		return childCount;	//childList.size()
	}

	/**
	 * Sets newborn child's family name from a random list of names
	 * @see	GoLconst#CREATE_UNIQUE_SURNAMES	CREATE_UNIQUE_SURNAMES
	 */
	private synchronized void setFamily()
	{	if( surnames.isEmpty() )
		{	createFamilyNameList();		}
		int index = (int)( Math.random()* surnames.size() );
		this.family = ( (String)surnames.get(index) );

		if( GoLconst.CREATE_UNIQUE_SURNAMES )
			surnames.remove(index);
	}
	/**
	 * Selects newborn child's family name from one or the other parent
	 * @see	GoLconst#INHERIT_FAMILY_FATHER	INHERIT_FAMILY_FATHER
	 */
	private synchronized void setFamily(Citizen citizen, Citizen mate)
	{	if( GoLconst.INHERIT_FAMILY_FATHER )	//child gets father's family name
			this.family = citizen.sex == 1 ? citizen.family : mate.family;
		else this.family = citizen.sex == 0 ? citizen.family : mate.family;
	}

	/**
	 * Updates last mating of citizen to current cycle or time period.
	 * @param	generation	current time period.
	 */
	public synchronized void setLastMating(int generation)
	{	this.lastMating = generation;	}

	/**
	 * Stores the string describing cause of death in citizen.
	 * @param	cause	String explaining cause of death.
	 */
	public synchronized void setCauseOfDeath(String cause)
	{	this.causeOfDeath = cause;	}

	/**
	 * Recalculates ranking for citizen. This influences the likelihood of selection 
	 * by other citizens looking to mate.
	 * Could also affect cultural transmission.
	 * @return	returns the citizen's current rank as an integer 
	 */
	public synchronized int setRanking()
	{	this.ranking = 0;
		this.ranking += (int)(this.sugar/GoLconst.SUGAR_LEVEL_POOR);
		this.ranking += (int)(this.spice/GoLconst.SPICE_LEVEL_POOR);
		this.ranking += this.inheritSugar >= HIGH_ENDOWMENT_SUGAR ? 1 : 0;
		this.ranking += this.inheritSpice >= HIGH_ENDOWMENT_SPICE ? 1 : 0;
		this.ranking += this.vision	> (0.5 * GoLconst.VISION_MAX) ? 1 : 0;
		this.ranking += this.metabSugar	< (0.5 * GoLconst.METABOLISM_MAX_SUGAR) ? 1 : 0;
		this.ranking += this.metabSpice	< (0.5 * GoLconst.METABOLISM_MAX_SPICE) ? 1 : 0;
		return ranking;
	}

	/**
	 * Generates culture tags for citizen randomly.
	 * Called when a citizen is randomly generated
	 * @see	GoLconst#CITIZEN_CULTURE_TAG	CITIZEN_CULTURE_TAG
	 */
	public synchronized void setCultureTags()
	{	if(cultTagLen != GoLconst.CITIZEN_CULTURE_TAG)
		{	//generate a random string of boolean values with same length as culture tag
			cultTagLen		= GoLconst.CITIZEN_CULTURE_TAG;
			cultTagMax = (long)Math.pow(2,((cultTagLen < 31)? cultTagLen : 31));
		}
		long randomNo	= (long)( ((float)Math.random()) * ((float)cultTagMax) );
		String randomString = Long.toBinaryString(randomNo);
		int stringLen = cultTagLen - randomString.length();
		
		for(short i=0; i<cultTagLen; i++)
		{	if( i >= stringLen && (randomString.charAt(i-stringLen) == '1') )
			{	this.genoMeme[i] = true;
				this.phenoMeme[i] = true; 
			} 
			else 
			{	this.genoMeme[i] = false;
				this.phenoMeme[i] = false;
			} 
		}
		
	}

	/**
	 * Generates culture tags for citizen.  
	 * Tags may be randomly generated or derived from parents genoType
	 * Called when a generated citizen inherits parent attributes. For each bit 
	 * attribute (meme/tag), if both parent tags match, child randomly inherits tag 
	 * of one or the other parent.
	 * @param citizen	parent #1
	 * @param mate		parent #2
	 * @see	GoLconst#CITIZEN_CULTURE_TAG	CITIZEN_CULTURE_TAG
	 */
	public synchronized void setCultureTags(Citizen citizen, Citizen mate)
	{	//int cultTagLen 				= GoLconst.CITIZEN_CULTURE_TAG;
		boolean genoCitizen[] 	= citizen.getPhenoMeme(),
				genoMate[] 		= mate.getPhenoMeme();
			
		for(short i=0; i<cultTagLen; i++)
		{	if( genoCitizen[i] == genoMate[i] )
			{	genoMeme[i] = genoCitizen[i]; 
				phenoMeme[i] = genoCitizen[i];
			} 
			else 
			{	genoMeme[i] = ((Math.random() >= .5) ? true : false);
				phenoMeme[i] = genoMeme[i];
			} 
		}
	}

	/**
	 * Tallies cultural group membership. Executed after each cycle
	 * @see	GoLconst#CITIZEN_GROUP_INTERVAL	CITIZEN_GROUP_INTERVAL
	 * @see	GoLconst#CITIZEN_CULTURE_TAG	CITIZEN_CULTURE_TAG
	 */
	public static void setDistrGroups()
	{
		int gCount = groupCount - 1;
		for(int i=0; i<gCount; i++)
		{	rangeArr[i] = GoLconst.CITIZEN_GROUP_INTERVAL[i];	}
		rangeArr[gCount] = GoLconst.CITIZEN_CULTURE_TAG;
	}
	/**
	 * Tallies cultural group membership and initializes color array determining 
	 * color of individual groups.
	 * @see	GoLconst#CITIZEN_GROUP_INTERVAL	CITIZEN_GROUP_INTERVAL
	 * @see	GoLconst#CITIZEN_CULTURE_TAG	CITIZEN_CULTURE_TAG
	 */
	public static void setDistrGroupsInit()
	{			
		int gCount = groupCount - 1;
		int a,b,c;
		for(int i=0; i<gCount; i++)
		{	a = i*3; b = a+1; c = b+1;
			rangeArr[i]   = GoLconst.CITIZEN_GROUP_INTERVAL[i];
			rangeColor[a] = GoLconst.CITIZEN_GROUP_COLOR[a];
			rangeColor[b] = GoLconst.CITIZEN_GROUP_COLOR[b];
			rangeColor[c] = GoLconst.CITIZEN_GROUP_COLOR[c];
		}
		rangeArr[gCount] 	   = GoLconst.CITIZEN_CULTURE_TAG;
		rangeColor[gCount*3]   = GoLconst.CITIZEN_GROUP_COLOR[gCount*3];
		rangeColor[gCount*3+1] = GoLconst.CITIZEN_GROUP_COLOR[gCount*3+1];
		rangeColor[gCount*3+2] = GoLconst.CITIZEN_GROUP_COLOR[gCount*3+2];
		//rangeColor[gCount] = GoLconst.CITIZEN_GROUP_COLOR[gCount];
	}

	/**
	 * Determines if citizen is sibling of another. Siblings share "both" parents
	 * @param	zenB	other citizen whose relation is being examined.
	 * @return	Returns true if two citizens share both parents.
	 */
	public synchronized  boolean isSiblingOf(Citizen zenB)
	{	return this.father != 0 && this.father == zenB.father && this.mother == zenB.mother; }
	/**
	 * Determines whether this citizen is child of another.
	 * @param	zenB	other citizen whose relation is being examined.
	 * @return	Returns true if citizen is a child of the citizen passed as a parameter 
	 * to this method..
	 */
	public synchronized  boolean isChildOf(Citizen zenB)
	{	return ( this.father != 0 && (zenB.id == this.father || zenB.id == this.mother) ); }
	/**
	 * Determines whether this citizen is parent of another.
	 * @return	Returns true if citizen is a parent of the citizen passed as a parameter to 
	 * this method.
	 * @param	zenB	other citizen whose relation is being examined.
	 */
	public synchronized  boolean isParentOf(Citizen zenB)
	{	return ( zenB.father != 0 && (this.id == zenB.father || this.id == zenB.mother) );	}
	/**
	 * Determines if citizen is parent, child or sibling of another.
	 * @param	zenB	other citizen whose relation is being examined.
	 * @return	Returns true if citizen is related to the other.
	 */
	public synchronized  boolean isKinOf(Citizen zenB)
	{	return this.isParentOf(zenB) || this.isChildOf(zenB) || this.isSiblingOf(zenB);	}
	/**
	 *  @return	Returns true, if the personality type for the citizen is bearish OR
	 * below the poverty level.
	 * @return	Returns true if citizen personality is bearish or below poverty level.
	 * @see	GoLconst#SUGAR_LEVEL_POOR	SUGAR_LEVEL_POOR
	 * @see	GoLconst#SPICE_LEVEL_POOR	SPICE_LEVEL_POOR
	 */
	public boolean isCautious()
	{	return ( (persona==0) || isPoor() );	}
	/**
	 * Returns true is resource levels are below the poverty line.
	 * @return	Returns true if sugar or spice consumption units are less than or equal to 
	 * wealth levels specified by global variables.
	 * @see	GoLconst#SUGAR_LEVEL_POOR	SUGAR_LEVEL_POOR
	 * @see	GoLconst#SPICE_LEVEL_POOR	SPICE_LEVEL_POOR
	 */
	public boolean isPoor()
	{	return spice <= GoLconst.SPICE_LEVEL_POOR || sugar <= GoLconst.SUGAR_LEVEL_POOR; }
	
	/**
	 * @return	Returns true when MRS > 1. This indicates that the citizen posseses a 
	 * surplus of spice and has a greater preference for sugar.
	 */
	public boolean prefersSugar()	{   return ( MRS > 1 );		}
	/**
	 * @return	Returns true when MRS < 1. This indicates that the citizen posseses a 
	 * surplus of sugar and has a greater preference for spice.
	 */
	public boolean prefersSpice()	{	return ( MRS < 1 );		}

	/**
	 * @return	Returns true when citizen is 'Risk-Averse' and MRS < 1. This indicates that the 
	 * citizen will only trade for spice.
	 */
	public boolean needsSpice()	{	return ( persona == 0 && MRS < 1 );		}
	/**
	 *  @return	Returns true when citizen is 'Risk-Averse' and MRS > 1. This indicates that the 
	 * citizen will only trade for sugar.
	 */
	public boolean needsSugar()	{   return ( persona == 0 && MRS > 1 );		}

	/**
	 *  @return	Returns true if two MRS values supplied as parameters have a preference for 
	 * the same good. This indicates that both citizens whose MRS are being compared
	 * prefer the same good. Thus if both MRS are less than one, both prefer spice,
	 * if both MRS are greater than one, both prefer sugar.
	 * Note: this rules out a trade only if both parties are risk-averse. A trade
	 * can always be made such that both parties benefit except when both have the 
	 * same MRS.
	 * @param	MRSA	Marginal Rate of Substitution of spice to sugar for Citizen
	 * @param	MRSB	Marginal Rate of Substitution of spice to sugar for Trader.
	 */
	public static boolean preferSame(float MRSA, float MRSB)
	{	return ( ( (MRSA < 1)  && (MRSB < 1) ) ||	// both prefer spice
				 ( (MRSA >= 1) && (MRSB >= 1) )  );		}
	
	/**
	 * @return	Returns true if the citizen's age falls between the range of values 
	 * signifying childhood and old age. These values can be configured in the 
	 * global constants file.
	 * 
	 */
	public synchronized  boolean ofMatingAge()
	{
		int age = GoLconst.timePeriod - this.birthYear;
		int lastChild = GoLconst.timePeriod - this.lastMating;
		if( sex == 0 )	//female
		{	if( age >= GoLconst.MATING_FEMALE_MIN
					&& age <= GoLconst.MATING_FEMALE_MAX
					&& lastChild > GoLconst.MATING_FEMALE_GAP )
			{	return true;	}
		}
		else	//male
		{	if( age >= GoLconst.MATING_MALE_MIN
					&& age <= GoLconst.MATING_MALE_MAX
					&& lastChild > GoLconst.MATING_MALE_GAP )
			{	return true;	}
		}
		return false;
	}

	/**
	 * @return	Returns true if the citizen's age falls within the range that defines childhood.
	 */
	public synchronized  boolean isChild()
	{
		int age = GoLconst.timePeriod - this.birthYear;
		if( sex == 0 )	//female
		{	if( age <= GoLconst.MATING_FEMALE_MIN )
			{	return true;	}
		}
		else	//male
		{	if( age <= GoLconst.MATING_MALE_MIN )
			{	return true;	}
		}
		return false;
	}

	/**
	 * @return	Returns true if the citizen's age falls within the range that defines senior 
	 * citizens.
	 */
	public synchronized  boolean isSenior()
	{
		int age = GoLconst.timePeriod - this.birthYear;
		if( sex == 0 )	//female
		{	if( age >= GoLconst.MATING_FEMALE_MAX )
			{	return true;	}
		}
		else	//male
		{	if( age >= GoLconst.MATING_MALE_MAX )
			{	return true;	}
		}
		return false;
	}

	/**
	 * @return	Returns true if the citizen has enough sugar to support prospective offspring.
	 */
	public synchronized  boolean hasSurplus()
	{	return ( this.sugar >= GoLconst.SUGAR_MIN_CITIZEN &&
				this.spice >= GoLconst.SPICE_MIN_CITIZEN);
		//return (sugar >= inheritSugar && spice >= inheritSpice );
	}

	/**
	 * @return	Returns true if the period specified in the global constants file has passed
	 * since the last successful mating for the citizen.
	 */
	public synchronized boolean pastMatingInterval()
	{	int gap = GoLconst.timePeriod - this.lastMating;
		if(sex == 0)	
			return gap > GoLconst.MATING_FEMALE_GAP;
		else
			return gap > GoLconst.MATING_MALE_GAP;
	}

	/**
	 * Determines whether citizen and prospective partner are of the opposite sex.
	 * @param	zenB	other citizen whose relation is being examined.
	 * @return	Returns true if the two prospective mates belong to the opposite sex.
	 */
	public synchronized  boolean ofCompatibleSex(Citizen zenB)
	{	return (this.sex != zenB.sex);	}

	/**
	 * Determines whether citizen is within allotted lifespan.
	 * @return	Returns true if the citizen is still within the determined lifespan.
	 */
	public synchronized  boolean isAlive()
	{	return ( (GoLconst.timePeriod - this.birthYear) <= this.lifeSpan );	}


	/**
	 * Generates key statistics describing citizen.
	 * @param	message	String to precede display of important citizen stats.
	 * @return	Returns a string describing some of the key attributes for the citizen.
	 */
	public String showStats( String message )
	{
		String spacer = "", spacer2 = "";
		if( homeCol < 10) 			{	spacer += "  ";	}
		else if( homeCol < 100) 	{	spacer += " ";	}
		if( homeRow < 10) 			{	spacer += "  ";	}
		else if( homeRow < 100) 	{	spacer += " ";	}

		int spaceCntr = (15 - family.length() );
		while( spaceCntr-- > 0)
			spacer2 += "  ";	//adjusting for varying length of family name

		return ( message + spacer + id + ") " + family.trim()
			+ spacer2 + "[" + homeCol + "][" + homeRow + "]\t"
			+ vision + "V  "
			+ customFormat("###.##", sugar) +"Su/" + metabSugar + "SuM/" 
			+ customFormat("###.##", inheritSugar) + "SuI\t"
			+ customFormat("###.##", spice) +"Sp/" + metabSpice + "SpM/" 
			+ customFormat("###.##", inheritSpice) + "SpI\t"
			+ (isMale() ? "M" : "Fem") + "ale  "
			+ (GoLconst.timePeriod-birthYear)  + "(" + lifeSpan + ") yrs old"
			+ ", Rank "	+ ranking + "\tRisk " + (isCautious() ? "Averse" : "Taker") 
			+ "\n\t MRS=" + customFormat("###.###", getMRS())
			+ "\t Dies in = " + getTUD() + "\t surplus=" + getSurplus() 
			+ "\tGeno = "	+ showGenoTags() + "\tPheno = " + showPhenoTags() + "\n"
			+ "Immune Sys = " + getPImmuneStr() + "\n"
			);
	}
	
	/**
	 * Show genoType tag as a binary string. This string is distinct from the phenoType
	 * which represents the cultural attributes modified as the citizen interacts with 
	 * others on the Sugarscape.
	 * @return	cultural string inherited at birth.
	 */
	public String showGenoTags()
	{	String genoTags = "";
		//int cultTagLen = GoLconst.CITIZEN_CULTURE_TAG;
		for(short i=0; i<cultTagLen; i++)
		{	genoTags += genoMeme[i]?"1":"0";	}
		
		return genoTags;
	}

	/**
	 * Show phenoType tag as a binary string. This string is distinct from the genoType
	 * which represents the unmodified cultural attributes inherited by the system.
	 * @return	modified version of citizen's cultural string.
	 */
	public String showPhenoTags()
	{	String phenoTags = "";
		//int cultTagLen = GoLconst.CITIZEN_CULTURE_TAG;
		for(short i=0; i<cultTagLen; i++)
		{	phenoTags += phenoMeme[i]?"1":"0";	}
		
		return phenoTags;
	}

	/**
	 * Changes individual tags in the citizen's phenoType on the basis of interactions with
	 * other citizens on the Sugarscape.
	 * @param tagIndex	meme tag to flip
	 * @param tagValue	new value for meme tag
	 */
	public void setPhenoMeme(int tagIndex, boolean tagValue)
	{	phenoMeme[tagIndex] = tagValue;	}

	/**
	 * Paints the citizen in to one of eight possible shapes
	 * The classification comprises eight categories and are representedgraphically as icons
	 * drawn using a combination of ovals and rectangles. These geometric shapes may be 
	 * filled or hollow.<BR>
	 * Each citizen is represented by combining two circles (the figure eight) or 
	 * rectangles (ladder) placed one below the other. The choice of shapes is determined 
	 * by the 'vision' property of the citizen. The '8' signifies a high and the 'ladder' 
	 * signifies a low-value for vision. It follows that a citizen with a high vision will
	 * move around more vigorously, hence the term 'hyper'. Conversely the 'ladder' 
	 * citizens will move at a more liesurely pace and are categorized as 'slow'.<BR>
	 * Each circle & rectangle component of these icons may be 'filled' to signify a low 
	 * metabolic rate or 'hollow' to signify a high metabolism.<BR>
	 * Finally, the color of these icons indicate the level of their food stockpiles. 
	 * This may range from lighter shades of red indicating near-zero (< 10)consumption 
	 * units, to yellow indicating resonably sufficient reserves (> 10 & < 100), to green
	 * indicating more than a normal lifetime (> 100) supply of food.<BR>
	 * The table below summarizes the eight categories,
	 * <UL>
	 * <LI> <B>Hyper</B>	citizens with high vision and metabolism values.
	 * <LI> <B>Hyper, prefer Sugar</B>	citizens with high vision & sugar metabolism 
	 * but low spice metabolism. These are said to prefer sugar.
	 * <LI> <B>Hyper, prefer Spice</B>	citizens with high vision & spice metabolism 
	 * but low sugar metabolism.
	 * <LI> <B>Efficient</B>	citizens with high vision and low metabolism values.
	 * <LI> <B>Inefficient</B>	citizens with low vision and high metabolism values.
	 * <LI> <B>Slow, prefer Sugar</B>	citizens with low vision & spice metabolism 
	 * but high sugar metabolism.
	 * <LI> <B>Slow, prefer Spice</B>	citizens with low vision & sugar metabolism 
	 * but high spice metabolism.
	 * <LI> <B>Slow</B>	citizens with low vision and metabolism values.
	 * </UL>	
	 * 
	 * @param	g	the handle to the graphics object that provides access to all 
	 * drawing methods.
	 * @param	cellSize	current cell size
	 * 	
	 */
	public void paintZen(Graphics g, int cellSize)
	{
		int mColorVal;
		int mVision = vision > (0.5 * GoLconst.VISION_MAX)? 'H' : 'L';
		int mSu		= metabSugar > (0.5 * GoLconst.METABOLISM_MAX_SUGAR)? 'H':'L';
		int mSp		= metabSpice > (0.5 * GoLconst.METABOLISM_MAX_SPICE)? 'H':'L';

		//**Determine Color **
		if(GoLconst.CITIZEN_COLOR == 1)		//based on food reserves
		{	mColorVal = this.getTUD();

			
			if( getFather() != 0 && getAge() < (isMale()?
						GoLconst.MATING_MALE_MIN : GoLconst.MATING_FEMALE_MIN) )
				g.setColor( new Color(GoLconst.CITIZEN_COLOR_CHILD,
					GoLconst.CITIZEN_COLOR_CHILD,GoLconst.CITIZEN_COLOR_CHILD) );
			else if( getAge() > (isMale()?
						GoLconst.MATING_MALE_MAX : GoLconst.MATING_FEMALE_MAX) )
				g.setColor( new Color(GoLconst.CITIZEN_COLOR_SENIOR,
					GoLconst.CITIZEN_COLOR_SENIOR,GoLconst.CITIZEN_COLOR_SENIOR) );
			else if( mColorVal < 10 )	//Red - gets darker with increasing reserve
			{	
				mColorVal = 255 - (mColorVal*10);
				mColorVal = (mColorVal > 0 && mColorVal < 240) ? mColorVal : 240;  
				g.setColor( new Color(mColorVal,0,0) );
			}
			else if( mColorVal > 100 )	//Green - gets darker with increasing reserve
			{	
				mColorVal = (mColorVal > 140) ? 100 : mColorVal-100;  
				mColorVal = 140 - mColorVal;
				g.setColor( new Color(0,mColorVal,0) );
			}
			else	//Orange changes to yellow as sugar reserve increases
				g.setColor( new Color(254,155+mColorVal,0) );
		}
		else if(GoLconst.CITIZEN_COLOR == 2)		//based on group membership
		{	//groupCount = GoLconst.CITIZEN_GROUP_INTERVAL.length + 1;
			int memeCount = this.getMemeTally();
			if(memeCount <= rangeArr[0])
				g.setColor( new Color(rangeColor[0],rangeColor[1],rangeColor[2]) );	//blue
			else if(memeCount <= rangeArr[1])
				g.setColor( new Color(rangeColor[3],rangeColor[4],rangeColor[5]) );	//red
			else if(memeCount <= rangeArr[2])
				g.setColor( new Color(rangeColor[6],rangeColor[7],rangeColor[8]) );	//green
			else if(memeCount <= rangeArr[3])
				g.setColor( new Color(rangeColor[9],rangeColor[10],rangeColor[11]) );	//yellow
			else if(memeCount <= rangeArr[4])
				g.setColor( new Color(rangeColor[12],rangeColor[13],rangeColor[14]) );	//brown
			else if(memeCount <= rangeArr[5])
				g.setColor( new Color(rangeColor[15],rangeColor[16],rangeColor[17]) );	//gray
			else //if(memeCount <= rangeArr[3])
				g.setColor( new Color(rangeColor[18],rangeColor[19],rangeColor[20]) );	//black			
		}
		else if(GoLconst.CITIZEN_COLOR == 3)		//based on toll exerted by diseases
		{
			float tollSu = diseaseList.size()* GoLconst.DISEASE_COST_SUGAR;
			float tollSp = diseaseList.size()* GoLconst.DISEASE_COST_SPICE;
			mColorVal = this.getTUD();
			
			// disease free, green gets darker with increasing reserve
			if(tollSu == 0 && tollSp == 0)
			{
				mColorVal = (mColorVal > 140) ? 100 : mColorVal-100;  
				mColorVal = 140 - mColorVal;
				g.setColor( new Color(0,255,0) );					
			}
			// significant cost incurred due to infections, 
			//red gets darker with increasing reserve
			else if(tollSu >= 1 || tollSp >= 1)
			{	
				mColorVal = 255 - (mColorVal*10);
				mColorVal = (mColorVal > 0 && mColorVal < 240) ? mColorVal : 240;  
				g.setColor( new Color(255,0,0) );
			}
			else //Orange 
				g.setColor( new Color(254,200,0) );
		}

		// **Determine Visual Attributes based on vision, metabolism**
		// HYPER -->  HIGH vision / HIGH sugar metab / HIGH spice metab
		if( mVision == 'H' && mSu == 'H' && mSp == 'H' )
		{	g.drawOval(homeCol*cellSize+(cellSize/4), homeRow*cellSize,
								cellSize/2, cellSize/2);
			g.drawOval(homeCol*cellSize+(cellSize/4), homeRow*cellSize+(cellSize/2),
								cellSize/2, cellSize/2);
		}
		// HYPER, prefers Sugar --> HIGH vision / HIGH sugar metab / LOW spice metab
		else if( mVision == 'H' && mSu == 'H' && mSp == 'L' )
		{	g.drawOval(homeCol*cellSize+(cellSize/4), homeRow*cellSize,
								cellSize/2, cellSize/2);
			g.fillOval(homeCol*cellSize+(cellSize/4), homeRow*cellSize+(cellSize/2),
								cellSize/2+1, cellSize/2+1);
		}
		// HYPER, prefers Spice --> HIGH vision / LOW sugar metab / HIGH spice metab
		else if( mVision == 'H' && mSu == 'L' && mSp == 'H' )
		{	g.fillOval(homeCol*cellSize+(cellSize/4), homeRow*cellSize,
								cellSize/2+1, cellSize/2+1);
			g.drawOval(homeCol*cellSize+(cellSize/4), homeRow*cellSize+(cellSize/2),
								cellSize/2, cellSize/2);
		}
		// EFFICIENT -->  HIGH vision / LOW sugar metab / LOW spice metab
		else if( mVision == 'H' && mSu == 'L' && mSp == 'L' )
		{	g.fillOval(homeCol*cellSize+(cellSize/4), homeRow*cellSize,
								cellSize/2+1, cellSize/2+1);
			g.fillOval(homeCol*cellSize+(cellSize/4), homeRow*cellSize+(cellSize/2),
								cellSize/2+1, cellSize/2+1);
		}
		// INEfficient -->  LOW vision / HIGH sugar metab / HIGH spice metab
		else if( mVision == 'L' && mSu == 'H' && mSp == 'H' )
		{	g.drawRect(homeCol*cellSize+(cellSize/4), homeRow*cellSize,
								cellSize/2, cellSize/2);
			g.drawRect(homeCol*cellSize+(cellSize/4), homeRow*cellSize+(cellSize/2),
								cellSize/2, cellSize/2-1);
		}
		// SLOW -->  LOW vision / LOW sugar metab / LOW spice metab
		else if( mVision == 'L' && mSu == 'L' && mSp == 'L' )
		{	g.fillRect(homeCol*cellSize+(cellSize/4), homeRow*cellSize,
								cellSize/2+1, cellSize/2);
			g.fillRect(homeCol*cellSize+(cellSize/4), homeRow*cellSize+(cellSize/2),
								cellSize/2+1, cellSize/2);
		}
		// SLOW, prefers Sugar --> LOW vision / HIGH sugar metab / LOW spice metab
		else if( mVision == 'L' && mSu == 'H' && mSp == 'L' )
		{	g.drawRect(homeCol*cellSize+(cellSize/4), homeRow*cellSize,
								cellSize/2, cellSize/2);
			g.fillRect(homeCol*cellSize+(cellSize/4), homeRow*cellSize+(cellSize/2),
								cellSize/2+1, cellSize/2);
		}
		// SLOW, prefers Spice --> LOW vision / LOW sugar metab / HIGH spice metab
		else if( mVision == 'L' && mSu == 'L' && mSp == 'H' )
		{	g.fillRect(homeCol*cellSize+(cellSize/4), homeRow*cellSize,
								cellSize/2+1, cellSize/2);
			g.drawRect(homeCol*cellSize+(cellSize/4), homeRow*cellSize+(cellSize/2),
								cellSize/2, cellSize/2-1);
		}
	}

	/**
	 * The following collections is randomly assigned to the initial inhabitants. The
	 * method creates a list of surnames from which names are randomly assigned to 
	 * new citizens created at the beginning of each simulaton.
	 * The 2000 names in the list were picked from a list of most popular surnames
	 * in USA that was posted online.
	 * 
	 * @return	number of items left in surname list.
	 * @see	GoLconst#CREATE_UNIQUE_SURNAMES
	 */
	private int createFamilyNameList()
	{	String names[] =
		{	"SMITH", "JOHNSON", "WILLIAMS", "JONES", "BROWN", "DAVIS", "MILLER", "WILSON", "MOORE", "TAYLOR", "ANDERSON", "THOMAS", "JACKSON", "WHITE", "HARRIS", "MARTIN", "THOMPSON", "GARCIA", "MARTINEZ", "ROBINSON",
			"CLARK", "RODRIGUEZ", "LEWIS", "LEE", "WALKER", "HALL", "ALLEN", "YOUNG", "HERNANDEZ", "KING", "WRIGHT", "LOPEZ", "HILL", "SCOTT", "GREEN", "ADAMS", "BAKER", "GONZALEZ", "NELSON", "CARTER",
			"MITCHELL", "PEREZ", "ROBERTS", "TURNER", "PHILLIPS", "CAMPBELL", "PARKER", "EVANS", "EDWARDS", "COLLINS", "STEWART", "SANCHEZ", "MORRIS", "ROGERS", "REED", "COOK", "MORGAN", "BELL", "MURPHY", "BAILEY",
			"RIVERA", "COOPER", "RICHARDSON", "COX", "HOWARD", "WARD", "TORRES", "PETERSON", "GRAY", "RAMIREZ", "JAMES", "WATSON", "BROOKS", "KELLY", "SANDERS", "PRICE", "BENNETT", "WOOD", "BARNES", "ROSS",
			"HENDERSON", "COLEMAN", "JENKINS", "PERRY", "POWELL", "LONG", "PATTERSON", "HUGHES", "FLORES", "WASHINGTON", "BUTLER", "SIMMONS", "FOSTER", "GONZALES", "BRYANT", "ALEXANDER", "RUSSELL", "GRIFFIN", "DIAZ", "HAYES",
			"MYERS", "FORD", "HAMILTON", "GRAHAM", "SULLIVAN", "WALLACE", "WOODS", "COLE", "WEST", "JORDAN", "OWENS", "REYNOLDS", "FISHER", "ELLIS", "HARRISON", "GIBSON", "MCDONALD", "CRUZ", "MARSHALL", "ORTIZ",
			"GOMEZ", "MURRAY", "FREEMAN", "WELLS", "WEBB", "SIMPSON", "STEVENS", "TUCKER", "PORTER", "HUNTER", "HICKS", "CRAWFORD", "HENRY", "BOYD", "MASON", "MORALES", "KENNEDY", "WARREN", "DIXON", "RAMOS",
			"REYES", "BURNS", "GORDON", "SHAW", "HOLMES", "RICE", "ROBERTSON", "HUNT", "BLACK", "DANIELS", "PALMER", "MILLS", "NICHOLS", "GRANT", "KNIGHT", "FERGUSON", "ROSE", "STONE", "HAWKINS", "DUNN",
			"PERKINS", "HUDSON", "SPENCER", "GARDNER", "STEPHENS", "PAYNE", "PIERCE", "BERRY", "MATTHEWS", "ARNOLD", "WAGNER", "WILLIS", "RAY", "WATKINS", "OLSON", "CARROLL", "DUNCAN", "SNYDER", "HART", "CUNNINGHAM",
			"BRADLEY", "LANE", "ANDREWS", "RUIZ", "HARPER", "FOX", "RILEY", "ARMSTRONG", "CARPENTER", "WEAVER", "GREENE", "LAWRENCE", "ELLIOTT", "CHAVEZ", "SIMS", "AUSTIN", "PETERS", "KELLEY", "FRANKLIN", "LAWSON",
			"FIELDS", "GUTIERREZ", "RYAN", "SCHMIDT", "CARR", "VASQUEZ", "CASTILLO", "WHEELER", "CHAPMAN", "OLIVER", "MONTGOMERY", "RICHARDS", "WILLIAMSON", "JOHNSTON", "BANKS", "MEYER", "BISHOP", "MCCOY", "HOWELL", "ALVAREZ",
			"MORRISON", "HANSEN", "FERNANDEZ", "GARZA", "HARVEY", "LITTLE", "BURTON", "STANLEY", "NGUYEN", "GEORGE", "JACOBS", "REID", "KIM", "FULLER", "LYNCH", "DEAN", "GILBERT", "GARRETT", "ROMERO", "WELCH",
			"LARSON", "FRAZIER", "BURKE", "HANSON", "DAY", "MENDOZA", "MORENO", "BOWMAN", "MEDINA", "FOWLER", "BREWER", "HOFFMAN", "CARLSON", "SILVA", "PEARSON", "HOLLAND", "DOUGLAS", "FLEMING", "JENSEN", "VARGAS",
			"BYRD", "DAVIDSON", "HOPKINS", "MAY", "TERRY", "HERRERA", "WADE", "SOTO", "WALTERS", "CURTIS", "NEAL", "CALDWELL", "LOWE", "JENNINGS", "BARNETT", "GRAVES", "JIMENEZ", "HORTON", "SHELTON", "BARRETT",
			"OBRIEN", "CASTRO", "SUTTON", "GREGORY", "MCKINNEY", "LUCAS", "MILES", "CRAIG", "RODRIQUEZ", "CHAMBERS", "HOLT", "LAMBERT", "FLETCHER", "WATTS", "BATES", "HALE", "RHODES", "PENA", "BECK", "NEWMAN",
			"HAYNES", "MCDANIEL", "MENDEZ", "BUSH", "VAUGHN", "PARKS", "DAWSON", "SANTIAGO", "NORRIS", "HARDY", "LOVE", "STEELE", "CURRY", "POWERS", "SCHULTZ", "BARKER", "GUZMAN", "PAGE", "MUNOZ", "BALL",
			"KELLER", "CHANDLER", "WEBER", "LEONARD", "WALSH", "LYONS", "RAMSEY", "WOLFE", "SCHNEIDER", "MULLINS", "BENSON", "SHARP", "BOWEN", "DANIEL", "BARBER", "CUMMINGS", "HINES", "BALDWIN", "GRIFFITH", "VALDEZ",
			"HUBBARD", "SALAZAR", "REEVES", "WARNER", "STEVENSON", "BURGESS", "SANTOS", "TATE", "CROSS", "GARNER", "MANN", "MACK", "MOSS", "THORNTON", "DENNIS", "MCGEE", "FARMER", "DELGADO", "AGUILAR", "VEGA",
			"GLOVER", "MANNING", "COHEN", "HARMON", "RODGERS", "ROBBINS", "NEWTON", "TODD", "BLAIR", "HIGGINS", "INGRAM", "REESE", "CANNON", "STRICKLAND", "TOWNSEND", "POTTER", "GOODWIN", "WALTON", "ROWE", "HAMPTON",
			"ORTEGA", "PATTON", "SWANSON", "JOSEPH", "FRANCIS", "GOODMAN", "MALDONADO", "YATES", "BECKER", "ERICKSON", "HODGES", "RIOS", "CONNER", "ADKINS", "WEBSTER", "NORMAN", "MALONE", "HAMMOND", "FLOWERS", "COBB",
			"MOODY", "QUINN", "BLAKE", "MAXWELL", "POPE", "FLOYD", "OSBORNE", "PAUL", "MCCARTHY", "GUERRERO", "LINDSEY", "ESTRADA", "SANDOVAL", "GIBBS", "TYLER", "GROSS", "FITZGERALD", "STOKES", "DOYLE", "SHERMAN",
			"SAUNDERS", "WISE", "COLON", "GILL", "ALVARADO", "GREER", "PADILLA", "SIMON", "WATERS", "NUNEZ", "BALLARD", "SCHWARTZ", "MCBRIDE", "HOUSTON", "CHRISTENSEN", "KLEIN", "PRATT", "BRIGGS", "PARSONS", "MCLAUGHLIN",
			"ZIMMERMAN", "FRENCH", "BUCHANAN", "MORAN", "COPELAND", "ROY", "PITTMAN", "BRADY", "MCCORMICK", "HOLLOWAY", "BROCK", "POOLE", "FRANK", "LOGAN", "OWEN", "BASS", "MARSH", "DRAKE", "WONG", "JEFFERSON",
			"PARK", "MORTON", "ABBOTT", "SPARKS", "PATRICK", "NORTON", "HUFF", "CLAYTON", "MASSEY", "LLOYD", "FIGUEROA", "CARSON", "BOWERS", "ROBERSON", "BARTON", "TRAN", "LAMB", "HARRINGTON", "CASEY", "BOONE",
			"CORTEZ", "CLARKE", "MATHIS", "SINGLETON", "WILKINS", "CAIN", "BRYAN", "UNDERWOOD", "HOGAN", "MCKENZIE", "COLLIER", "LUNA", "PHELPS", "MCGUIRE", "ALLISON", "BRIDGES", "WILKERSON", "NASH", "SUMMERS", "ATKINS",
			"WILCOX", "PITTS", "CONLEY", "MARQUEZ", "BURNETT", "RICHARD", "COCHRAN", "CHASE", "DAVENPORT", "HOOD", "GATES", "CLAY", "AYALA", "SAWYER", "ROMAN", "VAZQUEZ", "DICKERSON", "HODGE", "ACOSTA", "FLYNN",
			"ESPINOZA", "NICHOLSON", "MONROE", "WOLF", "MORROW", "KIRK", "RANDALL", "ANTHONY", "WHITAKER", "OCONNOR", "SKINNER", "WARE", "MOLINA", "KIRBY", "HUFFMAN", "BRADFORD", "CHARLES", "GILMORE", "DOMINGUEZ", "ONEAL",
			"BRUCE", "LANG", "COMBS", "KRAMER", "HEATH", "HANCOCK", "GALLAGHER", "GAINES", "SHAFFER", "SHORT", "WIGGINS", "MATHEWS", "MCCLAIN", "FISCHER", "WALL", "SMALL", "MELTON", "HENSLEY", "BOND", "DYER",
			"CAMERON", "GRIMES", "CONTRERAS", "CHRISTIAN", "WYATT", "BAXTER", "SNOW", "MOSLEY", "SHEPHERD", "LARSEN", "HOOVER", "BEASLEY", "GLENN", "PETERSEN", "WHITEHEAD", "MEYERS", "KEITH", "GARRISON", "VINCENT", "SHIELDS",
			"HORN", "SAVAGE", "OLSEN", "SCHROEDER", "HARTMAN", "WOODARD", "MUELLER", "KEMP", "DELEON", "BOOTH", "PATEL", "CALHOUN", "WILEY", "EATON", "CLINE", "NAVARRO", "HARRELL", "LESTER", "HUMPHREY", "PARRISH",
			"DURAN", "HUTCHINSON", "HESS", "DORSEY", "BULLOCK", "ROBLES", "BEARD", "DALTON", "AVILA", "VANCE", "RICH", "BLACKWELL", "YORK", "JOHNS", "BLANKENSHIP", "TREVINO", "SALINAS", "CAMPOS", "PRUITT", "MOSES",
			"CALLAHAN", "GOLDEN", "MONTOYA", "HARDIN", "GUERRA", "MCDOWELL", "CAREY", "STAFFORD", "GALLEGOS", "HENSON", "WILKINSON", "BOOKER", "MERRITT", "MIRANDA", "ATKINSON", "ORR", "DECKER", "HOBBS", "PRESTON", "TANNER",
			"KNOX", "PACHECO", "STEPHENSON", "GLASS", "ROJAS", "SERRANO", "MARKS", "HICKMAN", "ENGLISH", "SWEENEY", "STRONG", "PRINCE", "MCCLURE", "CONWAY", "WALTER", "ROTH", "MAYNARD", "FARRELL", "LOWERY", "HURST",
			"NIXON", "WEISS", "TRUJILLO", "ELLISON", "SLOAN", "JUAREZ", "WINTERS", "MCLEAN", "RANDOLPH", "LEON", "BOYER", "VILLARREAL", "MCCALL", "GENTRY", "CARRILLO", "KENT", "AYERS", "LARA", "SHANNON", "SEXTON",
			"PACE", "HULL", "LEBLANC", "BROWNING", "VELASQUEZ", "LEACH", "CHANG", "HOUSE", "SELLERS", "HERRING", "NOBLE", "FOLEY", "BARTLETT", "MERCADO", "LANDRY", "DURHAM", "WALLS", "BARR", "MCKEE", "BAUER",
			"RIVERS", "EVERETT", "BRADSHAW", "PUGH", "VELEZ", "RUSH", "ESTES", "DODSON", "MORSE", "SHEPPARD", "WEEKS", "CAMACHO", "BEAN", "BARRON", "LIVINGSTON", "MIDDLETON", "SPEARS", "BRANCH", "BLEVINS", "CHEN",
			"KERR", "MCCONNELL", "HATFIELD", "HARDING", "ASHLEY", "SOLIS", "HERMAN", "FROST", "GILES", "BLACKBURN", "WILLIAM", "PENNINGTON", "WOODWARD", "FINLEY", "MCINTOSH", "KOCH", "BEST", "SOLOMON", "MCCULLOUGH", "DUDLEY",
			"NOLAN", "BLANCHARD", "RIVAS", "BRENNAN", "MEJIA", "KANE", "BENTON", "JOYCE", "BUCKLEY", "HALEY", "VALENTINE", "MADDOX", "RUSSO", "MCKNIGHT", "BUCK", "MOON", "MCMILLAN", "CROSBY", "BERG", "DOTSON",
			"MAYS", "ROACH", "CHURCH", "CHAN", "RICHMOND", "MEADOWS", "FAULKNER", "ONEILL", "KNAPP", "KLINE", "BARRY", "OCHOA", "JACOBSON", "GAY", "AVERY", "HENDRICKS", "HORNE", "SHEPARD", "HEBERT", "CHERRY",
			"CARDENAS", "MCINTYRE", "WHITNEY", "WALLER", "HOLMAN", "DONALDSON", "CANTU", "TERRELL", "MORIN", "GILLESPIE", "FUENTES", "TILLMAN", "SANFORD", "BENTLEY", "PECK", "KEY", "SALAS", "ROLLINS", "GAMBLE", "DICKSON",
			"BATTLE", "SANTANA", "CABRERA", "CERVANTES", "HOWE", "HINTON", "HURLEY", "SPENCE", "ZAMORA", "YANG", "MCNEIL", "SUAREZ", "CASE", "PETTY", "GOULD", "MCFARLAND", "SAMPSON", "CARVER", "BRAY", "ROSARIO",
			"MACDONALD", "STOUT", "HESTER", "MELENDEZ", "DILLON", "FARLEY", "HOPPER", "GALLOWAY", "POTTS", "BERNARD", "JOYNER", "STEIN", "AGUIRRE", "OSBORN", "MERCER", "BENDER", "FRANCO", "ROWLAND", "SYKES", "BENJAMIN",
			"TRAVIS", "PICKETT", "CRANE", "SEARS", "MAYO", "DUNLAP", "HAYDEN", "WILDER", "MCKAY", "COFFEY", "MCCARTY", "EWING", "COOLEY", "VAUGHAN", "BONNER", "COTTON", "HOLDER", "STARK", "FERRELL", "CANTRELL",
			"FULTON", "LYNN", "LOTT", "CALDERON", "ROSA", "POLLARD", "HOOPER", "BURCH", "MULLEN", "FRY", "RIDDLE", "LEVY", "DAVID", "DUKE", "ODONNELL", "GUY", "MICHAEL", "BRITT", "FREDERICK", "DAUGHERTY",
			"BERGER", "DILLARD", "ALSTON", "JARVIS", "FRYE", "RIGGS", "CHANEY", "ODOM", "DUFFY", "FITZPATRICK", "VALENZUELA", "MERRILL", "MAYER", "ALFORD", "MCPHERSON", "ACEVEDO", "DONOVAN", "BARRERA", "ALBERT", "COTE",
			"REILLY", "COMPTON", "RAYMOND", "MOONEY", "MCGOWAN", "CRAFT", "CLEVELAND", "CLEMONS", "WYNN", "NIELSEN", "BAIRD", "STANTON", "SNIDER", "ROSALES", "BRIGHT", "WITT", "STUART", "HAYS", "HOLDEN", "RUTLEDGE",
			"KINNEY", "CLEMENTS", "CASTANEDA", "SLATER", "HAHN", "EMERSON", "CONRAD", "BURKS", "DELANEY", "PATE", "LANCASTER", "SWEET", "JUSTICE", "TYSON", "SHARPE", "WHITFIELD", "TALLEY", "MACIAS", "IRWIN", "BURRIS",
			"RATLIFF", "MCCRAY", "MADDEN", "KAUFMAN", "BEACH", "GOFF", "CASH", "BOLTON", "MCFADDEN", "LEVINE", "GOOD", "BYERS", "KIRKLAND", "KIDD", "WORKMAN", "CARNEY", "DALE", "MCLEOD", "HOLCOMB", "ENGLAND",
			"FINCH", "HEAD", "BURT", "HENDRIX", "SOSA", "HANEY", "FRANKS", "SARGENT", "NIEVES", "DOWNS", "RASMUSSEN", "BIRD", "HEWITT", "LINDSAY", "LE", "FOREMAN", "VALENCIA", "ONEIL", "DELACRUZ", "VINSON",
			"DEJESUS", "HYDE", "FORBES", "GILLIAM", "GUTHRIE", "WOOTEN", "HUBER", "BARLOW", "BOYLE", "MCMAHON", "BUCKNER", "ROCHA", "PUCKETT", "LANGLEY", "KNOWLES", "COOKE", "VELAZQUEZ", "WHITLEY", "NOEL", "VANG",
			"SHEA", "ROUSE", "HARTLEY", "MAYFIELD", "ELDER", "RANKIN", "HANNA", "COWAN", "LUCERO", "ARROYO", "SLAUGHTER", "HAAS", "OCONNELL", "MINOR", "KENDRICK", "SHIRLEY", "KENDALL", "BOUCHER", "ARCHER", "BOGGS",
			"ODELL", "DOUGHERTY", "ANDERSEN", "NEWELL", "CROWE", "WANG", "FRIEDMAN", "BLAND", "SWAIN", "HOLLEY", "FELIX", "PEARCE", "CHILDS", "YARBROUGH", "GALVAN", "PROCTOR", "MEEKS", "LOZANO", "MORA", "RANGEL",
			"BACON", "VILLANUEVA", "SCHAEFER", "ROSADO", "HELMS", "BOYCE", "GOSS", "STINSON", "SMART", "LAKE", "IBARRA", "HUTCHINS", "COVINGTON", "REYNA", "GREGG", "WERNER", "CROWLEY", "HATCHER", "MACKEY", "BUNCH",
			"WOMACK", "POLK", "JAMISON", "DODD", "CHILDRESS", "CHILDERS", "CAMP", "VILLA", "DYE", "SPRINGER", "MAHONEY", "DAILEY", "BELCHER", "LOCKHART", "GRIGGS", "COSTA", "CONNOR", "BRANDT", "WINTER", "WALDEN",
			"MOSER", "TRACY", "TATUM", "MCCANN", "AKERS", "LUTZ", "PRYOR", "LAW", "OROZCO", "MCALLISTER", "LUGO", "DAVIES", "SHOEMAKER", "MADISON", "RUTHERFORD", "NEWSOME", "MAGEE", "CHAMBERLAIN", "BLANTON", "SIMMS",
			"GODFREY", "FLANAGAN", "CRUM", "CORDOVA", "ESCOBAR", "DOWNING", "SINCLAIR", "DONAHUE", "KRUEGER", "MCGINNIS", "GORE", "FARRIS", "WEBBER", "CORBETT", "ANDRADE", "STARR", "LYON", "YODER", "HASTINGS", "MCGRATH",
			"SPIVEY", "KRAUSE", "HARDEN", "CRABTREE", "KIRKPATRICK", "HOLLIS", "BRANDON", "ARRINGTON", "ERVIN", "CLIFTON", "RITTER", "MCGHEE", "BOLDEN", "MALONEY", "GAGNON", "DUNBAR", "PONCE", "PIKE", "MAYES", "HEARD",
			"BEATTY", "MOBLEY", "KIMBALL", "BUTTS", "MONTES", "HERBERT", "GRADY", "ELDRIDGE", "BRAUN", "HAMM", "GIBBONS", "SEYMOUR", "MOYER", "MANLEY", "HERRON", "PLUMMER", "ELMORE", "CRAMER", "GARY", "RUCKER",
			"HILTON", "BLUE", "PIERSON", "FONTENOT", "FIELD", "RUBIO", "GRACE", "GOLDSTEIN", "ELKINS", "WILLS", "NOVAK", "JOHN", "HICKEY", "WORLEY", "GORMAN", "KATZ", "DICKINSON", "BROUSSARD", "FRITZ", "WOODRUFF",
			"CROW", "CHRISTOPHER", "BRITTON", "FORREST", "NANCE", "LEHMAN", "BINGHAM", "ZUNIGA", "WHALEY", "SHAFER", "COFFMAN", "STEWARD", "DELAROSA", "NIX", "NEELY", "NUMBERS", "MATA", "MANUEL", "DAVILA", "MCCABE",
			"KESSLER", "EMERY", "BOWLING", "HINKLE", "WELSH", "PAGAN", "GOLDBERG", "GOINS", "CROUCH", "CUEVAS", "QUINONES", "MCDERMOTT", "HENDRICKSON", "SAMUELS", "DENTON", "BERGERON", "LAM", "IVEY", "LOCKE", "HAINES",
			"THURMAN", "SNELL", "HOSKINS", "BYRNE", "MILTON", "WINSTON", "ARTHUR", "ARIAS", "STANFORD", "ROE", "CORBIN", "BELTRAN", "CHAPPELL", "HURT", "DOWNEY", "DOOLEY", "TUTTLE", "COUCH", "PAYTON", "MCELROY",
			"CROCKETT", "GROVES", "CLEMENT", "LESLIE", "CARTWRIGHT", "DICKEY", "MCGILL", "DUBOIS", "MUNIZ", "ERWIN", "SELF", "TOLBERT", "DEMPSEY", "CISNEROS", "SEWELL", "LATHAM", "GARLAND", "VIGIL", "TAPIA", "STERLING",
			"RAINEY", "NORWOOD", "LACY", "STROUD", "MEADE", "AMOS", "TIPTON", "LORD", "KUHN", "HILLIARD", "BONILLA", "TEAGUE", "COURTNEY", "GUNN", "HO", "GREENWOOD", "CORREA", "REECE", "WESTON", "POE",
			"TRENT", "PINEDA", "PHIPPS", "FREY", "KAISER", "AMES", "PAIGE", "GUNTER", "SCHMITT", "MILLIGAN", "ESPINOSA", "CARLTON", "BOWDEN", "VICKERS", "LOWRY", "PRITCHARD", "COSTELLO", "PIPER", "MCCLELLAN", "LOVELL",
			"DREW", "SHEEHAN", "QUICK", "HATCH", "DOBSON", "SINGH", "JEFFRIES", "HOLLINGSWORTH", "SORENSEN", "MEZA", "FINK", "DONNELLY", "BURRELL", "BRUNO", "TOMLINSON", "COLBERT", "BILLINGS", "RITCHIE", "HELTON", "SUTHERLAND",
			"PEOPLES", "MCQUEEN", "GASTON", "THOMASON", "MCKINLEY", "GIVENS", "CROCKER", "VOGEL", "ROBISON", "DUNHAM", "COKER", "SWARTZ", "KEYS", "LILLY", "LADNER", "HANNAH", "WILLARD", "RICHTER", "HARGROVE", "EDMONDS",
			"BRANTLEY", "ALBRIGHT", "MURDOCK", "BOSWELL", "MULLER", "QUINTERO", "PADGETT", "KENNEY", "DALY", "CONNOLLY", "PIERRE", "INMAN", "QUINTANA", "LUND", "BARNARD", "VILLEGAS", "SIMONS", "LAND", "HUGGINS", "TIDWELL",
			"SANDERSON", "BULLARD", "MCCLENDON", "DUARTE", "DRAPER", "MEREDITH", "MARRERO", "DWYER", "ABRAMS", "STOVER", "GOODE", "FRASER", "CREWS", "BERNAL", "SMILEY", "GODWIN", "FISH", "CONKLIN", "MCNEAL", "BACA",
			"ESPARZA", "CROWDER", "BOWER", "NICHOLAS", "CHUNG", "BREWSTER", "MCNEILL", "DICK", "RODRIGUES", "LEAL", "COATES", "RAINES", "MCCAIN", "MCCORD", "MINER", "HOLBROOK", "SWIFT", "DUKES", "CARLISLE", "ALDRIDGE",
			"ACKERMAN", "STARKS", "RICKS", "HOLLIDAY", "FERRIS", "HAIRSTON", "SHEFFIELD", "LANGE", "FOUNTAIN", "MARINO", "DOSS", "BETTS", "KAPLAN", "CARMICHAEL", "BLOOM", "RUFFIN", "PENN", "KERN", "BOWLES", "SIZEMORE",
			"LARKIN", "DUPREE", "JEWELL", "SILVER", "SEALS", "METCALF", "HUTCHISON", "HENLEY", "FARR", "CASTLE", "MCCAULEY", "HANKINS", "GUSTAFSON", "DEAL", "CURRAN", "ASH", "WADDELL", "RAMEY", "CATES", "POLLOCK",
			"MAJOR", "IRVIN", "CUMMINS", "MESSER", "HELLER", "DEWITT", "LIN", "FUNK", "CORNETT", "PALACIOS", "GALINDO", "CANO", "HATHAWAY", "SINGER", "PHAM", "ENRIQUEZ", "AARON", "SALGADO", "PELLETIER", "PAINTER",
			"WISEMAN", "BLOUNT", "HAND", "FELICIANO", "TEMPLE", "HOUSER", "DOHERTY", "MEAD", "MCGRAW", "TONEY", "SWAN", "MELVIN", "CAPPS", "BLANCO", "BLACKMON", "WESLEY", "THOMSON", "MCMANUS", "FAIR", "BURKETT",
			"POST", "GLEASON", "RUDOLPH", "OTT", "DICKENS", "CORMIER", "VOSS", "RUSHING", "ROSENBERG", "HURD", "DUMAS", "BENITEZ", "ARELLANO", "STORY", "MARIN", "CAUDILL", "BRAGG", "JARAMILLO", "HUERTA", "GIPSON",
			"COLVIN", "BIGGS", "VELA", "PLATT", "CASSIDY", "TOMPKINS", "MCCOLLUM", "KAY", "GABRIEL", "DOLAN", "DALEY", "CRUMP", "STREET", "SNEED", "KILGORE", "GROVE", "GRIMM", "DAVISON", "BRUNSON", "PRATER",
			"MARCUM", "DEVINE", "KYLE", "DODGE", "STRATTON", "ROSAS", "CHOI", "TRIPP", "LEDBETTER", "LAY", "HIGHTOWER", "HAYWOOD", "FELDMAN", "EPPS", "YEAGER", "POSEY", "SYLVESTER", "SCRUGGS", "COPE", "STUBBS",
			"RICHEY", "OVERTON", "TROTTER", "SPRAGUE", "CORDERO", "BUTCHER", "BURGER", "STILES", "BURGOS", "WOODSON", "HORNER", "BASSETT", "PURCELL", "HASKINS", "GEE", "AKINS", "ABRAHAM", "HOYT", "ZIEGLER", "SPAULDING",
			"HADLEY", "GRUBBS", "SUMNER", "MURILLO", "ZAVALA", "SHOOK", "LOCKWOOD", "JARRETT", "DRISCOLL", "DAHL", "THORPE", "SHERIDAN", "REDMOND", "PUTNAM", "MCWILLIAMS", "MCRAE", "CORNELL", "FELTON", "ROMANO", "JOINER",
			"SADLER", "HEDRICK", "HAGER", "HAGEN", "FITCH", "COULTER", "THACKER", "MANSFIELD", "LANGSTON", "GUIDRY", "FERREIRA", "CORLEY", "CONN", "ROSSI", "LACKEY", "CODY", "BAEZ", "SAENZ", "MCNAMARA", "DARNELL",
			"MICHEL", "MCMULLEN", "MCKENNA", "MCDONOUGH", "LINK", "ENGEL", "BROWNE", "ROPER", "PEACOCK", "EUBANKS", "DRUMMOND", "STRINGER", "PRITCHETT", "PARHAM", "MIMS", "LANDERS", "HAM", "GRAYSON", "STACY", "SCHAFER",
			"EGAN", "TIMMONS", "OHARA", "KEEN", "HAMLIN", "FINN", "CORTES", "MCNAIR", "LOUIS", "CLIFFORD", "NADEAU", "MOSELEY", "MICHAUD", "ROSEN", "OAKES", "KURTZ", "JEFFERS", "CALLOWAY", "BEAL", "BAUTISTA",
			"WINN", "SUGGS", "STERN", "STAPLETON", "LYLES", "LAIRD", "MONTANO", "DIAMOND", "DAWKINS", "ROLAND", "HAGAN", "GOLDMAN", "BRYSON", "BARAJAS", "LOVETT", "SEGURA", "METZ", "LOCKETT", "LANGFORD", "HINSON",
			"EASTMAN", "ROCK", "HOOKS", "WOODY", "SMALLWOOD", "SHAPIRO", "CROWELL", "WHALEN", "TRIPLETT", "HOOKER", "CHATMAN", "ALDRICH", "CAHILL", "YOUNGBLOOD", "YBARRA", "STALLINGS", "SHEETS", "SAMUEL", "REEDER", "PERSON",
			"PACK", "LACEY", "CONNELLY", "BATEMAN", "ABERNATHY", "WINKLER", "WILKES", "MASTERS", "HACKETT", "GRANGER", "GILLIS", "SCHMITZ", "SAPP", "NAPIER", "SOUZA", "LANIER", "GOMES", "WEIR", "OTERO", "LEDFORD",
			"BURROUGHS", "BABCOCK", "VENTURA", "SIEGEL", "DUGAN", "CLINTON", "CHRISTIE", "BLEDSOE", "ATWOOD", "WRAY", "VARNER", "SPANGLER", "OTTO", "ANAYA", "STALEY", "KRAFT", "FOURNIER", "EDDY", "BELANGER", "WOLFF",
			"THORNE", "BYNUM", "BURNETTE", "BOYKIN", "SWENSON", "PURVIS", "PINA", "KHAN", "DUVALL", "DARBY", "XIONG", "KAUFFMAN", "ALI", "YU", "HEALY", "ENGLE", "CORONA", "BENOIT", "VALLE", "STEINER",
			"SPICER", "SHAVER", "RANDLE", "LUNDY", "DOW", "CHIN", "CALVERT", "STATON", "NEFF", "KEARNEY", "DARDEN", "OAKLEY", "MEDEIROS", "MCCRACKEN", "CRENSHAW", "BLOCK", "BEAVER", "PERDUE", "DILL", "WHITTAKER",
			"TOBIN", "CORNELIUS", "WASHBURN", "HOGUE", "GOODRICH", "EASLEY", "BRAVO", "DENNISON", "VERA", "SHIPLEY", "KERNS", "JORGENSEN", "CRAIN", "ABEL", "VILLALOBOS", "MAURER", "LONGORIA", "KEENE", "COON", "SIERRA",
			"WITHERSPOON", "STAPLES", "PETTIT", "KINCAID", "EASON", "MADRID", "ECHOLS", "LUSK", "WU", "STAHL", "CURRIE", "THAYER", "SHULTZ", "SHERWOOD", "MCNALLY", "SEAY", "NORTH", "MAHER", "KENNY", "HOPE",
			"GAGNE", "BARROW", "NAVA", "MYLES", "MORELAND", "HONEYCUTT", "HEARN", "DIGGS", "CARON", "WHITTEN", "WESTBROOK", "STOVALL", "RAGLAND", "QUEEN", "MUNSON", "MEIER", "LOONEY", "KIMBLE", "JOLLY", "HOBSON",
			"LONDON", "GODDARD", "CULVER", "BURR", "PRESLEY", "NEGRON", "CONNELL", "TOVAR", "MARCUS", "HUDDLESTON", "HAMMER", "ASHBY", "SALTER", "ROOT", "PENDLETON", "OLEARY", "NICKERSON", "MYRICK", "JUDD", "JACOBSEN",
			"ELLIOT", "BAIN", "ADAIR", "STARNES", "SHELDON", "MATOS", "LIGHT", "BUSBY", "HERNDON", "HANLEY", "BELLAMY", "JACK", "DOTY", "BARTLEY", "YAZZIE", "ROWELL", "PARSON", "GIFFORD", "CULLEN", "CHRISTIANSEN",
			"BENAVIDES", "BARNHART", "TALBOT", "MOCK", "CRANDALL", "CONNORS", "BONDS", "WHITT", "GAGE", "BERGMAN", "ARREDONDO", "ADDISON", "MARION", "LUJAN", "DOWDY", "JERNIGAN", "HUYNH", "BOUCHARD", "DUTTON", "RHOADES",
			"OUELLETTE", "KISER", "RUBIN", "HERRINGTON", "HARE", "DENNY", "BLACKMAN", "BABB", "ALLRED", "RUDD", "PAULSON", "OGDEN", "KOENIG", "JACOB", "IRVING", "GEIGER", "BEGAY", "PARRA", "CHAMPION", "LASSITER",
			"HAWK", "ESPOSITO", "CHO", "WALDRON", "VERNON", "RANSOM", "PRATHER", "KEENAN", "JEAN", "GROVER", "CHACON", "VICK", "SANDS", "ROARK", "PARR", "MAYBERRY", "GREENBERG", "COLEY", "BRUNER", "WHITMAN",
			"SKAGGS", "SHIPMAN", "MEANS", "LEARY", "HUTTON", "ROMO", "MEDRANO", "LADD", "KRUSE", "FRIEND", "DARLING", "ASKEW", "VALENTIN", "SCHULZ", "ALFARO", "TABOR", "MOHR", "GALLO", "BERMUDEZ", "PEREIRA",
			"ISAAC", "BLISS", "REAVES", "FLINT", "COMER", "BOSTON", "WOODALL", "NAQUIN", "GUEVARA", "EARL", "DELONG", "CARRIER", "PICKENS", "BRAND", "TILLEY", "SCHAFFER", "READ", "LIM", "KNUTSON", "FENTON",
			"DORAN", "CHU", "VOGT", "VANN", "PRESCOTT", "MCLAIN", "LANDIS", "CORCORAN", "AMBROSE", "ZAPATA", "HYATT", "HEMPHILL", "FAULK", "CALL", "DOVE", "BOUDREAUX", "ARAGON", "WHITLOCK", "TREJO", "TACKETT",
			"SHEARER", "SALDANA", "HANKS", "GOLD", "DRIVER", "MCKINNON", "KOEHLER", "CHAMPAGNE", "BOURGEOIS", "POOL", "KEYES", "GOODSON", "FOOTE", "EARLY", "LUNSFORD", "GOLDSMITH", "FLOOD", "WINSLOW", "SAMS", "REAGAN"
		};
		surnames = new LinkedList(Arrays.asList(names));
		names = null;

		return surnames.size();
	}
	
	
	/**
	 * Method to format float and double vars for display on screen.
	 * @param	pattern	format pattern.
	 * @param	value	float/double value to format.
	 * @return	string containing formatted value.
	 */
	static public String customFormat(String pattern, double value )
	{
		  DecimalFormat myFormatter = new DecimalFormat(pattern);
		  String output = myFormatter.format(value);
		  return output;
	}

    /**
     * @return
     */
    public boolean[] getPImmuneArr()
    {
        return phenoImmuneSys;
    }
	/**
	 * @return
	 */
	public boolean[] getGImmuneArr()
	{
		return genoImmuneSys;
	}
	/**
	 * @return
	 */
	public String getPImmuneStr()
	{
		StringBuffer immuneStr = new StringBuffer("");
		short strLen = (short)this.phenoImmuneSys.length;
		for(short i = 0; i<strLen; i++)
			immuneStr.append(this.phenoImmuneSys[i] ? "1" : "0");
		
		return immuneStr.toString();
	}

	/**
	 * Periodic dispersal of accumulated pollution caused by gathering activity.
	 * 
	 */
	public static synchronized void generateDiseases()
	{	
		String newGerm;
		long randomNo;
		int dCount = GoLconst.CELLSPACE_DISEASE_COUNT;
		long minVal = (long)Math.pow(2,GoLconst.DISEASE_SIZE_MIN-1);
		long maxVal = (long)Math.pow(2,GoLconst.DISEASE_SIZE_MAX)-1;
		germList.removeAll(germList);
		
//		System.out.println("Disease val: Min->" + minVal + "\tMax->" + maxVal);
//		System.out.println("Disease val: Min->" + Long.toBinaryString(minVal) + 
//									"\tMax->" + Long.toBinaryString(maxVal) );
		
		for(int i=0; i<dCount; i++)
		{	
			randomNo = (long) (minVal + (Math.random()*(maxVal-minVal)) );
			newGerm = Long.toBinaryString(randomNo);
			if(germList.contains(newGerm))
				i--;	//duplicate germ detected, generate another
			else
				germList.add(newGerm);
		}
//		System.out.println("Germ List --> " + germList.toString());
		
	}

	/**
	 * Initializes a new citizens immune system 
	 *  - randomly, if no parents exist or
	 *  - inherited from parents
	 */
	public void setImmuneSys(Citizen father, Citizen mother)
	{	if(father == null)
		{	
			if(immuneTagLen != GoLconst.CITIZEN_IMMUNE_STRING)
			{	immuneTagLen = ( (GoLconst.CITIZEN_IMMUNE_STRING < 63)? 
										GoLconst.CITIZEN_IMMUNE_STRING : 63);
				immuneTagMax = (long)Math.pow(2, immuneTagLen); 
				immuneTagMin = (long)Math.pow(2, immuneTagLen-1); 
				immuneTagDiff = immuneTagMax - immuneTagMin;
			}

			long randomNo	= immuneTagMin + (long)(Math.random() * immuneTagDiff);
			String randomString = Long.toBinaryString(randomNo);

			for(short i=0; i<immuneTagLen; i++)
			{	if(randomString.charAt(i) == '1')
				{	this.phenoImmuneSys[i] = true;
					this.genoImmuneSys[i] = true;
				} 
				else 
				{	this.phenoImmuneSys[i] = false;
					this.genoImmuneSys[i] = false;
				} 
			}
		}
		else
		{	boolean fatherImmuneSys[] = father.getGImmuneArr();
			boolean motherImmuneSys[] = mother.getGImmuneArr();
			for(int i=0; i<immuneTagLen; i++)
			{	if( fatherImmuneSys[i] == motherImmuneSys[i] )
				{	this.phenoImmuneSys[i] = fatherImmuneSys[i];
					this.genoImmuneSys[i]  = fatherImmuneSys[i]; 
				} 
				else 
				{	this.phenoImmuneSys[i] = ((Math.random() >= .5) ? true : false);
					this.genoImmuneSys[i]  = this.phenoImmuneSys[i];
				} 
			}
		}
	}

	/**
	 * @param bs
	 */
	public void flipImmuneTag(int index)
	{
		if(index < this.phenoImmuneSys.length)
			this.phenoImmuneSys[index] = !this.phenoImmuneSys[index];
	}


	/**
	 * Randomly assigns some diseases to citizen
	 */
	public void setDiseases()
	{	tmpList.addAll(germList);
		int dCount = tmpList.size(), randomNum;
		String disease, immuneStr = "";
		int index = this.phenoImmuneSys.length;

		for(short i = 0; i<index; i++)
			immuneStr = immuneStr + (this.phenoImmuneSys[i] ? "1" : "0");

//System.out.println(this.getFamily() + "-> " + immuneStr);
		for(short i=0; i<GoLconst.DISEASE_COUNT_INITIAL; i++)
		{
			//select random disease
			randomNum = (short)(Math.random()*dCount);
			disease = (String)tmpList.get(randomNum);
			tmpList.remove(randomNum);
			dCount--;
//System.out.println(randomNum + ") " + disease);		
			//if citizen already immune, loop back
			index = immuneStr.indexOf(disease);
			if(index >= 0)
				continue;	//citizen already immune to this disease
				
			//else add disease to citizen's disease list
			if(diseaseList.contains(disease))
				System.out.println("***Duplicate disease!!*********************");
			diseaseList.add(disease);
		}
//System.out.println(this.getFamily()+ "->" + this.diseaseList.toString());
		tmpList.removeAll(tmpList);
		sortDiseases();	//sort diseases by binary value
	}

	/**
	 * Adds disease to neighbor's disease list.
	 * The disease is inserted after existing diseases that are of a smaller length
	 * than the new germ. This maintains the order of the disease list which has been 
	 * sorted by size.
	 * In case the first disease in the list is the same length or smaller, then the 
	 * new addition is added after the first to avoid interrupting the development of 
	 * immunity against the first germ.
	 */
	public int setDisease(String disease)
	{	
		if(diseaseList.contains(disease))
			return 1;								//already infected with this disease
		
		String immuneStr = this.getPImmuneStr();
		if( immuneStr.indexOf(disease) >= 0 )
			return 2;								//already immune

		int dCount = getDiseaseCount();
		int index = 1;	//skips first disease in list
		while(index < dCount && ((String)diseaseList.get(index)).length() > disease.length())
			index++;
			
		if(dCount > 0)
			diseaseList.add(index, disease);	//insert disease into list
		else
			diseaseList.add(disease);			//add disease to empty list
			
		return 0;
	}


    /**
     * @return
     */
    public int getDiseaseCount()
    {
        if(diseaseList == null)
        	return 0;
        else
        	return this.diseaseList.size();
    }

	/**
	 * Sort diseases by string length to ensure simpler diseases are treated earlier.
	 * This ensures that less complex germs do not exact the same toll as the more complex
	 * ones.
	 */
	private void sortDiseases()
	{
		int dCount = diseaseList.size();
		int loopCntr = dCount - 1;
		String tmpStr;
		String [] disease = new String[dCount];
		for(int i=0; i<dCount; i++)
			disease[i] = (String)diseaseList.get(i);
		boolean sorted = false;
		while( !sorted )
		{	sorted = true;
			for(int i=0; i<loopCntr; i++)
			{	
				if(disease[i].length() > disease[i+1].length() )
				{
					tmpStr = new String(disease[i]);
					disease[i] = new String(disease[i+1]);
					disease[i+1] = new String(tmpStr);
					sorted = false;
				}
			}
		}
//		System.out.print(this.getFamily() + ((dCount>0) ? "\n" : " has no diseases\n") );
//		for(int i=0; i<dCount; i++)
//			System.out.print( i + ") " + disease[i] + "\t");
//		System.out.println("");
	}
    /**
     * @return
     */
    public String getDisease(int index)
    {
        return (String)diseaseList.get(index);
    }


	/**
	 * @return
	 */
	public boolean removeDisease(int index)
	{
		if( this.diseaseList.remove(index) != null)
			return true;
		else
			return false;
	}
	
	/**
	 * @return
	 */
	public int getBestHammingMatch(String disease)
	{
		String immuneStr = this.getPImmuneStr();
		int dLen = disease.length(),
			iLen = immuneStr.length(),
			highValue = 0,
			highIndex = 0;
		int i, j;
			
		int index = immuneStr.indexOf(disease);
		if( index >= 0 )
			return index;	//exact match of disease exists in immune sys
			
		//the extra row & col are convenient to store totals denoting hamming values
		int [][] hamArr = new int[iLen+1][dLen+1];
		for(i=0; i<iLen; i++)
			for(j=0; j<dLen; j++)
				if(immuneStr.charAt(i) == disease.charAt(j) )
					hamArr[i][j] = 1;
				else
					hamArr[i][j] = 0;
			
		//Hamming totals not calculated for cols < disease length
		j = dLen;
		iLen++;	//iLen incremented to enable calculation of H'distance for all substrings
		for(i=dLen; i<iLen; i++)
		{	for(int k=1; k<=dLen; k++)
				hamArr[i][j] += hamArr[i-k][j-k]; 
			if(hamArr[i][j] > highValue)
			{	highValue = hamArr[i][j];
				highIndex = i;
			}
		}
//System.out.print("H'dist = " + (dLen-highValue) + "\t");		
		//Best hamming match for disease in immune sys ends before highIndex
		return highIndex - dLen;	
	}
	
	/**
	 * @return
	 */
	public void generateImmunity(String disease, int index)
	{
		String immuneStr = this.getPImmuneStr();
		int endTag = index + disease.length();
		for(int i=index; i<endTag; i++)
			if( immuneStr.charAt(i) != disease.charAt(i-index) )
			{	flipImmuneTag(i);
				break;
			}
	}

}





