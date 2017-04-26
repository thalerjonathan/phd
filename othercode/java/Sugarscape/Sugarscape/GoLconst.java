package Sugarscape;

import java.text.*;

/**
 * This file provides options to configure and manipulate 
 * the behavior and properties of objects in the Sugarscape simulation.<BR>
 * It allows a researcher to create an initial scenario and to tweak rules
 * within that scenario. Collaborating researchers could set, evaluate,
 * finalize these settings in order to enable others to duplicate, verify 
 * and critique conclusions drawn from these simulations.
 * <BR>
 * The configuration is achieved through the use of global constants available
 * to all objects on the Sugarscape. These constants can be broadly classified 
 * into the following categories
 * <UL>
 * <LI> <B>Project Information</B>	refers to variables storing version control info.
 * <LI> Dimensions and attributes	refer to variables influencing 
 * physical dimensions of the grid, individual cells and other components.
 * <LI> <B>Citizen and Cell property boundaries and ratios</B>	refer to range limits
 * set on values that are randomly determined. Thus while a citizens vision is
 * randomly determined, the random value is restricted to a range that can be 
 * configured by the user.
 * <LI> <B>Process control switches and attributes</B>	refers to the ability to 
 * activate and deactivate processes by means of a boolean property. This group
 * also includes property value that are applied once their relative processes 
 * are activated.
 * <LI> <B>Debugging Flags</B> refer to additional display controls which enable 
 * verification of proper processing during simulation runs. These can be 
 * useful to two groups of individuals, the programmer examining code 
 * and researchers evaluating the output of the simulation. In both cases,
 * raw data are caused to be displayed by activating the debugging flags. This
 * data can then reveal reasons for apparently surprising behavior by the 
 * simulation. While it is possible that such surprises may be attributable to 
 * programming/algorithmic errors, they may also uncover subtle behavior in the 
 * scenario that had not been forseen by the researcher (eureka :).  
 * <LI> <B>Public program variables</B>	refer to programming variables that need
 * to be accessed <B>and modified</B> by various objects. <U>The easy access to
 * this information enables some methods to remain local to the Cell 
 * and Citizen objects, which otherwise might have been forced into the 
 * Sugarscape and CellSpace classes.</U>
 * </UL>
 *  
 * @author abraham kannankeril
 *
 */
public class GoLconst
{
	/** Current Sugarscape Project Version # */
	public static String PROGRAM_VERSION	= "2.44.1 LL";
	/**
	 * Public release date
	 */
	public static String PROGRAM_RELEASE_DATE = "24 Aug 2005";
	
	/**
	 * Fixed space added to main window area to accomodate mainmenu & toolbar
	 */
	public final static short CSPACE_OFFSET = 120;

	/***
	 * <B>Dimensions and Attributes</B>	 */
	/** Maximum possible value for Sugarscape columns */
	public static int GRID_COLS_MAX			= 150;
	public static int GRID_ROWS_MAX			= 150;
	/** Default value for Sugarscape columns */
	public static int GRID_COLUMNS			= 40;
	public static int GRID_ROWS				= 20;
	/** Cell side in pixels, each cell is a square*/
	public static int GRID_CELLSIZE			= 15;
	/** Time-interval between timePeriod, value SLOW */
	public static int GRID_REFRESH_SLOW		= 1500;
	public static int GRID_REFRESH_FAST		= 400;
	public static int GRID_REFRESH_HYPER	= 10;
	/** Cell Background Color, 200->LightGray */
	//public static int 		CELL_BG			= 200;
	public static int 		CELL_BG_SUMMER	= 155;
	public static int 		CELL_BG_WINTER	= 200;
	/** Cell Bar color - bar represents sugar & spice quantity in cell */
	public static int 		CELL_BAR		= 100;
	/** Controls the appearence of individual cells */
	public static boolean 	CELL_3D			= true;
	/** Default size in rows for text display & command feedback  */
	public static int TEXTAREA_HEIGHT		= 15;
	public static int TEXTAREA_MAX			= 25;
	public static int TEXTAREA_MIN			= 1;
	
	//Variables determining properties for Grid coordinates
	/**
	 * switches grid coordinates on / off.
	 * 
	 */
	public static boolean GRID_COORD_SHOW 	= true;
	/** 0->all cells, 1->last row&col, 2->last col, 3->last row, 4->last cell */	
	public static int   GRID_COORD_DETAIL 	= 1;
	/**
	 * determines font size, range .5f(small)--> .9f(large), default .8f 
	 */
	public static float GRID_COORD_FONT	 	= .8f ;
	/** available options - blue, black, red, green */	
	public static String GRID_COORD_COLOR = "blue";



	/***Output Destination Selection***/
	/** cannot implement due to java restrictions on applets */
	public static String OUTPUT_DESTINATION	= "FILE";

	/***
	 * <B>Citizen and Cell property boundaries and ratios</B>
	 */
	/** density of population used in the Random & Sugarscape template */
	public static float DENSITY_FACTOR 		= 0.3f;
	
	/**
	 * maximum vision level for a citizen , minimum 1
	 */
	public static int VISION_MAX 			= 4;
	
	/**
	 * maximum sugar metabolism level for a citizen, minimum 1.
	 */
	public static int METABOLISM_MAX_SUGAR	= 2;
	public static int METABOLISM_MAX_SPICE	= 2;
	
	/**
	 * min "initial" qty of sugar inherited by a citizen - inheritance for first generation is derived
	 * randomly, later generations inherit from parents. 
	 * Each parent contributes either 1/2 of their own initial inheritance or 1/2 of their total wealth at conception.
	 * This is determined by the value of the INHERIT_INITIAL_WEALTH variable.
	 * 
	 * @see	GoLconst#INHERIT_INITIAL_WEALTH	INHERIT_INITIAL_WEALTH
	 */
	public static int SUGAR_MIN_CITIZEN		= 100;
	/**
	 * max "initial" qty of sugar inherited by a citizen at birth - may gather more later 
	 */
	public static int SUGAR_MAX_CITIZEN		= 150;
	public static int SPICE_MIN_CITIZEN		= 100;
	public static int SPICE_MAX_CITIZEN		= 150;
	
	/**
	 * if true, accumulation is limited by capacity defined in SUGAR/SPICE_MAX_CELL.
	 */
	public static boolean LIMIT_CELL_SUGAR = true;
	public static boolean LIMIT_CELL_SPICE = true;
	/**
	 * maximum "initial" qty of sugar in a cell, may exceed this limit later if 
	 * LIMIT_CELL_SUGAR is false.
	 * 
	 * @see	GoLconst#LIMIT_CELL_SUGAR	LIMIT_CELL_SUGAR
	 */
	public static int SUGAR_MAX_CELL		= 10;
	public static int SPICE_MAX_CELL		= 10;
	
	//Grid fertility - how generous a distribution do you want to start with
	/**
	 * Grid fertility for sugar - percentage of cells that will grow sugar. This defines the 
	 * ratio of fertile (sugar-growing) to barren cells.
	 */
	public static float GRID_FERTILITY_SUGAR= .8f; //Any value between 0.0 & 1.0
	public static float GRID_FERTILITY_SPICE= .8f; //Any value between 0.0 & 1.0
	
	/**
	 * Life Expectancy range - minimum value, people may die earlier from other causes.
	 */
	public static int LIFE_EXPECTANCY_MIN	= 60;
	public static int LIFE_EXPECTANCY_MAX	= 80;

	/** Option to specify what attribute of citizen determines their color
	 * 	1 -> food reserves
	 *  2 -> cultural group membership
	 *  ....more can be added as required
	 */
	public static int CITIZEN_COLOR			= 2;
	/** Determines RGB color value depicting senior citizens - pre/post child-bearing age */
	public static int CITIZEN_COLOR_SENIOR	= 20;	//value applied to RGB parameters
	public static int CITIZEN_COLOR_CHILD	= 250;
	/**
	 * Wealth Level indicator for sugar (influences ranking), multiplier based on Metabolism.
	 * For eg. a value of 20 means citizens with less than (20*sugar metabolism) are 
	 * considered poor in terms of their sugar wealth.
	 * 
	 */
	public static int SUGAR_LEVEL_POOR		= 20;
	public static int SPICE_LEVEL_POOR		= 20;
	//bourgeosie is everything in between
	//public static int WEALTH_LEVEL_RICH	= 100;
	
	//Sex & Sexual Reproduction Control Variables
	/** ratio of females to males born */
	public static float SEX_RATIO			= .5f;
	/**
	 * ratio of citizens that are risk-averse to risk-takers, influences trading strategy. 
	 * 
	 * @see	Citizen#persona	persona
	 */	
	public static float 	PERSONALITY_RATIO	= .5f;
	
	/**
	 * minimum mating age for female citizens.
	 */
	public static int 		MATING_FEMALE_MIN	= 15;
	/**
	 * maximum mating age for female citizens, i.e., menopause
	 */
	public static int 		MATING_FEMALE_MAX	= 55;
	/**
	 * waiting period that must elapse after mating before female citizens can resume mating.
	 * Can be set to 0, if no gap is desired.
	 */
	public static int 		MATING_FEMALE_GAP	= 1;
	
	public static int 		MATING_MALE_MIN		= 12;
	public static int 		MATING_MALE_MAX		= 60;
	public static int 		MATING_MALE_GAP		= 1;
	
	/**
	 * Determines if mating is allowed among immediate kin (parent, child, sibling).
	 */
	public static boolean	MATING_KIN_ALLOW	= false;

	/*** <B>Process control switches and attributes</B>  */
	/**
	 * Determines whether children inherit 1/2 of parents inheritance or 1/2 of parents entire wealth.
	 */
	public static boolean INHERIT_INITIAL_WEALTH = true;
	
	/**
	 * Surnames are randomly generated from a list of 2000 names. This option determines whether 
	 * the names are reused or assigned exclusively.
	 * This applies only to the first generation citizens, subsequent generations inherit surnames from 
	 * one of their parents. This is determined by the INHERIT_FAMILY_FATHER variable.
	 * If the initial population exceeds the available surnames, then uniqueness is not an option. A 
	 * longer list can be used, if needed.
	 * 
	 * @see	GoLconst#INHERIT_FAMILY_FATHER	INHERIT_FAMILY_FATHER
	 */
	public static boolean CREATE_UNIQUE_SURNAMES = true;
	/**
	 * Determines whose name is passed on to the progeny. If true, the father's name else 
	 * the mother's name is is given to the child.
	 */
	public static boolean INHERIT_FAMILY_FATHER	 = true;
	
	/** Duration in time periods of each season */
	public static int SEASON_DURATION			= 100;
	/** Unit increase in amt of sugar during summer */
	public static float SUGAR_RENEW_SUMMER		= .3f;	//winter, usually
	/**
	 * Unit increase in amt of sugar during winter. Typically much lower than the unit increase in 
	 * summer.
	 */
	public static float SUGAR_RENEW_WINTER		= 0.01f;//slower renewal
	public static float SPICE_RENEW_SUMMER		= .3f;
	public static float SPICE_RENEW_WINTER		= 0.01f;
	
	//Pollution Initiation & Control Variables
	/** Pollution generated in a cell per unit sugar gathered */
	public static float POLLUTION_PRODUCTION_SUGAR	= .01f;
	public static float POLLUTION_PRODUCTION_SPICE	= .01f;
	/** Dispersal / Pollution reduction per cell per year */
	public static float POLLUTION_DISPERSION_UNIT	= .005f;
	
	/* CULTURE variables */
	public static short CITIZEN_CULTURE_TAG = 17;
	public static int CITIZEN_CULTURE_DIVIDE	= 3;
	public static short CITIZEN_CULTURE_TRANSFER = 1;
	/** this value determines the minimum number of neighbors needed to effect
	 * a change in a citizen's sticky tags
	 */
	public static short CITIZEN_CULTURE_OVERWHELM = 4;
	/** Cultural Grouping data */
	public static short CITIZEN_GROUP_INTERVAL[] = {5,7,9,11,15,17,17,17,17,17};
	public static String CITIZEN_GROUP_NAME[] = {"Blue",   "Red",      "Green",		"Yellow",  "Brown",    "Pink", 		 "Gray",	  "Cyan",	  "Black"}; 
	public static short CITIZEN_GROUP_COLOR[] = {0,0,255 , 255,20,20 , 90,200,90 , 255,255,0 , 165,42,42 , 255,175,175 , 128,128,128, 200,100,200, 0,0,0};
	/** maximum number of groups to display on Citizen panel */
	public static short MAX_GROUPS = 5;
	
	/** Disease variables */
	//Cellspace disease list vars
	public static short CELLSPACE_DISEASE_COUNT = 12;
	public static float DISEASE_COST_SUGAR = .1f;
	public static float DISEASE_COST_SPICE = .1f;
	public static float DISEASE_COST_TRADE = .1f;

	//Citizen disease vars
	public static short CITIZEN_IMMUNE_STRING = 50;	//length of each citizen's immune string
	public static short DISEASE_SIZE_MAX = 10; //max length of disease string 
	public static short DISEASE_SIZE_MIN = 5;
	// default disease count for each citizen
	public static short DISEASE_COUNT_INITIAL = 5;

	/**
	 * Activate Gathering processes. Seek best source of food or farthest cell, go there & harvest.
	 */
	public static boolean INITIATE_GATHER			= true;
	
	/**
	 * Activate Mating processes. Verify mating criteria, select mate & bear offspring
	 */
	public static boolean INITIATE_MATING			= true;
	
	/** Climate Change Initiation, if false, its always summer */
	public static boolean INITIATE_SEASONS			= true;
	
	/**
	 * Switch for the pollution process. As cells accumulate pollutant, they become less desirable to 
	 * citizens. Pollution is caused by the food production / harvesting process. The pollution dispersal 
	 * processes are also activated with pollution. The amount of pollution caused & dispersed is
	 * determined by the following variables.
	 * 
	 * @see	GoLconst#POLLUTION_PRODUCTION_SUGAR	POLLUTION_PRODUCTION_SUGAR
	 * @see	GoLconst#POLLUTION_PRODUCTION_SPICE	POLLUTION_PRODUCTION_SPICE
	 * @see	GoLconst#POLLUTION_DISPERSION_UNIT	POLLUTION_DISPERSION_UNIT
	 */
	public static boolean INITIATE_POLLUTION		= true;
	
	/**
	 * Inheritance switch. Wealth of dead citizens is divided among surviving children.
	 */
	public static boolean INITIATE_INHERITANCE		= true;
	
	/**
	 * Barter Initiation switch. This enables or disables trade on the Sugarscape.
	 */
	public static boolean INITIATE_BARTER			= true;
	
	/**
	 * Activate Culture propogation. 
	 */
	public static boolean INITIATE_CULTURE			= true;

	/**
	 * Activate Disease, Infection & Immunity processes.
	 */
	public static boolean INITIATE_DISEASE			= true;


	/**
	 * Debugging Flags
	 * The DEBUG flag switches on all debugging, leave it Off if you want debug info
	 * for a specific section of code. Use one of the other more specific debug
	 * switches to debug a particular section of code.
	 * Enabling the DEBUG switch will render some of the other debugging switches
	 * ineffective. The switches listed above the DEBUG switch are turned On but
	 * not Off with the DEBUG switch.
	 * 
	 */
	/**
	 * Controls display of critical system errors that invalidate results. Default true.
	 */
	public static boolean DEBUG_CRITICAL_ERROR		= true;
	
	/**
	 * This determines whether the feedback to commands typed in by the user are 
	 * generated and displayed. Default true.
	 */
	public static boolean DEBUG_CMD_FEEDBACK		= true;
	
	/**
	 * This activates all debugging output, irrespective of the value of the rest of the DEBUG 
	 * variables. It is similar to a master switch.
	 * Avoid this option unless you absolutely need it as it will dramatically slow down the 
	 * execution of the simulation. It may also cause a memory protection error if too much 
	 * data is generated.
	 */
	public static boolean DEBUG						= false;

	/**
	 * Controls display of program flow by listing the names of major methods as they 
	 * execute. If other debugging options are activated, the added feedback may make 
	 * it harder to understand the program flow.
	 */
	public static boolean DEBUG_PROGRAM_FLOW		= false;
	
	/**
	 * Enables output of subprocesses that comprise birth of a child.
	 */
	public static boolean DEBUG_CITIZEN_BIRTH 		= false;
	public static boolean DEBUG_CITIZEN_DEATH 		= false;
	
	/**
	 * Enables output of subprocesses that control the search for sugar.
	 */
	public static boolean DEBUG_SEARCH_SUGAR 		= false;
	/**
	 * Displays the actual distribution of sugar at the start of each cycle.
	 */
	public static boolean DEBUG_SUGAR_PRODUCTION 	= false;
	/**
	 * Displays the actual distribution of spice at the start of each cycle.
	 */
	public static boolean DEBUG_SPICE_PRODUCTION 	= false;
	/**
	 * Enables output of subprocesses that comprise the mating function.
	 */
	public static boolean DEBUG_MATING 				= false;
	/**
	 * Enables output of processes that comprise the search for a mate.
	 */
	public static boolean DEBUG_MATING_SEARCH 		= false;
	/**
	 * Enables output of processes that comprise the selection of a mate.
	 */
	public static boolean DEBUG_MATING_SELECTION	= false;
	/**
	 * Enables output of processes that comprise the birth of a child after mating.
	 */
	public static boolean DEBUG_MATING_BIRTH 		= false;
	
	/**
	 * Enables tracking of pollution accumulation and dispersal processes.
	 */
	public static boolean DEBUG_POLLUTION	 		= false;
	
	/**
	 * Enables tracking of time taken by the major processes.
	 */
	public static boolean DEBUG_PROCESS_TIME	 	= false;
	
	/**
	 * Enables tracking of the inheritance process.
	 */
	public static boolean DEBUG_INHERITANCE		 	= false;
	
	/**
	 * Enables output of subprocesses that comprise the barter function.
	 */
	public static boolean DEBUG_BARTER		 		= false;
	/**
	 * Enables output of process that evaluates, ranks and selects from multiple trade 
	 * offers.
	 */
	public static boolean DEBUG_BARTER_SORT	 		= false;
	/**
	 * Enables output from the process finalizing the exchange of goods between citizens.
	 * 
	 */
	public static boolean DEBUG_BARTER_EXCHANGE		= false;
	
	public static boolean DEBUG_CULTURE				= false;
	//public static boolean DEBUG_CULTURE_ASSIGN		= false;
	public static boolean DEBUG_CULTURE_TRANSMIT	= false;
	public static boolean DEBUG_CULTURE_STICKY		= false;
	public static boolean DEBUG_CULTURE_VOLATILE	= false;
	//public static boolean DEBUG_CULTURE_GROUPING	= false;
	
	public static boolean DEBUG_DISEASE				= false;
	public static boolean DEBUG_DISEASE_COST		= false;
	public static boolean DEBUG_DISEASE_TREAT		= false;
	public static boolean DEBUG_DISEASE_TRANSMIT	= false;


	/*
	 * <B>Public program variables</B>
	 */
	/**
	 * flag identifying (de)activation of the Sugarscape process from either the menu or
	 * the command line on the Sugarscape.
	 * 
	 */
	public static boolean flagSugarscape;
	/**
	 * flag identifying (de)activation of the Demo/test process that enables a quick test or 
	 * demonstration of the Sugarscape simulation.
	 * 
	 */
	public static boolean flagDemo = false;
	/**
	 * not used currently due to security restrictions on applets in Java.
	 * 
	 */
	public static boolean flagOutFileCreated = false;
	public static int demoCntr = 0;
	public static int timePeriod = 0;

	/**
	 * Default set of checks attempts to determine if default settings for variables are valid.
	 * 
	 * @return	String listing all errors discovered and corrective action taken
	 */
	public static String chkGoLconstants()
	{	//Boolean Variables don't require checking as non-boolean values
		//Would Cause a Compilation error
		String dataStream = "\n";

		if( GRID_CELLSIZE < 5 || GRID_CELLSIZE > 25)
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream +=
				"GRID_CELLSIZE:Invalid value, assuming 10!!\n";	}
			GRID_CELLSIZE = 16;
		}
		//Defines if & what form grid coordinates are to be displayed
		if( GRID_COORD_DETAIL < 0 || GRID_COORD_DETAIL > 5)
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream +=
				"GRID_COORD_DETAIL:Invalid value, assuming 1!!\n";	}
			GRID_COORD_DETAIL = 1;
		}
		if( GRID_COORD_FONT < .5f || GRID_COORD_FONT > .9f)
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream +=
				"GRID_COORD_FONT:Invalid value, assuming .8!!\n";	}
			GRID_COORD_FONT = .8f;
		}
		if(   !(GRID_COORD_COLOR == "blue" 	|| GRID_COORD_COLOR == "black" 	||
				GRID_COORD_COLOR == "red" 	|| GRID_COORD_COLOR == "green") 	)
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream += "GRID_COORD_COLOR:Invalid value, assuming blue!!\n";	}
			GRID_COORD_COLOR = "blue";
		}

		if( DENSITY_FACTOR > 1 || DENSITY_FACTOR < 0)
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream += "DENSITY_FACTOR:Invalid value, assuming 0.5!!\n";	}
			DENSITY_FACTOR = 0.5f;
		}

		if( VISION_MAX <= 0 )
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream += "VISION_MAX:Invalid value, assuming 4!!\n";	}
			VISION_MAX = 4;
		}
		if( METABOLISM_MAX_SUGAR < 0 )
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream += "METABOLISM_MAX_SUGAR:Invalid value, assuming 4!!\n";	}
			METABOLISM_MAX_SUGAR = 4;
		}
		if( SUGAR_MAX_CITIZEN < 0)
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream += "SUGAR_MAX_CITIZEN:Invalid value, assuming 10!!\n";	}
			SUGAR_MAX_CITIZEN = 10;
		}
		if( METABOLISM_MAX_SPICE < 0 )
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream += "METABOLISM_MAX_SPICE:Invalid value, assuming 4!!\n";	}
			METABOLISM_MAX_SPICE = 4;
		}
		if( SPICE_MAX_CITIZEN < 0)
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream += "SPICE_MAX_CITIZEN:Invalid value, assuming 10!!\n";	}
			SPICE_MAX_CITIZEN = 10;
		}

		if( SUGAR_MAX_CELL < 0)
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream += "SUGAR_MAX_CELL:Invalid value, assuming 10!!\n";	}
			SUGAR_MAX_CELL = 10;
		}
		if( SPICE_MAX_CELL < 0)
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream += "SPICE_MAX_CELL:Invalid value, assuming 10!!\n";	}
			SPICE_MAX_CELL = 10;
		}

		if( GRID_FERTILITY_SUGAR > 1 || GRID_FERTILITY_SUGAR < 0)
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream += "GRID_FERTILITY_SUGAR:Invalid value, assuming 0.5!!\n";	}
			GRID_FERTILITY_SUGAR = 0.5f;
		}
		if( GRID_FERTILITY_SPICE > 1 || GRID_FERTILITY_SPICE < 0)
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream += "GRID_FERTILITY_SPICE:Invalid value, assuming 0.5!!\n";	}
			GRID_FERTILITY_SPICE = 0.5f;
		}

		if( SUGAR_RENEW_SUMMER > SUGAR_MAX_CELL || SUGAR_RENEW_SUMMER < 0)
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream += "SUGAR_RENEW_SUMMER:Invalid value, assuming 0.1!!\n";	}
			SUGAR_RENEW_SUMMER = 0.1f;
		}
		if( SUGAR_RENEW_WINTER > SUGAR_MAX_CELL || SUGAR_RENEW_WINTER < 0)
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream += "SUGAR_RENEW_WINTER:Invalid value, assuming 0.1!!\n";	}
			SUGAR_RENEW_WINTER = 0.1f;
		}
		if( SPICE_RENEW_SUMMER > SPICE_MAX_CELL || SPICE_RENEW_SUMMER < 0)
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream += "SPICE_RENEW_SUMMER:Invalid value, assuming 0.1!!\n";	}
			SPICE_RENEW_SUMMER = 0.1f;
		}
		if( SPICE_RENEW_WINTER > SPICE_MAX_CELL || SPICE_RENEW_WINTER < 0)
		{	if( DEBUG_CRITICAL_ERROR || DEBUG )
			{	dataStream += "SPICE_RENEW_WINTER:Invalid value, assuming 0.1!!\n";	}
			SPICE_RENEW_WINTER = 0.1f;
		}
		return dataStream;
	}
	
	/**
	 * Method to format float and double vars for display on screen.
	 * 
	 * @param	pattern	format pattern.
	 * @param	value	float/double value to format.
	 * @return	string containing formatted value.
	 */
	public static String customFormat(String pattern, double value )
	{
		  DecimalFormat myFormatter = new DecimalFormat(pattern);
		  String output = myFormatter.format(value);
		  return output;
	}
    
	/**
	  * @return
	  */
	 public static short groupCount()
	 {
		for(short i=1; i<MAX_GROUPS; i++)
		{	if( CITIZEN_GROUP_INTERVAL[i-1] >= CITIZEN_GROUP_INTERVAL[i])
				return i; 
		} 
		 return MAX_GROUPS;
	 }

}