/*
 * This file (Sugarscape.java) contains the init(), start(), stop() and run() methods 
 * that initiate the simulation in applet form. Due to limitations of Swing the program
 * can only run within the AppletViewer. Hopefully that will change once the browsers 
 * extend support for the latest version of the JVM.
 * This file also generates & executes all GUI components via the actionControls() and
 * actionPerformed() methods. 
 */
import Sugarscape.*;

import java.lang.String;
import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.FlowLayout;
import java.awt.Container;
import java.awt.event.*;
import java.awt.Dimension;
import java.awt.Color;
import java.awt.Toolkit;
import java.util.StringTokenizer;
import javax.swing.*;
import javax.swing.JApplet;
import javax.swing.border.*;

/**
 * Built-on the original code from<BR>
 * 
 * --------------------------------<BR>
 * Game of Life v1.3<BR>
 * Copyright 1996-2001 Edwin Martin <edwin@bitstorm.nl><BR>
 * version 1.0 online since July 3 1996<BR>
 * Changes:<BR>
 * 1.1: Double buffering to screen; faster paint<BR>
 * 1.2: Arrowkeys changed; better use of `synchronized'<BR>
 * 1.3: Choose speed from drop down menu and draw with mouse<BR>
 * <BR>
 * -------------Sugarscape - Linked List version------------<BR>
 * -------------abraham kannankeril---------------------------<BR>
 * 1.4: Added template 'Random'& 'Big Bang' to layout choices<BR>
 * -------------------------xxx-------------------------------<BR>
 * 2.0: Add Sugarscape & Citizen objects incorporating a new<BR>
 * & more sophisticated ruleset.<BR>
 * -> a single resource (sugar) is randomly dispersed on the grid (sugarscape)<BR>
 * -> citizens scattered on the sugarscape must collect & consume sugar to ensure survival<BR>
 * -> citizens possess attributes - vision, metabolism & sugar hoard<BR>
 * System-level variables can be manually adjusted via public constants to influence<BR>
 * behavior on the sugarscape.<BR>
 * 2.1:	Split into multiple files for better manageablity - package GameOfLife<BR>
 * Added system variable DEBUG to switch debug info ON/OFF<BR>
 * 2.2:	Added visual representation of cell & citizen properties<BR>
 * -> cells shaded to represent available sugar level (low / medium / high)<BR>
 * -> citizen shaded to represent available sugar level (low / medium / high)<BR>
 * -> citizen shape conveys representation of following combined properties<BR>
 * - high vision, high metabolism<BR>
 * - high vision, low metabolism<BR>
 * - low vision, high metabolism<BR>
 * - low vision, low metabolism<BR>
 * 2.21	Improved sugar search routine to enable neutral selection between equally fertile cells<BR>
 * and selection of random cell if no sugar available to citizen<BR>
 * Added boolean variables to toggle debugging of selected program functions<BR>
 * Added boolean flag to allow imposition of limits on sugar accumulation in cells and a<BR>
 * corresponding method to depict absolute values of the sugar matrix<BR>
 * Color cues implemented in visual display:<BR>
 * -> Cell color darkens according to sugar accumulated in each cell<BR>
 * -> Citizen color gradually varies from red to orange to yellow to green to represent
 * the amount of sugar collected<BR>
 * -> Citizen shape varies to depict one of four relative vision/metabolism combinations
 * -> Grid coordinates can be printed if needed. This includes options to print all or
 * some coordinates & to select a print color<BR>
 * 2.22 All output from the program has now been redirected to a graphical textarea within the
 * same window as the grid. The earlier dump into STDOUT has been discontinued.<BR>
 * 2.30 Eliminated "Active Cell found unexpectedly" error<BR>
 * Eliminated grid sugar renewal problem - now regenerated with new sugarscape<BR>
 * Added debugging variable, DEBUG_PROGRAM_FLOW - enables tracing of method sequence<BR>
 * Added command line interface to enable interactive control of program parameters<BR>
 * 2.35 Added mate selection, child birth and death by aging to the Citizen object.<BR>
 * The Citizen object now recognizes familial relationships (sibling, parent, child).<BR>
 * Reworked the debug feedback elements. The program feedback has a much smaller
 * delay during execution.<BR>
 * Renamed main class/package to Sugarscape to reflect the true name of the project.<BR>
 * 2.36 Added climate change, pollution<BR>
 * grid fertility adjustable by season<BR>
 * 2.37 Unreleased version, implements the option<BR>
 * - to resize the grid at runtime.<BR>
 * - of timing the major events(gathering & mating).<BR>
 * - of executing a specified number timePeriods via the command-line.<BR>
 * - to inspect data values for any citizen on the grid and the entire population.<BR>
 * - to start/stop climate change & pollution.<BR>
 * -----------------------<BR>
 * 2.40 Major design changes resulting from creation of new Cell object. 
 * -> The Cell object is now a subclass of the CellSpace object and a superclass of 
 *    the Citizen object.<BR>
 * -> A seperate linked list of references to each citizen is maintained which simplifies
 *    operations involving all citizens, shuffling citizens randomly and locating occupants
 *    of a particular cell.<BR>
 * -> The major benefit of this redesign (suggested by Digvijay Parmar) is in performance.
 *    <BR>The resulting code is atleast five times faster than my earlier design. 
 * -> That the code is cleaner & more readable is a welcome by-product.<BR>
 * -> I would also like to express my gratitude to Ms. Richa Parmar, whose expert advice,
 *    eliminated seemingly enormous obstacles for this novice programmer.<BR>
 * BEWARE - the ability to click or drag cells to create/destroy citizens has not been 
 *    fully implemented, due to plain laziness :)<BR>
 * 2.41 Added command 'DEMO' / 'TEST' to help demonstrate common program features.<BR>
 * -> Add 2nd commodity, SPICE to the Sugarscape. Citizens need both<BR>
 * -> Changed graphics to represent 2nd commodity & color-coded citizens based on type<BR>
 * -> Added variables in GoLconst to enable manipulation of new/modified objects<BR>
 * 2.42 Add Barter process to the Sugarscape. 
 * -> Module incorporates internal evaluation, price negotiation and goods exchange. <BR>
 * -> Added 'persona' to citizen - determines Barter strategy. Either maximizing trades
 *    involving single units (risk-taker) or maximizing volume by selling multiple units
 *    in safe trades (risk-averse) strategies. Allows creation of additional strategies.<BR>
 * 2.43 Added a tabbed-pane interface to replace the simple TextArea object that was 
 *    previously used to display results.
 * -> Separate panes now display simulation progress data, allow configuration and debugging.
 * -> Forced to switch from AWT to Swing graphics libraries, applet therefore executes only 
 *    in AppletViewer, working to improve the situation.
 * -> Rectified error in barter processes that caused corruption of trade data.
 * -> Added time counter for the barter processes.
 * -> Moved GUI objects to seperate top-level mini-classes within Sugarscape.java to 
 *    overcome problems hindering program execution as an applet.
 * 2.44 Added culture and group formation to the Sugarscape
 * -> The Cellspace & Citizen panels contains the fields to control the cultural 
 * 		and disease processes. Feedback from these processes can be controlled from the 
 * 		Configure panel.
 * -> Cultural Transmission, Inheritance & Group Formation implemented. 
 * 		- child inherits beliefs of parents.
 * 		- culture transfer options - transmit, absorb or by ranking.
 * 		- added concept of sticky tags (persistent beliefs that rarely change)
 * 	-> Disease Generation, Infection, Transmission  & Immunization
 * 		- random generation of disease superset with specified min & max length
 * 		- inheritance of original, not evolving immune systems from parents
 * 		- random seeding of diseases into newborn citizens
 * 		- infection detection & immunization; simpler diseases cured earlier
 * 		- infection of neighbors if not already infected or immune
 * 2.44.1 Resumed Work in July 2005
 * -> Grid display now reflects change of seasons
 * -> Resolved bug caused when Mating process was switched off
 * 
 * 
 * -----------------------------------xxx----------------------------------------
 * This is the primary class in the Sugarscape project. 
 * Its sub-components include the CellSpace or grid object, the Cell and Citizen objects.<BR>
 * Class Sugarscape sets up the GUI interface, the thread the executes the simulation, 
 * event handlers that control various Game of Life and Sugarscape templates and execute 
 * run-time commands.
 * 
 * @author	abraham kannankeril<BR>
 * <I>parts of code in this class borrowed with permission from 
 * Edwin Martin's Game of Life</I>
 * @see	<A HREF="http://sugarscape.sourceforge.net/">Sugarscape Home</A>
 * @see	<A HREF="http://www.brook.edu/press/books/ARTIFSOC.HTM">The Book</A>
 * @see	<A HREF="http://backspaces.net/Models/sugarscape.html">NetLogo Implementation</A>
 * @see	<A HREF="http://www.sciencenews.org/sn_arch/11_23_96/bob1.htm">Article on Sugarscape</A>
 * @see	<A HREF="http://www.theatlantic.com/issues/2002/04/rauch.htm">Article on AI</A>
 * @see	<A HREF="http://www.uni-koblenz.de/~kgt/Learn/Textbook/node1.html">eBook - Simulation for the Social  Science</A>
 * 	
 */
public class Sugarscape extends JApplet implements Runnable, ActionListener
{
    
    /**
     * The only instance of a CellSpace object in the simulation.
     * 
     */
    private CellSpace cellSpace;
    private Thread gameThread = null;
    private int genTime;
    private int cellSize;
    private int cellCols;
    private int cellRows;

    //Initial templates for the Game of Life grid
    private final String clear = "Clear";
    private final String sugarscape = "Sugarscape";
    private final String random = "Random";
    private final String glider = "Glider";
    private final String bigbang = "Big Bang";
    private final String exploder1 = "Small Exploder";
    private final String exploder2 = "Exploder";
    private final String row10 = "10 Cell Row";
    private final String fish = "Fish";
    private final String pump = "Pump";
    private final String gun = "Shooter";

    /** Speed settings for the simulation */
	private final String slow = "Slow";
	private final String fast = "Fast";
    private final String hyper = "Hyper";

    /** Label for buttons */
    private final String nextLabel = "Next";
    private final String startLabel = "Start";
    private final String stopLabel = "Stop";

    //**Program data display & interaction objects
    public JButton startstopButton, nextButton;
    private JLabel genLabel;
    private JTextField textField;
    private JTextArea textArea;
	
	/** Template Choice and speed control */
    public JComboBox c, speed;
	public JTabbedPane tabPanel;
	public JPanel scorePanel, sScapePanel, cellPanel, citizenPanel, configPanel,
			 historyPanel, controls;
	public JTextField scDensity, scBirths, scPerCycle, scDeaths, scStarved, scOldAge,
		scAvgVision, scAvgSugar, scAvgSuMetab, scAvgSpice, scAvgSpMetab, scAvgLifespan,
		scAvgAge, scDistSummer, scDistWinter, scDistMale, scDistFemale, scDistChildren,
		scDistAdults, scDistSeniors, scRiskTaker, scRiskAverse, scHyp, scHypSu, 
		scHypSp, scEff, scInEff, scSlow, scSlowSu, scSlowSp, scProcGather, 
		scProcMate, scProcTrade, scProcMisc;
	public JTextField [] scGrp;
	public JLabel [] scGrpName;
	public JTextField ssCols, ssRows, ssMaxCols, ssMaxRows, ssDensity, ssCoordShow,
		ssCoordDetail, ssCoordSize, ssCoordColor, ssPanelHeight, ssPanelMax, 
		ssPanelMin, ssProcGather, ssProcMate, ssProcTrade, ssProcSeasons, ssProcCulture,
		ssProcDisease, ssProcCombat,
		ssProcPollution, ssProcInherit, ssFertSugar, ssFertSpice, ssSeasonDuration,
		ssDiseaseCount, ssDiseaseCostSugar, ssDiseaseCostSpice, ssDiseaseCostTrade;
	public JTextField ceBground, ceBar, ceSenior, ceChild, ce3D, ceLimitSugar, ceLimitSpice,
		ceSugarMin, ceSugarMax, ceSpiceMin, ceSpiceMax, ceRenewSuSummer, ceRenewSuWinter, 
		ceRenewSpSummer, ceRenewSpWinter, cePolluSugar, cePolluSpice, cePolluDispersal,
		ceDiseaseCount;
	
	public JLabel ci2aLbl1, ci2aLbl2, ci2aLbl3, ci2aLbl4, ci2aLbl5, ci2aLbl6, ci2aLbl7, ci2aLbl8,
				ci2bLbl9, ci2bLbl10;
	public JTextField ciVision, ciSex, ciPersona, ciSuMin, ciSuMax, ciSuMetab, ciSpMin, 
		ciSpMax, ciSpMetab, ciLifeMin, ciLifeMax, ciSugarPoor, ciSpicePoor, 
		ciMateMaleMin, ciMateMaleMax, ciMateMaleGap, ciMateFemaleMin, ciMateFemaleMax, 
		ciMateFemaleGap, ciColorType, ciCultureTagLen, ciCultureTagDivider, ciCultureTagFlipType, 
		ciCultureTagMinStickyFlip, ciKinMate,	ciInheritInitial, ciSurnamesUnique, ciSurnamesFather;
	public JTextField ciDiseaseInitial, ciImmuneSysLen, ssDiseaseLenMin, ssDiseaseLenMax;	
		
	public JTextField [] ciGroupInterval, ciGroupName, ciGroupColor;
	public JPanel [] ciColorPanel;
	public JTextField deDebug, deCriticalError, deCmdFeedback, deProgFlow, deCitizenBirth, 
		deCitizenDeath, deSugarSearch, deProductionSugar, deProductionSpice, deMating, 
		deMateSearch, deMateSelection, deChildBirth, dePollution, deTime, deInheritance, 
		deBarter, deBarterSort, deBarterExchange, deCulture, deCultureTransmit, 
		deCultureVolatile, deCultureSticky ,deDisease, deDiseaseCost, deDiseaseTreat,
		deDiseaseTransmit;
		
	public int gCount;
	
	public final Border blackline = BorderFactory.createLineBorder(Color.black);
	//public Border raisedetched, loweredetched, raisedbevel, loweredbevel, empty, paneEdge;
	//Puts 10 extra pixels at the sides and bottom of each pane.
	public final Border paneEdge = BorderFactory.createEmptyBorder(5,5,5,5);
	public final Border doubleBlack = BorderFactory.createCompoundBorder(blackline,paneEdge),
	//raisedetched = BorderFactory.createEtchedBorder(EtchedBorder.RAISED);
	//loweredetched = BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);
	//raisedbevel = BorderFactory.createRaisedBevelBorder();
	//loweredbevel = BorderFactory.createLoweredBevelBorder();
	empty = BorderFactory.createEmptyBorder();
		
    /**
     * Initialises the GUI including the CellSpace object and starts the 
     * Sugarscape thread.
     * 
     */
    public void init()
    {
        String param;

        // set background
        //setBackground(new Color(GoLconst.CELL_BG,GoLconst.CELL_BG,GoLconst.CELL_BG));	
        // read parameters from HTML
        param = getParameter("cellsize");
        if (param == null)
        {	cellSize = GoLconst.GRID_CELLSIZE;
        } else
        {   cellSize = Integer.valueOf(param).intValue();	}

        param = getParameter("cellcols");
        if (param == null)
        {	this.cellCols = GoLconst.GRID_COLUMNS;
        } else
        {	this.cellCols = Integer.valueOf(param).intValue();	}

        param = getParameter("cellrows");
        if (param == null)
        {   this.cellRows = GoLconst.GRID_ROWS;
        } else
        {   this.cellRows = Integer.valueOf(param).intValue();	}

        param = getParameter("gentime");
        if (param == null)
        {	genTime = GoLconst.GRID_REFRESH_FAST;
        } else
        {   genTime = Integer.valueOf(param).intValue();	}

		if (GoLconst.TEXTAREA_HEIGHT > 20 || GoLconst.TEXTAREA_HEIGHT < 1)
		{	if (GoLconst.DEBUG_CRITICAL_ERROR || GoLconst.DEBUG)
				System.out.println("TEXTAREA_HEIGHT:Invalid value specified, assuming 10!!\n");
			if(GoLconst.TEXTAREA_HEIGHT == 0)
			{
				;
			}

			GoLconst.TEXTAREA_HEIGHT = 10;
		}
		
		try {	UIManager.setLookAndFeel(
			//"javax.swing.plaf.metal.MetalLookAndFeel"
			"com.sun.java.swing.plaf.windows.WindowsLookAndFeel"
			//"com.sun.java.swing.plaf.motif.MotifLookAndFeel"
			);
		} catch (Exception e) { }
		
		c = createTemplateList();
        speed = createSpeedList();
        
        startstopButton = new JButton(startLabel);
		startstopButton.addActionListener(new ControlStartStop(this));

		nextButton = new JButton(nextLabel);
		nextButton.addActionListener(new ControlNext(this));

		genLabel = new JLabel("Timeperiods: 0");

		controls = new JPanel();
		controls.add(c);
		controls.add(nextButton);
		controls.add(startstopButton);
		controls.add(speed);
		controls.add(genLabel);
				
        //Text objects to accept commands & display feedback
        textField = new JTextField(this.cellCols * 2 + 1);
        textField.addActionListener(this);
        textArea = new JTextArea( GoLconst.TEXTAREA_HEIGHT, this.cellCols * cellSize);
        textArea.setBorder(blackline);
        textArea.setEditable(false);
		JScrollPane scrollPane = new JScrollPane(textArea,
			JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
			JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

		Container cp = getContentPane();
		cp.add(scrollPane);

        //The Sugarscape Grid 
        cellSpace = new CellSpace( cellSize, this.cellCols, this.cellRows, textArea);
        
        //Chk global constantfor invalid values
        textArea.append(GoLconst.chkGoLconstants());
        
		tabPanel = new JTabbedPane();

 		scorePanel = new JPanel(new FlowLayout());
		createScorePanel();

		sScapePanel = new JPanel();
		createSscapePanel();

		cellPanel = new JPanel();
		cellPanel.setLayout(new FlowLayout(FlowLayout.CENTER,10,10));
		createCellPanel();

		citizenPanel = new JPanel();
		citizenPanel.setLayout(new FlowLayout(FlowLayout.CENTER,10,10));
		createCitizenPanel();

		configPanel = new JPanel();
		configPanel.setLayout(new FlowLayout(FlowLayout.CENTER,10,10));
		configPanel.setBorder(blackline);
		createConfigPanel();

		historyPanel = new JPanel();
		historyPanel.setLayout(new BorderLayout());
		historyPanel.add(scrollPane,"North");
		historyPanel.add(textField, "South");
		
		tabPanel.setMaximumSize(new Dimension(tabPanel.getHeight(),tabPanel.getWidth()));
		tabPanel.addTab("Scorecard", scorePanel);
		tabPanel.addTab("Cellspace", sScapePanel);
		tabPanel.addTab("Cell", cellPanel);
		tabPanel.addTab("Citizen", citizenPanel);
		tabPanel.addTab("Configure", configPanel);
		tabPanel.addTab("History", historyPanel);
		tabPanel.setSelectedComponent(scorePanel);
		
		cp.add(controls,BorderLayout.NORTH);
		cp.add(cellSpace,BorderLayout.CENTER);
		cp.add(tabPanel, BorderLayout.SOUTH);
		

		resize(this.cellCols * this.cellSize, GoLconst.CSPACE_OFFSET + 
			((cellRows) * cellSize) + (int)(GoLconst.TEXTAREA_HEIGHT * 16.67) );
		
		setVisible(true);
        if (GoLconst.DEBUG_CMD_FEEDBACK)
            textArea.append( "Sugarscape ver. " + GoLconst.PROGRAM_VERSION + "\n"
                    + "Type 'SS' to setup Sugarscape or '?' for help.\n");
    }

    /**
     * No start() to prevent starting immediately.<BR>
     * This method enables the GUI to initialize and await action from the user
     * to begin execution, rather than executing as soon as the applet has 
     * finished loading.
     * 
     */
    public void start2()
    {
        if (gameThread == null)
        {
            gameThread = new Thread(this);
            gameThread.start();
        }
    }

    /** Pauses the current simulation */
    public void stop()
    {
        if (gameThread != null)
        {
            gameThread.interrupt();	//stop();
            gameThread = null;
        }
    }

    /**
     * This method monitors status of the simulation and stops execution if the population
     * dies out.
     * It continues the next cycle until interrupted by the user.
     * It also controls the pace of execution of the simulation at one of three speeds
     * <UL>
     * <LI> Hyper
     * <LI> Fast
     * <LI> Slow
     * </UL>
     * 
     */
    public void run()
    {
        while (gameThread != null)
        {
			if (!GoLconst.flagSugarscape)
				cellSpace.next();
			else if (cellSpace.getPopulation() == 0)
            {
                textArea.append("\nPopulation extinct, please reset Sugarscape!!\n\n");
                if (GoLconst.flagDemo)
                {
                    textArea.append("Test/Demo aborted\n");
                    GoLconst.flagDemo = false;
                    textField.setText("");
                }
                cellSpace.repaint();
                startstopButton.setText(startLabel);
 				setVisible(true);
                gameThread = null;
            } else 		//(GoLconst.flagSugarscape)
            {
                cellSpace.nextScape(); 
                //display sugar distribution
                if (GoLconst.DEBUG || GoLconst.DEBUG_SUGAR_PRODUCTION)
                {	cellSpace.showSugarStats(0, 0, this.cellCols, this.cellRows);	}
                if (GoLconst.DEBUG || GoLconst.DEBUG_SPICE_PRODUCTION)
                {	cellSpace.showSpiceStats(0, 0, this.cellCols, this.cellRows);   }
            } 
			updateScorePanel();
			repaint();
            cellSpace.repaint();
            tabPanel.repaint();
            
			setVisible(true);
            showGenerations();
            try {java.lang.Thread.sleep(genTime);	}
            catch (InterruptedException e) {}
        }
    }

    /**
     * Implements various template options for the Game of Life (multiple) and the 
     * Sugarscape (one)
     * 
     * @param	arg
     * @return	return boolean, always true
     */
    public void actionControls(String arg)
    {
        if (clear.equals(arg)) // clear the grid
        {
            cellSpace.clear();
			updateScorePanel();
            cellSpace.repaint();
            showGenerations();
			setVisible(true);
            GoLconst.flagSugarscape = false;
        } else if (sugarscape.equals(arg)) // Generate Sugarscape
        { // Note: there are 2 drawSugarscape functions
            //	Sugarscape.drawSugarscape() & Cellspace.drawSugarscape()
            GoLconst.flagSugarscape = true;
            drawSugarscape(this.cellCols, this.cellRows);
            if (GoLconst.DEBUG_CMD_FEEDBACK)
                textArea.append("Sugarscape set, type 'GO' to execute or '?' for help.\n");
        } else if (random.equals(arg)) // generate random shape
        {
            drawRandomShape();
            GoLconst.flagSugarscape = false;
        } else if (glider.equals(arg)) // misc shapes
        {
            int shape[] = { 0, 1, 1, 2, 2, 2, 2, 1, 2, 0 };
            drawShape(3, 3, shape);
            GoLconst.flagSugarscape = false;
        } else if (bigbang.equals(arg)) // misc shapes
        {
            int shape[] = {0,0, 0,1, 0,2, 0,4, 0,5, 0,6,
				1,0, 1,6, 2,1, 2,5, 4,1, 4,5,
				5,0, 5,6, 6,0, 6,1, 6,2, 6,4, 6,5, 6,6	};
            drawShape(7, 5, shape);
            GoLconst.flagSugarscape = false;
        } else if (exploder1.equals(arg))
        {
            int shape[] = { 0, 1, 0, 2, 1, 0, 1, 1, 1, 3, 2, 1, 2, 2 };
            drawShape(3, 4, shape);
            GoLconst.flagSugarscape = false;
        } else if (exploder2.equals(arg))
        {
            int shape[] = { 0,0, 0,1, 0,2, 0,3, 0,4, 2,0, 2,4, 4,0, 4,1, 4,2, 4,3, 4,4  };
            drawShape(5, 5, shape);
            GoLconst.flagSugarscape = false;
        } else if (row10.equals(arg))
        {
            int shape[] = { 0,0, 1,0, 2,0, 3,0, 4,0, 5,0, 6,0, 7,0, 8,0, 9,0 };
            drawShape(10, 1, shape);
            GoLconst.flagSugarscape = false;
        } else if (fish.equals(arg))
        {
            int shape[] = { 0, 1, 0, 3, 1, 0, 2, 0, 3, 0, 3, 3, 4, 0, 4, 1, 4, 2 };
            drawShape(5, 4, shape);
            GoLconst.flagSugarscape = false;
        } else if (pump.equals(arg))
        {
            int shape[] = {0,3, 0,4, 0,5, 1,0, 1,1, 1,5, 2,0, 2,1, 2,2, 2,3, 2,4, 
                4,0, 4,1, 4,2, 4,3, 4,4, 5,0, 5,1, 5,5, 6,3, 6,4, 6,5  };
            drawShape(7, 6, shape);
            GoLconst.flagSugarscape = false;
        } else if (gun.equals(arg))
        {
            int shape[] = {0,2, 0,3, 1,2, 1,3, 8,3, 8,4, 9,2, 9,4, 10,2, 10,3, 16,4, 16,5, 16,6, 
                		17,4, 18,5, 22,1, 22,2, 23,0, 23,2, 24,0, 24,1, 24,12, 24,13, 25,12, 
                		25,14, 26,12, 34,0, 34,1, 35,0, 35,1, 35,7, 35,8, 35,9, 36,7, 37,8  };
            drawShape(38, 15, shape);
            GoLconst.flagSugarscape = false;
        } else if (nextLabel.equals(arg))
        {
            if (GoLconst.flagSugarscape)
            {
                cellSpace.nextScape();
				updateScorePanel();
        	}
            else cellSpace.next();

            cellSpace.repaint();
            showGenerations();
			setVisible(true);
        } else if (startLabel.equals(arg)) 
        {
            start2();
            startstopButton.setText(stopLabel);
            if (GoLconst.DEBUG_CMD_FEEDBACK)
                textArea.append("Execution started/resumed, type '?' for help.\n");
        } else if (stopLabel.equals(arg)) // stop
        {
            stop();
            startstopButton.setText(startLabel);
        } else if (slow.equals(arg))
        {
            genTime = GoLconst.GRID_REFRESH_SLOW;
        } else if (fast.equals(arg)) 
        {
            genTime = GoLconst.GRID_REFRESH_FAST;
        } else if (hyper.equals(arg)) // hyperspeed
        {
            genTime = GoLconst.GRID_REFRESH_HYPER;
        }
    }

    /**
     * Program info accessible through the menu options of the Appletviewer
     * 
     * @return	program information - this information is displayed when the user types "INFO" 
     * at the command line of the Sugarscape.
     */
    public String getAppletInfo()
    {
        return "Sugarscape Program Info\n"
            + "-----------------------------------\n"
            + "author: abraham kannankeril, \nVersion: " + GoLconst.PROGRAM_VERSION
            + "\nrel. date : " + GoLconst.PROGRAM_RELEASE_DATE
            + "\n\tImplements the 'Sugarscape' model, based on the book"
            + "\n\t'Growing Artificial Societies by Joshua M. Epstein & Robert Axtell"
            + "\nparts borrowed from Game Of Life 1.3, copyright 1996-2001 Edwin Martin";
    }

    /**
     * Show number of cycles that have executed and the current population count
     * 
     */
    public void showGenerations()
    {
        if (GoLconst.flagSugarscape)
            genLabel.setText("Gen'rtn: " + GoLconst.timePeriod
                    + "   Pop'ltn: " + cellSpace.getPopulation() + "       ");
        else
            genLabel.setText("Gen'rtn: " + GoLconst.timePeriod);
    }

    /**
     * Draws the shape to canvas depending on the template selected for all selections 
     * relating to the Game of Life.<BR>
     * Calls the relevant method in the CellSpace class.
     * 
     * @param	shapeWidth	defines the max width of the requested template
     * @param	shapeHeight	defines the max height of the requested template
     * @param	shape			boolean array that defines exact shape to be drawn
     * for a given template.
     */
    public void drawShape(int shapeWidth, int shapeHeight, int shape[])
    {
        if (!cellSpace.drawShape(shapeWidth, shapeHeight, shape))
            showStatus("Shape is too big to fit.");
        else
        {
            showStatus("");
            cellSpace.repaint();
            showGenerations();
        }
    }

    /**
     * Draws a random template on the Game of Life grid. 
     * Calls the relevant method in the CellSpace class.
     * 
     */
    public void drawRandomShape()
    {
        cellSpace.drawRandomShape();
        showStatus("Random grid generated!");
        cellSpace.repaint();
        showGenerations();
    }

    /**
     * Implements the Sugarscape template by calling the relevant method in 
     * the CellSpace class.<BR>
     * Provides the option to specify physical dimensions by passing parameters 
     * for columns and rows through HTML code. This applies to webpages executing
     * the program as an applet.
     * 
     * @param	cellCols	number of columns passed as parameters by a parent webpage.
     * @param	cellRows	number of rows passed as parameters by a parent webpage.
     * @see	CellSpace#CellSpace(int, int, int, JTextArea)
     */
    public synchronized void drawSugarscape(int cellCols, int cellRows)
    {
        cellSpace.drawSugarscape(cellCols, cellRows);
        showStatus("Initial Sugarscape population " + cellSpace.getPopulation());
        showGenerations();
		updateScorePanel();
        cellSpace.repaint();
        setVisible(true);
    }

    /**
     * This Method implements command-line control for the program. It enables 
     * manipulation of program parameters that are otherwise only configurable
     * in the GoLconst.java source file.<BR>
     * Accepts text commands in a textfield at the bottom of the Sugarscape GUI.<BR>
     * Users can type 'help' in this field for a reference list of commands.
     * 
     * @param	evt
     */
	public void actionPerformed(ActionEvent evt)
	{   String text = evt.getActionCommand();
		JTextField field = (JTextField)evt.getSource();
		String param = field.getText().toLowerCase();
		text = text.toUpperCase();
		
		textArea.append("\nActionCommand = " + evt.getActionCommand() + "\n" );
		textArea.append("Param = " + param + "\n");
						
		if( GoLconst.flagDemo )
		{
			if( GoLconst.demoCntr == 0 )
			{	textField.setText("INFO");	}
			else if( GoLconst.demoCntr == 1 )
			{	textField.setText("DEBUG");	}
			else if( GoLconst.demoCntr == 2 )
			{	textField.setText("GO");	}
			else if( GoLconst.demoCntr == 3 )
			{	textField.setText("STOP");	}
			else if( GoLconst.demoCntr == 4 )
			{	textField.setText("TEXTAREA - 5");	}
			else if( GoLconst.demoCntr == 5 )
			{	textField.setText("COLS + 4");	}
			else if( GoLconst.demoCntr == 6 )
			{	textField.setText("FAST");	}
			else if( GoLconst.demoCntr == 7 )
			{	textField.setText("GO");	}
			else if( GoLconst.demoCntr == 8 && cellSpace.getPopulation() > 0)
			{	textField.setText("STOP");	}
			else if( GoLconst.demoCntr == 9 && cellSpace.getPopulation() > 0)
			{	textField.setText("POPULATION");	}
			else if( GoLconst.demoCntr == 10 )
			{	textField.setText("Done!!");		}
			else
			{	GoLconst.flagDemo = false;
				textArea.append( "Test/Demo complete!!\n" );
				textField.setText("");
				return;
			}
			GoLconst.demoCntr++;
		}
		//**Clear Grid or the Display area**
		if( text.startsWith("CLEAR") )
		{	if( text.equals("CLEAR GRID") )
			{
				c.setSelectedItem( clear );
				cellSpace.clear();
				updateScorePanel();
				updateSscapePanel();
				cellSpace.repaint();
				showGenerations();
				setVisible(true);
				GoLconst.flagSugarscape = false;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("Sugarscape cleared\n");
			}
			else if( text.equals("CLEAR TEXT") )
			{	textArea.setText("");	}
			else
			{	if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("Clear what?\n");
			}
		}
		//**Display Stats For Population or Specific Citizen**
		else if( text.startsWith("SHOW") || text.startsWith("CITIZEN")
				|| text.startsWith("POPULATION") || text.equals("TIME")
				|| text.startsWith("CELL") || text.equals("DEBUG") )
		{
			if( text.startsWith("SHOW CITIZEN") || text.startsWith("CITIZEN")
				|| text.startsWith("SHOW CELL") || text.startsWith("CELL"))
			{	String targetGroup;
				int col=0, row=0;

				StringTokenizer token = new StringTokenizer(text," \t\n[]");
				if( (targetGroup = token.nextToken()) == "SHOW" )
					targetGroup = token.nextToken();
				if(targetGroup.equals("CITIZEN") || targetGroup.equals("CELL")  )
				{
					col = Integer.parseInt( token.nextToken());
					row = Integer.parseInt( token.nextToken());
				}
				if(col < 0 || row < 0)
					textArea.append("Invalid coordinate(s)\n");
				else if( targetGroup.equals("CITIZEN") )
					textArea.append(cellSpace.showCitizenStats("Citizen: ", col, row));	
				else if( targetGroup.equals("CELL") )
					textArea.append(cellSpace.showCellStats(col,row) );
			}
			else if( text.equals("SHOW POPULATION") || text.equals("POPULATION"))
			{	cellSpace.showPopulationStats(1);	}
			else if( text.equals("SHOW TIME") || text.equals("TIME"))
			{	cellSpace.showProcessStats();	}
			else if( text.equals("SHOW DEBUG") || text.equals("DEBUG"))
			{	cellSpace.showDebugSettings();	}
			else
			{	if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append(text + " what?\n");
			}
		}

		//**Control Program Pace
		else if( text.equals("HYPER") || text.equals("H") )
		{	genTime = GoLconst.GRID_REFRESH_HYPER;
			speed.setSelectedItem( hyper );
			if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("Switching to Hyper mode!\n");
		}
		else if( text.equals("FAST") || text.equals("F") )
		{	genTime = GoLconst.GRID_REFRESH_FAST;
			speed.setSelectedItem( fast );
			if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("Switching to Fast mode!\n");
		}
		else if( text.equals("SLOW") || text.equals("S") )
		{	genTime = GoLconst.GRID_REFRESH_SLOW;
			speed.setSelectedItem( slow );
			if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("Switching to Slow mode!\n");
		}
		else if( text.equals("STOP") || text.equals("X") )
		{	gameThread.interrupt();
			gameThread = null;
			startstopButton.setText( startLabel );
			if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("Execution paused, type 'START' to continue.\n");
		}
		else if( text.equals("START") || text.equals("GO") ) // start
		{	start2();
			startstopButton.setText( stopLabel );
			if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("Execution started/resumed, type '?' for help.\n");
		}
		else if( text.startsWith("NEXT") || text.startsWith("+") ) // start
		{	if(GoLconst.flagSugarscape)
			{	StringTokenizer token = new StringTokenizer(text," \t");
				if( token.countTokens() > 1 )
				{	 
					token.nextToken();	//grab first item & ignore
					//Determine number of timePeriods to execute
					int cntr = Integer.parseInt( token.nextToken());
					while( cntr-- > 0 && cellSpace.getPopulation() > 0)
					{	cellSpace.nextScape();	//(cellCols, cellRows);
						showGenerations();
					}
				} else cellSpace.nextScape();	//(cellCols, cellRows);
			} else cellSpace.next();

			if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("Ok\n");
		}
		else if( text.startsWith("COLS") || text.startsWith("ROWS") ) // start
		{	if(  !GoLconst.flagSugarscape )
			{	textArea.append("Resize permitted in Sugarscape only!\n");
				return;
			}
			String msgStr = "";
			
			StringTokenizer token = new StringTokenizer(text," \t");
			if( token.countTokens() == 3 )
			{	String cmd = token.nextToken();
				String operand = token.nextToken();
				int value  = Integer.parseInt(token.nextToken()); //number of rows
				value = (value > 0) ? value : 0;	//value cannot be negative

				if( cmd.equals("COLS") )
				{	if( operand.equals("+") )
					{
						if(cellCols <= GoLconst.GRID_COLS_MAX)
							cellCols = cellCols + value;
						else
							cellCols = GoLconst.GRID_COLS_MAX;
						
						//cellCols += (cellCols+value) <= GoLconst.GRID_COLS_MAX ?
						//		value : GoLconst.GRID_COLS_MAX - cellCols;
					}
					else if( operand.equals("-") )
					{
						cellCols -= (value < cellCols) ? value : 0;
					}
					else msgStr += "Invalid operand\n";

					GoLconst.GRID_COLUMNS = cellCols;
				}
				else //cmd.equals("ROWS")
				{	if( operand.equals("+") )
					{
						if(cellRows <= GoLconst.GRID_ROWS_MAX)
							cellRows = cellRows + value;
						else
							cellRows = GoLconst.GRID_ROWS_MAX;
	
						//cellRows += (cellRows+value) <= GoLconst.GRID_ROWS_MAX ?
							//		value : GoLconst.GRID_ROWS_MAX - cellRows;
					}
					else if( operand.equals("-") )
					{
						cellRows -= (value < cellRows) ? value : 0;
					}
					else msgStr += "Invalid operand\n";
					
					GoLconst.GRID_ROWS = cellRows;	
				}

				//cellCols & cellRows are separate vars in Sugarscape.java &
				//CellSpace.java, both need to be updated
				cellSpace.resizeCols( cellCols );
				cellSpace.resizeRows( cellRows );	//refer comment above

				//If new cells have been added, create them
				if( operand.equals("+") )
					cellSpace.createCells();

				if( GoLconst.timePeriod == 0 )
					drawSugarscape(cellCols, cellRows);	//regenerate Sugarscape
				else 
				{	int orphans = cellSpace.deleteOrphanCitizens(cellCols, cellRows);
					if( GoLconst.DEBUG || GoLconst.DEBUG_CITIZEN_DEATH ||
						GoLconst.DEBUG_CMD_FEEDBACK )
					{	msgStr +=  orphans + " citizens eliminated\n"; }
				}
				//This function is different from the resize above
				resize(this.cellCols * this.cellSize, GoLconst.CSPACE_OFFSET + 
					((cellRows) * cellSize) + (int)(GoLconst.TEXTAREA_HEIGHT * 16.67) );
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					msgStr += "New Grid size: "
						+ this.cellCols + " cols by " + this.cellRows + " rows\n";
			}
			else if( GoLconst.DEBUG_CMD_FEEDBACK )
				msgStr += "Missing numeric or too many parameters!\n";

			textArea.append(msgStr);
			msgStr = null;
		}

		//**Provide General Help with Interactive Commands**/
		else if( text.equals("HELP") || text.equals("?") )
		{	textArea.append(
			"\n\nSugarscape Command-line Options\n" +
			"=============================\n" +
			"SUGARSCAPE / SS Creates a new world on the Sugarscape" +
			"START / GO   \t Starts the program run\n" +
			"STOP  / X    \t Pause the program run\n" +
			"CLEAR TEXT   \t clears the text in the display area\n" +
			"CLEAR GRID   \t Stops the run & clears the grid\n" +
			"H / F / S / X\t Controls program pace: Hyper / Fast / Slow / Pause\n" +
			"? / HELP     \t Display this screen\n" +
			"VERSION / INFO\t Display Program Information\n" +
			"SEASONS      \t Start/stop change of seasons on Sugarscape" +
			"POLLUTION    \t Start/stop pollution processes on Sugarscape" +
			"TEXTAREA + n \t 'n' is the number of rows added to the display area\n" +
			"\t\t-n \t 'n' is the number of rows subtracted from the display area\n" +

			"COLS + n     \t 'n' is the number of cols added to the Sugarscape\n" +
			"\t\t-n \t 'n' is the number of cols subtracted from the Sugarscape\n" +
			"ROWS + n     \t 'n' is the number of rows added to the Sugarscape\n" +
			"\t\t-n \t 'n' is the number of rows subtracted from the Sugarscape\n" +
			"-----------------\n" +
			"GRID COORD SHOW\t Display/hide grid coordinates for Sugarscape\n" +
			"LIMIT CELL SUGAR\tSet/remove limits on sugar accumulation in each cell\n" +
			"\nDebugging Program Execution\n" +
			"--------------------------------------\n" +
			"DEBUG CRITICAL ERROR\tDisplay program inconsistencies\n" +
			"DEBUG ALL\t\t\t\tDisplay all events in a program\n" +
			"DEBUG PROGRAM FLOW\t\tTraces program process flow\n" +
			"DEBUG PROCESS TIME\t\tTracks time for gathering/mating/other events\n" +
			"DEBUG CITIZEN BIRTH\t\tDisplay 'Birth' events\n" +
			"DEBUG CITIZEN DEATH\t\tDisplay 'Death' events\n" +
			"DEBUG SEARCH SUGAR\t\tTraces each citizen's search for sugar\n" +
			"DEBUG SUGAR PRODUCTION\tDisplay sugar distribution on Cellscape\n" +
			"DEBUG MATING\t\t\tTraces each citizen's mating process\n" +
			"DEBUG MATING SEARCH\t\tTraces each citizen's search for mates\n" +
			"DEBUG MATING SELECTION\tTraces each citizen's mate selection\n" +
			"DEBUG MATING BIRTH\t\tDisplay 'Birth' events resulting from mating\n" +
			"DEBUG POLLUTION\t\tTracks cell pollution generated by gathering\n" );

			textArea.append("-----------------------------------------------" +
			"-----------------------------------------------\n" +
			"Note: Commands are case-insensitive.\nDebugging options will slow down " +
			"program, especially the 'DEBUG' option\n" +
			"Use the display area scrollbar to view the feedback.\n");
		}

		//**Provide Version Info**/
		else if( text.equals("VERSION") || text.equals("INFO") )
		{	textArea.append( "\nSugarscape Program Info\n"
			+ "-----------------------------------\n"
			+ "Sugarscape: abraham kannankeril, \nVersion: " + GoLconst.PROGRAM_VERSION
			+ "\nRel. Date : " + GoLconst.PROGRAM_RELEASE_DATE
			+ "\n\tver. 2.0 implements 'The Sugarscape', based on the book"
			+ "\n\t'Growing Artificial Societies by Joshua M. Epstein & Robert Axtell"
			+ "\nparts borrowed from Game Of Life v. 1.3, copyright 1996-2001 Edwin Martin"
			);
		}
		//**Display Feedback For Specific Processes**/
		else if( text.startsWith("DEBUG ") )
		{	if( text.equals("DEBUG CRITICAL ERROR") )
			{
				if(param.equals(Boolean.toString(!GoLconst.DEBUG_CRITICAL_ERROR)))
					GoLconst.DEBUG_CRITICAL_ERROR = !GoLconst.DEBUG_CRITICAL_ERROR;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG_CRITICAL_ERROR = "
										+ GoLconst.DEBUG_CRITICAL_ERROR + "\n");
			}
			else if( text.equals("DEBUG CMD FEEDBACK") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_CMD_FEEDBACK)))
					GoLconst.DEBUG_CMD_FEEDBACK = !GoLconst.DEBUG_CMD_FEEDBACK;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG_CMD_FEEDBACK = "
										+ GoLconst.DEBUG_CMD_FEEDBACK + "\n");
			}

			else if( text.equals("DEBUG PROGRAM FLOW") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_PROGRAM_FLOW)))
					GoLconst.DEBUG_PROGRAM_FLOW = !GoLconst.DEBUG_PROGRAM_FLOW;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG_PROGRAM_FLOW = "
										+ GoLconst.DEBUG_PROGRAM_FLOW + "\n");
			}
			else if( text.equals("DEBUG CITIZEN BIRTH") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_CITIZEN_BIRTH)))
					GoLconst.DEBUG_CITIZEN_BIRTH = !GoLconst.DEBUG_CITIZEN_BIRTH;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG_CITIZEN_BIRTH = "
										+ GoLconst.DEBUG_CITIZEN_BIRTH + "\n");
			}
			else if( text.equals("DEBUG CITIZEN DEATH") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_CITIZEN_DEATH)))
					GoLconst.DEBUG_CITIZEN_DEATH = !GoLconst.DEBUG_CITIZEN_DEATH;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG_CITIZEN_DEATH = "
										+ GoLconst.DEBUG_CITIZEN_DEATH + "\n");
			}
			else if( text.equals("DEBUG SEARCH SUGAR") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_SEARCH_SUGAR)))
					GoLconst.DEBUG_SEARCH_SUGAR = !GoLconst.DEBUG_SEARCH_SUGAR;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG_SEARCH_SUGAR = "
										+ GoLconst.DEBUG_SEARCH_SUGAR + "\n");
			}
			else if( text.equals("DEBUG SUGAR PRODUCTION") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_SUGAR_PRODUCTION)))
					GoLconst.DEBUG_SUGAR_PRODUCTION = !GoLconst.DEBUG_SUGAR_PRODUCTION;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG_SUGAR_PRODUCTION = "
										+ GoLconst.DEBUG_SUGAR_PRODUCTION + "\n");
			}
			else if( text.equals("DEBUG SPICE PRODUCTION") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_SPICE_PRODUCTION)))
					GoLconst.DEBUG_SPICE_PRODUCTION = !GoLconst.DEBUG_SPICE_PRODUCTION;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG_SPICE_PRODUCTION = "
										+ GoLconst.DEBUG_SPICE_PRODUCTION + "\n");
			}
			else if( text.equals("DEBUG MATING SEARCH") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_MATING_SEARCH)))
					GoLconst.DEBUG_MATING_SEARCH = !GoLconst.DEBUG_MATING_SEARCH;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG_MATING_SEARCH = "
										+ GoLconst.DEBUG_MATING_SEARCH + "\n");
			}
			else if( text.equals("DEBUG MATING SELECTION") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_MATING_SELECTION)))
					GoLconst.DEBUG_MATING_SELECTION = !GoLconst.DEBUG_MATING_SELECTION;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG_MATING_SELECTION = "
										+ GoLconst.DEBUG_MATING_SELECTION + "\n");
			}
			else if( text.equals("DEBUG MATING BIRTH") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_MATING_BIRTH)))
					GoLconst.DEBUG_MATING_BIRTH = !GoLconst.DEBUG_MATING_BIRTH;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG_MATING_BIRTH = "
										+ GoLconst.DEBUG_MATING_BIRTH + "\n");
			}
			else if( text.equals("DEBUG MATING") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_MATING)))
					GoLconst.DEBUG_MATING = !GoLconst.DEBUG_MATING;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG_MATING = " + GoLconst.DEBUG_MATING + "\n");
			}
			else if( text.equals("DEBUG POLLUTION") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_POLLUTION)))
					GoLconst.DEBUG_POLLUTION = !GoLconst.DEBUG_POLLUTION;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG_POLLUTION = " + GoLconst.DEBUG_POLLUTION + "\n");
			}
			else if( text.equals("DEBUG BARTER") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_BARTER)))
					GoLconst.DEBUG_BARTER = !GoLconst.DEBUG_BARTER;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG_BARTER = " + GoLconst.DEBUG_BARTER + "\n");
			}
			else if( text.equals("DEBUG BARTER SORT") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_BARTER_SORT)))
					GoLconst.DEBUG_BARTER_SORT = !GoLconst.DEBUG_BARTER_SORT;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG BARTER_SORT = " + GoLconst.DEBUG_BARTER_SORT + "\n");
			}
			else if( text.equals("DEBUG BARTER EXCHANGE") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_BARTER_EXCHANGE)))
					GoLconst.DEBUG_BARTER_EXCHANGE = !GoLconst.DEBUG_BARTER_EXCHANGE;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG BARTER_EXCHANGE = " + GoLconst.DEBUG_BARTER_EXCHANGE + "\n");
			}



			else if( text.equals("DEBUG CULTURE") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_CULTURE)))
					GoLconst.DEBUG_CULTURE = !GoLconst.DEBUG_CULTURE;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG CULTURE = " + GoLconst.DEBUG_CULTURE + "\n");
			}
			else if( text.equals("DEBUG CULTURE TRANSMIT") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_CULTURE_TRANSMIT)))
					GoLconst.DEBUG_CULTURE_TRANSMIT = !GoLconst.DEBUG_CULTURE_TRANSMIT;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG CULTURE TRANSMIT = " + GoLconst.DEBUG_CULTURE_TRANSMIT + "\n");
			}
			else if( text.equals("DEBUG CULTURE VOLATILE") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_CULTURE_VOLATILE)))
					GoLconst.DEBUG_CULTURE_VOLATILE = !GoLconst.DEBUG_CULTURE_VOLATILE;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG CULTURE VOLATILE = " + GoLconst.DEBUG_CULTURE_VOLATILE + "\n");
			}
			else if( text.equals("DEBUG CULTURE STICKY") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_CULTURE_STICKY)))
					GoLconst.DEBUG_CULTURE_STICKY = !GoLconst.DEBUG_CULTURE_STICKY;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG CULTURE STICKY = " 
							+ GoLconst.DEBUG_CULTURE_STICKY + "\n");
			}


			else if( text.equals("DEBUG DISEASE") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_DISEASE)))
					GoLconst.DEBUG_DISEASE = !GoLconst.DEBUG_DISEASE;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG DISEASE = " + GoLconst.DEBUG_DISEASE + "\n");
			}
			else if( text.equals("DEBUG DISEASE COST") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_DISEASE_COST)))
					GoLconst.DEBUG_DISEASE_COST = !GoLconst.DEBUG_DISEASE_COST;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG DISEASE COST = " + GoLconst.DEBUG_DISEASE_COST + "\n");
			}
			else if( text.equals("DEBUG DISEASE TREAT") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_DISEASE_TREAT)))
					GoLconst.DEBUG_DISEASE_TREAT = !GoLconst.DEBUG_DISEASE_TREAT;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG DISEASE TREAT = " + GoLconst.DEBUG_DISEASE_TREAT + "\n");
			}
			else if( text.equals("DEBUG DISEASE TRANSMIT") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_DISEASE_TRANSMIT)))
					GoLconst.DEBUG_DISEASE_TRANSMIT = !GoLconst.DEBUG_DISEASE_TRANSMIT;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG DISEASE TRANSMIT = " 
							+ GoLconst.DEBUG_DISEASE_TRANSMIT + "\n");
			}



			else if( text.equals("DEBUG PROCESS TIME") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_PROCESS_TIME)))
					GoLconst.DEBUG_PROCESS_TIME = !GoLconst.DEBUG_PROCESS_TIME;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG PROCESS TIME = " + GoLconst.DEBUG_PROCESS_TIME + "\n");
			}
			else if( text.equals("DEBUG INHERITANCE") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG_INHERITANCE)))
					GoLconst.DEBUG_INHERITANCE = !GoLconst.DEBUG_INHERITANCE;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG INHERITANCE = " + GoLconst.DEBUG_INHERITANCE + "\n");
			}
			else if( text.equals("DEBUG ALL") )
			{	if(param.equals(Boolean.toString(!GoLconst.DEBUG)))
					GoLconst.DEBUG = !GoLconst.DEBUG;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("DEBUG = " + GoLconst.DEBUG + "\n");
			}
			else
			{	if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("debug what?\n");	}
		}
		else if( text.equals("GRID COORD SHOW") )
		{	if(param.equals(Boolean.toString(!GoLconst.GRID_COORD_SHOW)))
				GoLconst.GRID_COORD_SHOW = !GoLconst.GRID_COORD_SHOW;
			if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("GRID COORD SHOW = " + GoLconst.GRID_COORD_SHOW + "\n");
		}
		else if( text.equals("LIMIT CELL SUGAR") )
		{	if(param.equals(Boolean.toString(!GoLconst.LIMIT_CELL_SUGAR)))
				GoLconst.LIMIT_CELL_SUGAR = !GoLconst.LIMIT_CELL_SUGAR;
			if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("LIMIT CELL SUGAR = " + GoLconst.LIMIT_CELL_SUGAR + "\n");
		}
		else if( text.equals("MATING") )
		{	GoLconst.INITIATE_MATING = !GoLconst.INITIATE_MATING;
			if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("INITIATE MATING = " + GoLconst.INITIATE_MATING + "\n");
		}
		else if( text.equals("SEASONS") )
		{	//if(param.equals(Boolean.toString(!GoLconst.INITIATE_SEASONS)))
			GoLconst.INITIATE_SEASONS = !GoLconst.INITIATE_SEASONS;
			if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("INITIATE SEASONS = " + GoLconst.INITIATE_SEASONS + "\n");
		}
		else if( text.equals("POLLUTION") )
		{	if(param.equals(Boolean.toString(!GoLconst.INITIATE_POLLUTION)))
				GoLconst.INITIATE_POLLUTION = !GoLconst.INITIATE_POLLUTION;
			if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("INITIATE POLLUTION = " + GoLconst.INITIATE_POLLUTION + "\n");
		}
		else if( text.equals("INHERITANCE") )
		{	if(param.equals(Boolean.toString(!GoLconst.INITIATE_INHERITANCE)))
				GoLconst.INITIATE_INHERITANCE = !GoLconst.INITIATE_INHERITANCE;
			if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("INITIATE INHERITANCE = " + GoLconst.INITIATE_INHERITANCE + "\n");
		}
		else if( text.startsWith("TEXTAREA")) // adjust display area
		{	String operand="", numStr="";
			int num=0, opIndex;
			if( text.length() > 8 )
			{	opIndex = text.indexOf("+");
				opIndex = (opIndex < 0) ? text.indexOf("-") : opIndex;
				operand = text.substring( opIndex,opIndex+1 );
				if( operand.equals("+") || operand.equals("-") )
				{	numStr = text.substring(opIndex+1).trim();
					numStr = numStr.equals("") ? numStr = "1" : numStr;
					num = Integer.valueOf(numStr).intValue();
				} else
				{	operand = "+";
					num = 0;
					if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("Invalid operand!\n");
				}
			}
			if( operand.equals("+") )
			{	GoLconst.TEXTAREA_HEIGHT =
				( GoLconst.TEXTAREA_MAX > (GoLconst.TEXTAREA_HEIGHT + num) ) ?
					GoLconst.TEXTAREA_HEIGHT + num : GoLconst.TEXTAREA_MAX;
			}
			else if( operand.equals("-") )
			{	GoLconst.TEXTAREA_HEIGHT =
				GoLconst.TEXTAREA_MIN < (GoLconst.TEXTAREA_HEIGHT - num) ?
					GoLconst.TEXTAREA_HEIGHT - num : GoLconst.TEXTAREA_MIN;
			}
			textArea.setRows(GoLconst.TEXTAREA_HEIGHT);
			if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("TEXTAREA DIMENSIONS = " + textArea.getRows() + 
					" rows by " + textArea.getColumns() + " cols\n");
			textArea.setPreferredSize(new Dimension(GoLconst.TEXTAREA_HEIGHT, 
													this.cellCols * cellSize));
			resize(this.cellCols * this.cellSize, GoLconst.CSPACE_OFFSET + 
				((cellRows) * cellSize) + (int)(GoLconst.TEXTAREA_HEIGHT * 16.67) );
			setVisible(true);
		}
		else if( text.equals("SUGARSCAPE") || text.equals("SS") )
		{	// Note: there are 2 drawSugarscape functions
			//	Sugarscape.drawSugarscape() & Cellspace.drawSugarscape()
			c.setSelectedItem( sugarscape );
			GoLconst.flagSugarscape = true;
			drawSugarscape(this.cellCols, this.cellRows);
			if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("Sugarscape set, type 'GO' to execute or '?' for help.\n");
		}
		else if( text.equals("TEST") || text.equals("DEMO") )
		{	if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("Beginning test/demo mode, press Enter after each command\n");
			GoLconst.flagDemo = true;
			GoLconst.demoCntr = 0;
			textField.setText("SS");
		}
		else if(  text.startsWith("ZEN ") )	//options from CITIZEN panel
		{	if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("Change Citizen attribute\n");
			if( text.equals("ZEN VISION MAX") )
			{
				GoLconst.VISION_MAX = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN VISION MAX = " + GoLconst.VISION_MAX + "\n");
			}
			else if( text.equals("ZEN COLOR SENIOR") )
			{
				GoLconst.CITIZEN_COLOR_SENIOR = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN COLOR SENIOR = "
										+ GoLconst.CITIZEN_COLOR_SENIOR + "\n");
			}
			else if( text.equals("ZEN COLOR CHILD") )
			{
				GoLconst.CITIZEN_COLOR_CHILD = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN COLOR CHILD = "
										+ GoLconst.CITIZEN_COLOR_CHILD + "\n");
			}
			else if( text.equals("ZEN SEX RATIO") )
			{
				GoLconst.SEX_RATIO = Float.parseFloat(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN SEX RATIO = " + GoLconst.SEX_RATIO + "\n");
			}
			else if( text.equals("ZEN PERSONALITY RATIO") )
			{
				GoLconst.PERSONALITY_RATIO = Float.parseFloat(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN PERSONALITY RATIO = "
										+ GoLconst.PERSONALITY_RATIO + "\n");
			}
			else if( text.equals("ZEN SUGAR MIN") )
			{
				GoLconst.SUGAR_MIN_CITIZEN = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN SUGAR MIN = "
										+ GoLconst.SUGAR_MIN_CITIZEN + "\n");
			}
			else if( text.equals("ZEN SUGAR MAX") )
			{
				GoLconst.SUGAR_MAX_CITIZEN = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN SUGAR MAX = "
										+ GoLconst.SUGAR_MAX_CITIZEN + "\n");
			}
			else if( text.equals("ZEN METABOLISM MAX SUGAR") )
			{
				GoLconst.METABOLISM_MAX_SUGAR = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN METABOLISM MAX SUGAR = "
										+ GoLconst.METABOLISM_MAX_SUGAR + "\n");
			}
			else if( text.equals("ZEN SPICE MIN") )
			{
				GoLconst.SPICE_MIN_CITIZEN = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN SPICE MIN = "
										+ GoLconst.SPICE_MIN_CITIZEN + "\n");
			}
			else if( text.equals("ZEN SPICE MAX") )
			{
				GoLconst.SPICE_MAX_CITIZEN = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN SPICE MAX = "
										+ GoLconst.SPICE_MAX_CITIZEN + "\n");
			}
			else if( text.equals("ZEN METABOLISM MAX SPICE") )
			{
				GoLconst.METABOLISM_MAX_SPICE = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN METABOLISM MAX SPICE = "
										+ GoLconst.METABOLISM_MAX_SPICE + "\n");
			}
			else if( text.equals("ZEN LIFE EXPECTANCY MIN") )
			{
				GoLconst.LIFE_EXPECTANCY_MIN = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN LIFE EXPECTANCY MIN = "
										+ GoLconst.LIFE_EXPECTANCY_MIN + "\n");
			}
			else if( text.equals("ZEN LIFE EXPECTANCY MAX") )
			{
				GoLconst.LIFE_EXPECTANCY_MAX = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN LIFE EXPECTANCY MAX = "
										+ GoLconst.LIFE_EXPECTANCY_MAX + "\n");
			}
			else if( text.equals("ZEN SUGAR LEVEL POOR") )
			{
				GoLconst.SUGAR_LEVEL_POOR = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN SUGAR LEVEL POOR = "
										+ GoLconst.SUGAR_LEVEL_POOR + "\n");
			}
			else if( text.equals("ZEN SPICE LEVEL POOR") )
			{
				GoLconst.SPICE_LEVEL_POOR = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN SPICE LEVEL POOR = "
										+ GoLconst.SPICE_LEVEL_POOR + "\n");
			}
			else if( text.equals("ZEN MATING MALE MIN") )
			{
				GoLconst.MATING_MALE_MIN = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN MATING MALE MIN = "
										+ GoLconst.MATING_MALE_MIN + "\n");
			}
			else if( text.equals("ZEN MATING MALE MAX") )
			{
				GoLconst.MATING_MALE_MAX = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN MATING MALE MAX = "
										+ GoLconst.MATING_MALE_MAX + "\n");
			}
			else if( text.equals("ZEN MATING MALE GAP") )
			{
				GoLconst.MATING_MALE_GAP = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN MATING MALE GAP = "
										+ GoLconst.MATING_MALE_GAP + "\n");
			}
			else if( text.equals("ZEN MATING FEMALE MIN") )
			{
				GoLconst.MATING_FEMALE_MIN = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN MATING FEMALE MIN = "
										+ GoLconst.MATING_FEMALE_MIN + "\n");
			}
			else if( text.equals("ZEN MATING FEMALE MAX") )
			{
				GoLconst.MATING_FEMALE_MAX = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN MATING FEMALE MAX = "
										+ GoLconst.MATING_FEMALE_MAX + "\n");
			}
			else if( text.equals("ZEN MATING FEMALE GAP") )
			{
				GoLconst.MATING_FEMALE_GAP = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN MATING FEMALE GAP = "
										+ GoLconst.MATING_FEMALE_GAP + "\n");
			}

			else if( text.equals("ZEN COLOR TYPE") )
			{
				{	short newVal = (short)Integer.parseInt(param);
					if(newVal >= 1 && newVal <= 3)
						GoLconst.CITIZEN_COLOR = newVal;
					else
						textArea.append("Color must be 1(food reserves), 2(cultural group) "
											+ "or 3(disease cost)!\n");
				}
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN COLOR TYPE = "	+ GoLconst.CITIZEN_COLOR + "\n");
			}
			else if( text.equals("ZEN TAG LENGTH") )
			{
				if(GoLconst.flagSugarscape)
					textArea.append("ZEN TAG LENGTH: Cannot be changed with Sugarscape active!\n" +						"Select 'Clear' from the template menu, then enter new tag length\n");
				else
				{	short newVal = (short)Integer.parseInt(param);
					if(newVal >= 1 && newVal <= 30)
						GoLconst.CITIZEN_CULTURE_TAG = newVal;
					else
						textArea.append("Tag length must be 1 - 30 chrs!\n");
				}
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN TAG LENGTH = "
										+ GoLconst.CITIZEN_CULTURE_TAG + "\n");
			}
			else if( text.equals("ZEN TAG DIVIDER") )
			{
				short newVal = (short)Integer.parseInt(param);
				if(newVal >= 1 && newVal <= GoLconst.CITIZEN_CULTURE_TAG)
					GoLconst.CITIZEN_CULTURE_DIVIDE = newVal;
				else
					textArea.append("Tag divider must be > 1 && < CITIZEN CULTURE TAG!\n");
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN TAG DIVIDER = "
										+ GoLconst.CITIZEN_CULTURE_DIVIDE + "\n");
			}
			else if( text.equals("ZEN TAG FLIP TYPE") )
			{
				short newVal = (short)Integer.parseInt(param);
				if(newVal >= 1 && newVal <= 3)
					GoLconst.CITIZEN_CULTURE_TRANSFER = newVal;
				else
					textArea.append("Tag flip type must be >= 1 && <= 3!\n");
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN TAG FLIP TYPE = "
										+ GoLconst.CITIZEN_CULTURE_TRANSFER + "\n");
			}
			else if( text.equals("ZEN MIN STICKY FLIP") )
			{
				short newVal = (short)Integer.parseInt(param);
				if(newVal >= 1 && newVal <= 8)
					GoLconst.CITIZEN_CULTURE_OVERWHELM = newVal;
				else
					textArea.append("Minimum neighbors required to flip stick tags range from 1 - 8!\n");
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN MIN STICKY FLIP = "
										+ GoLconst.CITIZEN_CULTURE_OVERWHELM + "\n");
			}
			else if( text.startsWith("ZEN GRP NAME ") )
			{
				short fieldNum = (short)Integer.parseInt(text.substring(
						"ZEN GRP NAME ".length()));
				GoLconst.CITIZEN_GROUP_NAME[fieldNum] = param;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN GRP NAME #" + (fieldNum+1) + " = "
										+ GoLconst.CITIZEN_GROUP_NAME[fieldNum] + "\n");
				updateScorePanel();
			}
			else if( text.startsWith("ZEN GRP COLOR ") )
			{
				short fieldNum = (short)Integer.parseInt(text.substring(
						"ZEN GRP COLOR ".length()));
				GoLconst.CITIZEN_GROUP_COLOR[fieldNum] = (short)Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN GRP COLOR #" + (fieldNum+1) + " = "
										+ GoLconst.CITIZEN_GROUP_COLOR[fieldNum] + "\n");
			}
			else if( text.startsWith("ZEN GRP INTERVAL ") )
			{
				short fieldNum = (short)Integer.parseInt(text.substring(
						"ZEN GRP INTERVAL ".length()));
				short newVal = (short)Integer.parseInt(param);
				
				short offset = (short)(fieldNum + 1);
				if( offset == GoLconst.MAX_GROUPS )
					GoLconst.CITIZEN_GROUP_INTERVAL[fieldNum] = GoLconst.CITIZEN_CULTURE_TAG;
				else
				{
					short minVal = fieldNum == 0 ? 0 : GoLconst.CITIZEN_GROUP_INTERVAL[fieldNum-1];
					short maxVal = GoLconst.CITIZEN_GROUP_INTERVAL[offset];
					// value can range from 
					if( newVal > minVal && newVal <= maxVal )
						GoLconst.CITIZEN_GROUP_INTERVAL[fieldNum] = newVal;
				}
				ciGroupInterval[fieldNum].setText(
							Integer.toString(GoLconst.CITIZEN_GROUP_INTERVAL[fieldNum]));
				CellSpace.setDistrGroupsInit();
				updateCitizenPanel();
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN GRP INTERVAL " + fieldNum + " = "
						+ GoLconst.CITIZEN_GROUP_INTERVAL[fieldNum] + "\n");
			}

			else if( text.equals("ZEN MATING KIN ALLOW") )
			{	if(param.equals(Boolean.toString(!GoLconst.MATING_KIN_ALLOW)))
					GoLconst.MATING_KIN_ALLOW = !GoLconst.MATING_KIN_ALLOW;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN MATING KIN ALLOW = "
										+ GoLconst.MATING_KIN_ALLOW + "\n");
			}
			else if( text.equals("ZEN INHERIT INITIAL WEALTH") )
			{	if(param.equals(Boolean.toString(!GoLconst.INHERIT_INITIAL_WEALTH)))
					GoLconst.INHERIT_INITIAL_WEALTH = !GoLconst.INHERIT_INITIAL_WEALTH;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN INHERIT INITIAL WEALTH = "
										+ GoLconst.INHERIT_INITIAL_WEALTH + "\n");
			}
			else if( text.equals("ZEN CREATE UNIQUE SURNAMES") )
			{	if(param.equals(Boolean.toString(!GoLconst.CREATE_UNIQUE_SURNAMES)))
					GoLconst.CREATE_UNIQUE_SURNAMES = !GoLconst.CREATE_UNIQUE_SURNAMES;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN CREATE UNIQUE SURNAMES = "
										+ GoLconst.CREATE_UNIQUE_SURNAMES + "\n");
			}
			else if( text.equals("ZEN INHERIT FAMILY FATHER") )
			{	if(param.equals(Boolean.toString(!GoLconst.INHERIT_FAMILY_FATHER)))
					GoLconst.INHERIT_FAMILY_FATHER = !GoLconst.INHERIT_FAMILY_FATHER;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN INHERIT FAMILY FATHER = "
										+ GoLconst.INHERIT_FAMILY_FATHER + "\n");
			}
			else if( text.equals("ZEN DISEASE COUNT INITIAL") )
			{	
				short newVal = (short)Integer.parseInt(param);
				if(newVal >= 1 && newVal <= GoLconst.CELLSPACE_DISEASE_COUNT)
					GoLconst.DISEASE_COUNT_INITIAL = newVal;
				else
					textArea.append("Tag divider must be > 1 && < CELLSPACE DISEASE COUNT!\n");
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN DISEASE COUNT INITIAL = "
										+ GoLconst.DISEASE_COUNT_INITIAL + "\n");
			}
			else if( text.equals("ZEN IMMUNE STRING") )
			{	
				if(GoLconst.flagSugarscape)
					textArea.append("ZEN IMMUNE STRING: Cannot be changed with Sugarscape active!\n" +
						"Select 'Clear' from the template menu, then enter new length\n");
				else
				{	short newVal = (short)Integer.parseInt(param);
					if(newVal >= 1 && newVal < 63)	//63 bits - largest num a long type will allow
						GoLconst.CITIZEN_IMMUNE_STRING = newVal;
					else
						textArea.append("Tag divider must be > 1 && < CITIZEN IMMUNE STRING!\n");
				}
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("ZEN IMMUNE STRING = "
										+ GoLconst.CITIZEN_IMMUNE_STRING + "\n");
			}

			updateCitizenPanel();
			GoLconst.chkGoLconstants();
		}
		else if(  text.startsWith("GRIDCELL ") )	//options from CITIZEN panel
		{	if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("Change Cell attribute\n");
			if( text.equals("GRIDCELL BG") )
			{
				GoLconst.CELL_BG_SUMMER = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("GRIDCELL BG = " + GoLconst.CELL_BG_SUMMER + "\n");
			}
			else if( text.equals("GRIDCELL BAR") )
			{
				GoLconst.CELL_BAR = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("GRIDCELL BAR = " + GoLconst.CELL_BAR + "\n");
			}
			else if( text.equals("GRIDCELL 3D") )
			{
				if(param.equals(Boolean.toString(!GoLconst.CELL_3D)))
					GoLconst.CELL_3D = !GoLconst.CELL_3D;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("GRIDCELL 3D = " + GoLconst.CELL_3D + "\n");
			}
			else if( text.equals("GRIDCELL LIMIT SUGAR") )
			{
				if(param.equals(Boolean.toString(!GoLconst.LIMIT_CELL_SUGAR)))
					GoLconst.LIMIT_CELL_SUGAR = !GoLconst.LIMIT_CELL_SUGAR;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("GRIDCELL LIMIT SUGAR = "
										+ GoLconst.LIMIT_CELL_SUGAR + "\n");
			}
			else if( text.equals("GRIDCELL SUGAR MAX") )
			{
				GoLconst.SUGAR_MAX_CELL = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("GRIDCELL SUGAR MAX = "
										+ GoLconst.SUGAR_MAX_CELL + "\n");
			}
			else if( text.equals("GRIDCELL SUGAR RENEW SUMMER") )
			{
				GoLconst.SUGAR_RENEW_SUMMER = Float.parseFloat(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("GRIDCELL SUGAR RENEW SUMMER = "
										+ GoLconst.SUGAR_RENEW_SUMMER + "\n");
			}
			else if( text.equals("GRIDCELL SUGAR RENEW WINTER") )
			{
				GoLconst.SUGAR_RENEW_WINTER = Float.parseFloat(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("GRIDCELL SUGAR RENEW WINTER = "
										+ GoLconst.SUGAR_RENEW_WINTER + "\n");
			}
			else if( text.equals("GRIDCELL LIMIT SPICE") )
			{
				if(param.equals(Boolean.toString(!GoLconst.LIMIT_CELL_SPICE)))
					GoLconst.LIMIT_CELL_SPICE = !GoLconst.LIMIT_CELL_SPICE;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("GRIDCELL LIMIT SPICE = "
										+ GoLconst.LIMIT_CELL_SPICE + "\n");
			}
			else if( text.equals("GRIDCELL SPICE MAX") )
			{
				GoLconst.SPICE_MAX_CELL = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("GRIDCELL SPICE MAX = "
										+ GoLconst.SPICE_MAX_CELL + "\n");
			}
			else if( text.equals("GRIDCELL SPICE RENEW SUMMER") )
			{
				GoLconst.SPICE_RENEW_SUMMER = Float.parseFloat(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("GRIDCELL SPICE RENEW SUMMER = "
										+ GoLconst.SPICE_RENEW_SUMMER + "\n");
			}
			else if( text.equals("GRIDCELL SPICE RENEW WINTER") )
			{
				GoLconst.SPICE_RENEW_WINTER = Float.parseFloat(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("GRIDCELL SPICE RENEW WINTER = "
										+ GoLconst.SPICE_RENEW_WINTER + "\n");
			}
			else if( text.equals("GRIDCELL POLLUTION PRODUCTION SUGAR") )
			{
				GoLconst.POLLUTION_PRODUCTION_SUGAR = Float.parseFloat(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("GRIDCELL POLLUTION PRODUCTION SUGAR = "
										+ GoLconst.POLLUTION_PRODUCTION_SUGAR + "\n");
			}
			else if( text.equals("GRIDCELL POLLUTION PRODUCTION SPICE") )
			{
				GoLconst.POLLUTION_PRODUCTION_SPICE = Float.parseFloat(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("GRIDCELL POLLUTION PRODUCTION SPICE = "
										+ GoLconst.POLLUTION_PRODUCTION_SPICE + "\n");
			}
			else if( text.equals("GRIDCELL POLLUTION DISPERSION UNIT") )
			{
				GoLconst.POLLUTION_DISPERSION_UNIT = Float.parseFloat(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("GRIDCELL POLLUTION DISPERSION UNIT = "
										+ GoLconst.POLLUTION_DISPERSION_UNIT + "\n");
			}
			GoLconst.chkGoLconstants();			
		}
		else if(  text.startsWith("CSPACE ") )	//options from CellSpace panel
		{	if( GoLconst.DEBUG_CMD_FEEDBACK )
				textArea.append("Change Cellspace attribute\n");
			if( text.equals("CSPACE COLUMNS") )
			{
				if(  !GoLconst.flagSugarscape )
				{	textArea.append("Resize permitted in Sugarscape only!\n");
					return;
				}
				int cellColsOld = cellCols;
				cellCols = Integer.parseInt(param);
				if( cellCols > GoLconst.GRID_COLS_MAX )
					cellCols = GoLconst.GRID_COLS_MAX;
					
				//cellCols & cellRows are seperate vars in Sugarscape.java &
				//CellSpace.java, both need to be updated
				cellSpace.resizeCols( cellCols );
				//If new cells have been added, create them
				if( cellColsOld < cellCols )
					cellSpace.createCells();
				if( GoLconst.timePeriod == 0 )
					drawSugarscape(cellCols, cellRows);	//regenerate Sugarscape
				else 
				{	int orphans = cellSpace.deleteOrphanCitizens(cellCols, cellRows);
					if( GoLconst.DEBUG || GoLconst.DEBUG_CITIZEN_DEATH ||
						GoLconst.DEBUG_CMD_FEEDBACK )
					{	textArea.append(orphans + " citizens eliminated\n"); }
				}
				//resizes the window to accomodate added row or columns
				resize(this.cellCols * this.cellSize, GoLconst.CSPACE_OFFSET + 
					((cellRows) * cellSize) + (int)(GoLconst.TEXTAREA_HEIGHT * 16.67) );
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE COLUMNS = " + GoLconst.GRID_COLUMNS + "\n");
			}
			else if( text.equals("CSPACE ROWS") )
			{
				if(  !GoLconst.flagSugarscape )
				{	textArea.append("Resize permitted in Sugarscape only!\n");
					return;
				}
				int cellRowsOld = cellRows;
				cellRows = Integer.parseInt(param);
				if( cellRows > GoLconst.GRID_ROWS_MAX )
					cellRows = GoLconst.GRID_ROWS_MAX;
					
				//cellCols & cellRows are seperate vars in Sugarscape.java &
				//CellSpace.java, both need to be updated
				//cellSpace.resizeCols( cellCols );
				cellSpace.resizeRows( cellRows );
				//If new cells have been added, create them
				int value = cellRows - cellRowsOld;
				if( value > 0 )
					cellSpace.createCells();
				if( GoLconst.timePeriod == 0 )
					drawSugarscape(cellCols, cellRows);	//regenerate Sugarscape
				else 
				{	int orphans = cellSpace.deleteOrphanCitizens(cellCols, cellRows);
					if( GoLconst.DEBUG || GoLconst.DEBUG_CITIZEN_DEATH ||
						GoLconst.DEBUG_CMD_FEEDBACK )
					{	textArea.append(orphans + " citizens eliminated\n"); }
				}
				//resizes the window to accomodate added rows
				resize(this.cellCols * this.cellSize, GoLconst.CSPACE_OFFSET + 
					((cellRows) * cellSize) + (int)(GoLconst.TEXTAREA_HEIGHT * 16.67) );
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE ROWS = " + GoLconst.GRID_ROWS + "\n");
			}
			else if( text.equals("CSPACE DENSITY FACTOR") )
			{
				GoLconst.DENSITY_FACTOR = Float.parseFloat(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE DENSITY FACTOR = "
										+ GoLconst.DENSITY_FACTOR + "\n");
			}
			else if( text.equals("CSPACE COORD SHOW") )
			{	if(param.equals(Boolean.toString(!GoLconst.GRID_COORD_SHOW)))
					GoLconst.GRID_COORD_SHOW = !GoLconst.GRID_COORD_SHOW;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE COORD SHOW = "
										+ GoLconst.GRID_COORD_SHOW + "\n");
			}
			else if( text.equals("CSPACE COORD DETAIL") )
			{
				GoLconst.GRID_COORD_DETAIL = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE COORD DETAIL = "
										+ GoLconst.GRID_COORD_DETAIL + "\n");
			}
			else if( text.equals("CSPACE COORD FONT") )
			{
				GoLconst.GRID_COORD_FONT = Float.parseFloat(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE COORD FONT = "
										+ GoLconst.GRID_COORD_FONT + "\n");
			}
			else if( text.equals("CSPACE COORD COLOR") )
			{
				GoLconst.GRID_COORD_COLOR = param;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE COORD COLOR = "
										+ GoLconst.GRID_COORD_COLOR + "\n");
			}
			else if( text.equals("CSPACE TEXTAREA HEIGHT") )
			{
				GoLconst.TEXTAREA_HEIGHT = Integer.parseInt(param);
				if( GoLconst.TEXTAREA_HEIGHT > GoLconst.TEXTAREA_MAX )
					GoLconst.TEXTAREA_HEIGHT = GoLconst.TEXTAREA_MAX;
				else if( GoLconst.TEXTAREA_HEIGHT < GoLconst.TEXTAREA_MIN )
					GoLconst.TEXTAREA_HEIGHT = GoLconst.TEXTAREA_MIN;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("TEXTAREA dimensions = " + textArea.getRows() 
								+ " rows by " + textArea.getColumns() + " cols\n");
				resize(this.cellCols * this.cellSize, GoLconst.CSPACE_OFFSET + 
					((cellRows) * cellSize) + (int)(GoLconst.TEXTAREA_HEIGHT * 16.67) );
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE TEXTAREA HEIGHT = "
										+ GoLconst.TEXTAREA_HEIGHT + "\n");
			}
			else if( text.equals("CSPACE INITIATE GATHER") )
			{	if(param.equals(Boolean.toString(!GoLconst.INITIATE_GATHER)))
					GoLconst.INITIATE_GATHER = !GoLconst.INITIATE_GATHER;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE INITIATE GATHER = "
										+ GoLconst.INITIATE_GATHER + "\n");
			}
			else if( text.equals("CSPACE INITIATE MATING") )
			{	if(param.equals(Boolean.toString(!GoLconst.INITIATE_MATING)))
					GoLconst.INITIATE_MATING = !GoLconst.INITIATE_MATING;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE INITIATE MATING = "
										+ GoLconst.INITIATE_MATING + "\n");
			}
			else if( text.equals("CSPACE INITIATE BARTER") )
			{	if(param.equals(Boolean.toString(!GoLconst.INITIATE_BARTER)))
					GoLconst.INITIATE_BARTER = !GoLconst.INITIATE_BARTER;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE INITIATE BARTER = "
										+ GoLconst.INITIATE_BARTER + "\n");
			}
			else if( text.equals("CSPACE INITIATE POLLUTION") )
			{	if(param.equals(Boolean.toString(!GoLconst.INITIATE_POLLUTION)))
					GoLconst.INITIATE_POLLUTION = !GoLconst.INITIATE_POLLUTION;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE INITIATE POLLUTION = "
										+ GoLconst.INITIATE_POLLUTION + "\n");
			}
			else if( text.equals("CSPACE INITIATE SEASONS") )
			{	if(param.equals(Boolean.toString(!GoLconst.INITIATE_SEASONS)))
					GoLconst.INITIATE_SEASONS = !GoLconst.INITIATE_SEASONS;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE INITIATE SEASONS = "
										+ GoLconst.INITIATE_SEASONS + "\n");
			}
			else if( text.equals("CSPACE INITIATE INHERITANCE") )
			{	if(param.equals(Boolean.toString(!GoLconst.INITIATE_INHERITANCE)))
					GoLconst.INITIATE_INHERITANCE = !GoLconst.INITIATE_INHERITANCE;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE INITIATE INHERITANCE = "
										+ GoLconst.INITIATE_INHERITANCE + "\n");
			}
			else if( text.equals("CSPACE INITIATE CULTURE") )
			{	if(param.equals(Boolean.toString(!GoLconst.INITIATE_CULTURE)))
					GoLconst.INITIATE_CULTURE = !GoLconst.INITIATE_CULTURE;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE INITIATE CULTURE = "
										+ GoLconst.INITIATE_CULTURE + "\n");
			}
			else if( text.equals("CSPACE INITIATE DISEASE") )
			{	if(param.equals(Boolean.toString(!GoLconst.INITIATE_DISEASE)))
					GoLconst.INITIATE_DISEASE = !GoLconst.INITIATE_DISEASE;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE INITIATE DISEASE = "
										+ GoLconst.INITIATE_DISEASE + "\n");
			}
//			else if( text.equals("CSPACE INITIATE COMBAT") )
//			{	if(param.equals(Boolean.toString(!GoLconst.INITIATE_COMBAT)))
//					GoLconst.INITIATE_COMBAT = !GoLconst.INITIATE_COMBAT;
//				if( GoLconst.DEBUG_CMD_FEEDBACK )
//					textArea.append("CSPACE INITIATE COMBAT = "
//										+ GoLconst.INITIATE_COMBAT + "\n");
//			}

			else if( text.equals("CSPACE FERTILITY SUGAR") )
			{
				GoLconst.GRID_FERTILITY_SUGAR = Float.parseFloat(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE FERTILITY SUGAR = "
										+ GoLconst.GRID_FERTILITY_SUGAR + "\n");
			}
			else if( text.equals("CSPACE FERTILITY SPICE") )
			{
				GoLconst.GRID_FERTILITY_SPICE = Float.parseFloat(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE FERTILITY SPICE = "
										+ GoLconst.GRID_FERTILITY_SPICE + "\n");
			}
			else if( text.equals("CSPACE SEASON DURATION") )
			{
				GoLconst.SEASON_DURATION = Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE SEASON DURATION = "
										+ GoLconst.SEASON_DURATION + "\n");
			}
			else if( text.equals("CSPACE DISEASE COUNT") )
			{
				GoLconst.CELLSPACE_DISEASE_COUNT = (short)Integer.parseInt(param);
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE DISEASE COUNT = "
										+ GoLconst.CELLSPACE_DISEASE_COUNT + "\n");
			}
			else if( text.equals("CSPACE DISEASE SIZE MIN") )
			{	
				short newVal = (short)Integer.parseInt(param);
				if(newVal >= 1 && newVal < GoLconst.DISEASE_SIZE_MAX)	//63 bits - largest num a long type will allow
					GoLconst.DISEASE_SIZE_MIN = newVal;
				else
					textArea.append("Tag divider must be > 1 && < DISEASE SIZE MIN!\n");
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE DISEASE SIZE MIN = "
										+ GoLconst.DISEASE_SIZE_MIN + "\n");
			}
			else if( text.equals("CSPACE DISEASE SIZE MAX") )
			{	
				short newVal = (short)Integer.parseInt(param);
				if(newVal > GoLconst.DISEASE_SIZE_MIN && newVal < GoLconst.CITIZEN_IMMUNE_STRING)	//63 bits - largest num a long type will allow
					GoLconst.DISEASE_SIZE_MAX = newVal;
				else
					textArea.append(
						"Tag divider must be > DISEASE SIZE MIN && < CITIZEN IMMUNE STRING!\n");
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE DISEASE SIZE MAX = "
										+ GoLconst.DISEASE_SIZE_MAX + "\n");
			}
			
			
			
			else if( text.equals("CSPACE DISEASE COST SUGAR") )
			{	
				float newVal = Float.parseFloat(param);
				GoLconst.DISEASE_COST_SUGAR = newVal;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE DISEASE COST SUGAR = "
										+ GoLconst.DISEASE_COST_SUGAR + "\n");
			}
			else if( text.equals("CSPACE DISEASE COST SPICE") )
			{	
				float newVal = Float.parseFloat(param);
				GoLconst.DISEASE_COST_SPICE = newVal;
				if( GoLconst.DEBUG_CMD_FEEDBACK )
					textArea.append("CSPACE DISEASE COST SPICE = "
										+ GoLconst.DISEASE_COST_SPICE + "\n");
			}
			
			GoLconst.chkGoLconstants();			
		}
		else
		{	if( GoLconst.DEBUG_CMD_FEEDBACK )
			{
				Toolkit.getDefaultToolkit().beep();
				textArea.append(text + "?\n");
			}
		}
		textField.selectAll();
		updateScorePanel();
		cellSpace.repaint();
		setVisible(true);
		showGenerations();
	}
	
	public static String filler(int size)
	{
		String Filler = " ";
		while( --size > 0 )
			Filler = Filler + " ";
		return Filler;
	}
	
	public JComboBox createTemplateList()
	{
		c = new JComboBox();
		c.addItem(clear);
		c.addItem(sugarscape);
		c.addItem(random);
		c.addItem(glider);
		c.addItem(bigbang);
		c.addItem(exploder2);
		c.addItem(exploder1);
		c.addItem(row10);
		c.addItem(fish);
		c.addItem(pump);
		c.addItem(gun);
		c.addActionListener(new ControlChoice(this));
		return c;
	}

	public JComboBox createSpeedList()
	{
		speed = new JComboBox();
		speed.addItem(hyper);
		speed.addItem(fast);
		speed.addItem(slow);
		speed.addActionListener(new ControlSpeed(this));
		return speed;
	}

	public void createScorePanel()
	{
		int population = cellSpace.getPopulation();
		population = (population == 0) ? 1 : population;
				
		scDensity = new JTextField(3);
		scDensity.setEditable(false);
		scDensity.setToolTipText("Current population density");
		scBirths = new JTextField(3);
		scBirths.setEditable(false);
		scBirths.setToolTipText("Children born since timeperiod zero");
		scPerCycle = new JTextField(3);
		scPerCycle.setEditable(false);
		scPerCycle.setToolTipText("Average children born per cycle");
		scDeaths = new JTextField(3);
		scDeaths.setEditable(false);
		scDeaths.setToolTipText("Death count since timeperiod zero");
		scStarved = new JTextField(3);
		scStarved.setEditable(false);
		scStarved.setToolTipText("Death by starving since timeperiod zero");
		scOldAge = new JTextField(3);
		scOldAge.setEditable(false);			
		scOldAge.setToolTipText("Death by old age since timeperiod zero");

		scAvgVision = new JTextField(3);
		scAvgVision.setToolTipText("Average citizen vision");
		scAvgVision.setEditable(false);
		scAvgSugar = new JTextField(7);
		scAvgSugar.setToolTipText("Average sugar collected (avg metabolism) per citizen");
		scAvgSugar.setEditable(false);
		scAvgSuMetab = new JTextField(3);
		//scAvgSuMetab.setToolTipText("Average sugar metabolism");
		scAvgSuMetab.setEditable(false);
		scAvgSpice = new JTextField(7);
		scAvgSpice.setToolTipText("Average spice collected (avg metabolism) per citizen");
		scAvgSpice.setEditable(false);
		scAvgSpMetab = new JTextField(3);
		//scAvgSpMetab.setToolTipText("Average spice metabolism");
		scAvgSpMetab.setEditable(false);
		scAvgLifespan = new JTextField(2);
		scAvgLifespan.setToolTipText("Average lifespan");
		scAvgLifespan.setEditable(false);
		scAvgAge = new JTextField(2);
		scAvgAge.setToolTipText("Average age");
		scAvgAge.setEditable(false);
		scDistSummer = new JTextField(7);
		scDistSummer.setToolTipText("Percentage of population contained in summer/winter region");
		scDistSummer.setEditable(false);
		scDistWinter = new JTextField(4);
		//scDistWinter.setToolTipText("Percentage of population contained in winter region");
		scDistWinter.setEditable(false);
		scDistMale = new JTextField(6);
		scDistMale.setToolTipText("Percentage of males/females in population");
		scDistMale.setEditable(false);
		scDistFemale = new JTextField(4);
		//scDistFemale.setToolTipText("Percentage of females in population");
		scDistFemale.setEditable(false);
		scDistChildren = new JTextField(5);
		scDistChildren.setToolTipText("Percentage of children(pre-puberty) in population");
		scDistChildren.setEditable(false);
		scDistAdults = new JTextField(5);
		scDistAdults.setToolTipText("Percentage of adults in population");
		scDistAdults.setEditable(false);
		scDistSeniors = new JTextField(5);
		scDistSeniors.setToolTipText("Percentage of seniors in population");
		scDistSeniors.setEditable(false);
		scRiskTaker = new JTextField(5);
		scRiskTaker.setToolTipText("Percentage of bulls (trade-maximizers) in population");
		//scRiskTaker.setToolTipText("Seeks to execute maximum trades for unit quantities");
		scRiskTaker.setEditable(false);
		scRiskAverse = new JTextField(5);
		scRiskAverse.setToolTipText("Percentage of bears (safe traders) in population");
		scRiskAverse.setEditable(false);
			
		scHyp = new JTextField(4);
		scHyp.setToolTipText("% population with High vision, High sugar & High spice attributes");
		scHyp.setEditable(false);
		scHypSu = new JTextField(4);
		scHypSu.setToolTipText("% population with High vision, High sugar & Low spice attributes");
		scHypSu.setEditable(false);
		scHypSp = new JTextField(4);
		scHypSp.setToolTipText("% population with High vision, Low sugar & High spice attributes");
		scHypSp.setEditable(false);
		scEff = new JTextField(4);
		scEff.setToolTipText("% population with High vision, Low sugar & Low spice attributes");
		scEff.setEditable(false);
		scInEff = new JTextField(4);
		scInEff.setToolTipText("% population with Low vision, High sugar & High spice attributes");
		scInEff.setEditable(false);
		scSlow = new JTextField(4);
		scSlow.setToolTipText("% population with Low vision, Low sugar & Low spice attributes");
		scSlow.setEditable(false);
		scSlowSu = new JTextField(4);
		scSlowSu.setToolTipText("% population with Low vision, High sugar & Low spice attributes");
		scSlowSu.setEditable(false);
		scSlowSp = new JTextField(4);
		scSlowSp.setToolTipText("% population with Low vision, Low sugar & High spice attributes");
		scSlowSp.setEditable(false);
			
		scProcGather = new JTextField(4);
		scProcGather.setToolTipText("Time(seconds) used by gathering processes");
		scProcGather.setEditable(false);
		scProcMate = new JTextField(4);
		scProcMate.setToolTipText("Time(seconds) used by mating processes");
		scProcMate.setEditable(false);
		scProcTrade = new JTextField(4);
		scProcTrade.setToolTipText("Time(seconds) used by trading processes");
		scProcTrade.setEditable(false);
		scProcMisc = new JTextField(4);
		scProcMisc.setToolTipText("Time(seconds) used by misc processes");
		scProcMisc.setEditable(false);
		
		JPanel sc0Panel = new JPanel(new FlowLayout());
		sc0Panel.setPreferredSize(new Dimension(580,30));
		sc0Panel.setBorder(blackline);
		scorePanel.add(sc0Panel);
		
		JPanel sc1Panel = new JPanel(new FlowLayout());
		sc1Panel.setPreferredSize(new Dimension(580,30));
		sc1Panel.setBorder(blackline);
		scorePanel.add(sc1Panel);
		
		JPanel sc2Panel = new JPanel(new FlowLayout());
		sc2Panel.setPreferredSize(new Dimension(580,30));
		sc2Panel.setBorder(blackline);
		scorePanel.add(sc2Panel);
		
		JPanel sc3Panel = new JPanel(new GridLayout(2,4));
		sc3Panel.setPreferredSize(new Dimension(580,44));
		sc3Panel.setBorder(blackline);
		scorePanel.add(sc3Panel);
		
		JPanel sc4Panel = new JPanel(new FlowLayout());
		sc4Panel.setPreferredSize(new Dimension(200,30));
		sc4Panel.setBorder(blackline);
		scorePanel.add(sc4Panel);
		
		JPanel sc5Panel = new JPanel(new FlowLayout());
		sc5Panel.setPreferredSize(new Dimension(378,30));
		sc5Panel.setBorder(blackline);
		scorePanel.add(sc5Panel);
		
		JPanel sc6Panel = new JPanel(new FlowLayout());
		sc6Panel.setPreferredSize(new Dimension(580,30));
		sc6Panel.setBorder(blackline);
		scorePanel.add(sc6Panel);

		gCount = GoLconst.groupCount();
		scGrp = new JTextField[gCount];
		scGrpName = new JLabel[gCount];
		for(int i=0; i<cellSpace.groupCount; i++)
		{	scGrpName[i] = new JLabel(GoLconst.CITIZEN_GROUP_NAME[i]);
			scGrp[i] = new JTextField(5);
			scGrp[i].setToolTipText("Group " + (i+1));
			scGrp[i].setEditable(false);
			sc6Panel.add(scGrpName[i]);
			sc6Panel.add(scGrp[i]);
		}
		
		sc0Panel.add( new JLabel("Density"));
		sc0Panel.add(scDensity);
		sc0Panel.add(new JLabel("Births"));
		sc0Panel.add(scBirths);
		sc0Panel.add(new JLabel("PerCycle"));
		sc0Panel.add(scPerCycle);
		sc0Panel.add(new JLabel("Deaths"));
		sc0Panel.add(scDeaths);
		sc0Panel.add(new JLabel("Starved"));
		sc0Panel.add(scStarved);
		sc0Panel.add(new JLabel("Old Age"));
		sc0Panel.add(scOldAge);
		
		sc1Panel.add(new JLabel("Vision "));
		sc1Panel.add(scAvgVision);
		sc1Panel.add(new JLabel(" Su(Metab)"));
		sc1Panel.add(scAvgSugar);
		sc1Panel.add(new JLabel("Sp(Metab)"));
		sc1Panel.add(scAvgSpice);
		sc1Panel.add(new JLabel(" Life"));
		sc1Panel.add(scAvgLifespan);
		sc1Panel.add(new JLabel("Age"), c);
		sc1Panel.add(scAvgAge);

		sc2Panel.add(new JLabel("Sum'r/Winter"));
		sc2Panel.add(scDistSummer);
		sc2Panel.add(new JLabel("M/F"));
		sc2Panel.add(scDistMale);
		sc2Panel.add(new JLabel("Child"));
		sc2Panel.add(scDistChildren);
		sc2Panel.add(new JLabel("Adult"));
		sc2Panel.add(scDistAdults);
		sc2Panel.add(new JLabel("Senior"));
		sc2Panel.add(scDistSeniors);
		
		sc3Panel.add(new JLabel("  Hi"));
		sc3Panel.add(scHyp);
		sc3Panel.add(new JLabel(" HiSu"));
		sc3Panel.add(scHypSu);
		sc3Panel.add(new JLabel(" HiSp"));
		sc3Panel.add(scHypSp);
		sc3Panel.add(new JLabel("  Eff"));
		sc3Panel.add(scEff);
		sc3Panel.add(new JLabel(" InEff"));
		sc3Panel.add(scInEff);
		sc3Panel.add(new JLabel(" LowSu"));
		sc3Panel.add(scSlowSu);
		sc3Panel.add(new JLabel(" LowSp"));
		sc3Panel.add(scSlowSp);
		sc3Panel.add(new JLabel("  Low"));
		sc3Panel.add(scSlow);
		
		sc4Panel.add(new JLabel("Bull"));
		sc4Panel.add(scRiskTaker);
		sc4Panel.add(new JLabel("Bear"));
		sc4Panel.add(scRiskAverse);
		
		sc5Panel.add(new JLabel("Gather"));
		sc5Panel.add(scProcGather);
		sc5Panel.add(new JLabel("Mate"));
		sc5Panel.add(scProcMate);
		sc5Panel.add(new JLabel("Trade"));
		sc5Panel.add(scProcTrade);
		sc5Panel.add(new JLabel("Other"));
		sc5Panel.add(scProcMisc);
		
		updateScorePanel();		
	}	
	
	public void updateScorePanel()
	{	int population = cellSpace.getPopulation();
		population = (population == 0) ? 1 : population; 
		
		scDensity.setText(GoLconst.customFormat("##0%",
							(float)population/(cellSpace.getCellCols()*cellSpace.getCellRows()) ) );
		scBirths.setText(GoLconst.customFormat("###0",cellSpace.getBirthCount()) );
		scPerCycle.setText(GoLconst.customFormat("#0.00", 
								cellSpace.getBirthCount()/(GoLconst.timePeriod+1f) ));
		scDeaths.setText(GoLconst.customFormat("###0",
								cellSpace.getDeathByDerliction()+
								cellSpace.getDeathByDotage()+
								cellSpace.getDeathByStarvation()));
		scStarved.setText(GoLconst.customFormat("###0",
								cellSpace.getDeathByStarvation()));
		scOldAge.setText(GoLconst.customFormat("###0",
								cellSpace.getDeathByDotage()));
			
		scAvgVision.setText(GoLconst.customFormat("#0.00",
								((float)cellSpace.getTotVision()/(population))) );
		scAvgSugar.setText((GoLconst.customFormat("##0.00",
								((float)cellSpace.getTotSugar()/population))) + "(" +
								(GoLconst.customFormat("0.00",
									((float)cellSpace.getTotSuMetab()/population))) + ")" );
		scAvgSpice.setText(GoLconst.customFormat("##0.00",
								((float)cellSpace.getTotSpice()/population)) + "(" +
								GoLconst.customFormat("0.00",
									((float)cellSpace.getTotSpMetab()/population)) + ")" );
		scAvgLifespan.setText(GoLconst.customFormat("#00",
								(cellSpace.getTotLifespan()/population)) );
		scAvgAge.setText(GoLconst.customFormat("#00",
								(cellSpace.getTotAge()/population)) );
			
		scDistSummer.setText(GoLconst.customFormat("#0%",
								(float)cellSpace.getDistrSummer()/population) + " / "
								+ GoLconst.customFormat("#0%",
									(float)cellSpace.getDistrWinter()/population) );
		scDistMale.setText(GoLconst.customFormat("#0%",
								(float)cellSpace.getDistrMale()/population) + "/"
								+ GoLconst.customFormat("#0%",
									(float)cellSpace.getDistrFemale()/population) );
		scDistChildren.setText(GoLconst.customFormat("#0.00%",
								(float)cellSpace.getDistrChildren()/population));
		scDistAdults.setText(GoLconst.customFormat("#0.00%",
								(float)cellSpace.getDistrAdults()/population));
		scDistSeniors.setText(GoLconst.customFormat("#0.00%",
								(float)cellSpace.getDistrSeniors()/population));
		scRiskTaker.setText(GoLconst.customFormat("#0.00%",
								(float)cellSpace.getDistrRiskTaker()/population));
		scRiskAverse.setText(GoLconst.customFormat("#0.00%",
								(float)cellSpace.getDistrRiskAverse()/population));
			
		scHyp.setText(GoLconst.customFormat("#0.00%",
								(float)cellSpace.getDistrHyp()/population));
		scHypSu.setText(GoLconst.customFormat("#0.00%",
								(float)cellSpace.getDistrHypSu()/population));
		scHypSp.setText(GoLconst.customFormat("#0.00%",
								(float)cellSpace.getDistrHypSp()/population));
		scEff.setText(GoLconst.customFormat("#0.00%",
								(float)cellSpace.getDistrEff()/population));
		scInEff.setText(GoLconst.customFormat("#0.00%",
								(float)cellSpace.getDistrIneff()/population));
		scSlow.setText(GoLconst.customFormat("#0.00%",
								(float)cellSpace.getDistrSlow()/population));
		scSlowSu.setText(GoLconst.customFormat("#0.00%",
								(float)cellSpace.getDistrSlowSu()/population));
		scSlowSp.setText(GoLconst.customFormat("##0.00%",
								(float)cellSpace.getDistrSlowSp()/population));
			
		scProcGather.setText(GoLconst.customFormat("###0.00",CellSpace.getGatherTime()/1000f));
		scProcMate.setText(GoLconst.customFormat("###0.00",CellSpace.getMateTime()/1000f));
		scProcTrade.setText(GoLconst.customFormat("###0.00",CellSpace.getBarterTime()/1000f)); 
		scProcMisc.setText(GoLconst.customFormat("###0.00",CellSpace.getOtherTime()/1000f));
		
		gCount = GoLconst.groupCount();
		for(int i=0; i<gCount; i++)
		{	scGrpName[i].setText( GoLconst.CITIZEN_GROUP_NAME[i] );
			scGrp[i].setText( GoLconst.customFormat("##0.00%",
								(float)cellSpace.getRangeTally(i)/population) );
		}
	}

	public void createSscapePanel()
	{	
		ssCols = new JTextField(3);
		ssCols.setToolTipText("Number of columns on the sugarscape");
		ssCols.setActionCommand("CSPACE COLUMNS");
		ssCols.addActionListener(this);
		
		ssRows = new JTextField(3);
		ssRows.setToolTipText("Number of rows on the sugarscape");
		ssRows.setActionCommand("CSPACE ROWS");
		ssRows.addActionListener(this);
		
		ssMaxCols = new JTextField(3);
		ssMaxCols.setToolTipText("Maximum column space available on the sugarscape");
		ssMaxCols.setActionCommand("CSPACE COLS MAX");
		ssMaxCols.addActionListener(this);
		
		ssMaxRows = new JTextField(3);
		ssMaxRows.setToolTipText("Maximum row space available on the sugarscape");
		ssMaxRows.setActionCommand("CSPACE ROWS MAX");
		ssMaxRows.addActionListener(this);
		
		ssDensity = new JTextField(3);
		ssDensity.setToolTipText("Population density on the sugarscape");
		ssDensity.setActionCommand("CSPACE DENSITY FACTOR");
		ssDensity.addActionListener(this);
				
		ssCoordShow = new JTextField(2);
		ssCoordShow.setToolTipText("Switch controlling coordinates display");
		ssCoordShow.setActionCommand("CSPACE COORD SHOW");
		ssCoordShow.addActionListener(this);
		
		ssCoordDetail = new JTextField(2);
		ssCoordDetail.setToolTipText("Nature of coordinates display");
		ssCoordDetail.setActionCommand("CSPACE COORD DETAIL");
		ssCoordDetail.addActionListener(this);
		
		ssCoordSize = new JTextField(2);
		ssCoordSize.setToolTipText("Character size for coordinates display");
		ssCoordSize.setActionCommand("CSPACE COORD FONT");
		ssCoordSize.addActionListener(this);
		
		ssCoordColor = new JTextField(2);
		ssCoordColor.setToolTipText("Character color for coordinates display");
		ssCoordColor.setActionCommand("CSPACE COORD COLOR");
		ssCoordColor.addActionListener(this);
		
		ssPanelHeight = new JTextField(2);
		ssPanelHeight.setToolTipText("Size of display area");
		ssPanelHeight.setActionCommand("CSPACE TEXTAREA HEIGHT");
		ssPanelHeight.addActionListener(this);
		
		ssPanelMax = new JTextField(2);
		ssPanelMax.setToolTipText("Maximum size of display area");
		ssPanelMax.setActionCommand("CSPACE TEXTAREA MAX");
		ssPanelMax.setEditable(false);
		ssPanelMax.addActionListener(this);
		
		ssPanelMin = new JTextField(2);
		ssPanelMin.setToolTipText("Minimum size of display area");
		ssPanelMin.setActionCommand("CSPACE TEXTAREA MIN");
		ssPanelMin.setEditable(false);
		ssPanelMin.addActionListener(this);
	
		ssProcGather = new JTextField(2);
		ssProcGather.setActionCommand("CSPACE INITIATE GATHER");
		ssProcGather.setEditable(false);
		ssProcGather.setToolTipText("Gather process is essential & cannot be deactivated.");
		ssProcGather.addActionListener(this);
		
		ssProcMate = new JTextField(2);
		ssProcMate.setActionCommand("CSPACE INITIATE MATING");
		//ssProcMate.setEditable(false);
		ssProcMate.setToolTipText("Switch for mating processes.");
		ssProcMate.addActionListener(this);
		
		ssProcTrade = new JTextField(2);
		ssProcTrade.setActionCommand("CSPACE INITIATE BARTER");
		ssProcTrade.setToolTipText("Switch for barter processes.");
		ssProcTrade.addActionListener(this);
		
		ssProcSeasons = new JTextField(2);
		ssProcSeasons.setActionCommand("CSPACE INITIATE SEASONS");
		ssProcSeasons.setToolTipText("Switch for change of season processes.");
		ssProcSeasons.addActionListener(this);
		
		ssProcPollution = new JTextField(2);
		ssProcPollution.setActionCommand("CSPACE INITIATE POLLUTION");
		ssProcPollution.setToolTipText("Switch for pollution processes.");
		ssProcPollution.addActionListener(this);
		
		ssProcInherit = new JTextField(2);
		ssProcInherit.setActionCommand("CSPACE INITIATE INHERITANCE");
		ssProcInherit.setToolTipText("Switch for inheritance processes.");
		ssProcInherit.addActionListener(this);
		
		ssProcCulture = new JTextField(2);
		ssProcCulture.setActionCommand("CSPACE INITIATE CULTURE");
		ssProcCulture.setToolTipText("Switch for culture processes.");
		ssProcCulture.addActionListener(this);
		
		ssProcDisease = new JTextField(2);
		ssProcDisease.setActionCommand("CSPACE INITIATE DISEASE");
		//ssProcDisease.setEnabled(false);
		ssProcDisease.setToolTipText("Switch for disease processes.");
		ssProcDisease.addActionListener(this);
		
		ssProcCombat = new JTextField(2);
		ssProcCombat.setActionCommand("CSPACE INITIATE COMBAT");
		ssProcCombat.setEnabled(false);
		ssProcCombat.setToolTipText("Switch for combat processes - not implemented yet.");
		ssProcCombat.addActionListener(this);
		
		ssFertSugar = new JTextField(5);
		ssFertSugar.setActionCommand("CSPACE FERTILITY SUGAR");
		ssFertSugar.setToolTipText("Sugar fertility across Sugarscape");
		ssFertSugar.addActionListener(this);
		
		ssFertSpice = new JTextField(5);
		ssFertSpice.setActionCommand("CSPACE FERTILITY SPICE");
		ssFertSpice.setToolTipText("Spice fertility across Sugarscape");
		ssFertSpice.addActionListener(this);
		
		ssSeasonDuration = new JTextField(6);
		ssSeasonDuration.setActionCommand("CSPACE SEASON DURATION");
		ssSeasonDuration.setToolTipText("Season duration in cycles");
		ssSeasonDuration.addActionListener(this);
		
		ssDiseaseCount = new JTextField(3);
		ssDiseaseCount.setActionCommand("CSPACE DISEASE COUNT");
		ssDiseaseCount.setToolTipText(
			"Total number of diseases existing on Sugarscape");
		ssDiseaseCount.addActionListener(this);




		
		ssDiseaseLenMin = new JTextField(2);
		ssDiseaseLenMin.setActionCommand("CSPACE DISEASE SIZE MIN");
		ssDiseaseLenMin.setToolTipText("Minimum string length of disease germs");
		ssDiseaseLenMin.addActionListener(this);

		ssDiseaseLenMax = new JTextField(2);
		ssDiseaseLenMax.setActionCommand("CSPACE DISEASE SIZE MAX");
		ssDiseaseLenMax.setToolTipText("Maximum string length of disease germs");
		ssDiseaseLenMax.addActionListener(this);

		
		
		
		ssDiseaseCostSugar = new JTextField(3);
		ssDiseaseCostSugar.setActionCommand("CSPACE DISEASE COST SUGAR");
		ssDiseaseCostSugar.setToolTipText("Unit sugar metabolism increase per infection");
		ssDiseaseCostSugar.addActionListener(this);

		ssDiseaseCostSpice = new JTextField(3);
		ssDiseaseCostSpice.setActionCommand("CSPACE DISEASE COST SPICE");
		ssDiseaseCostSpice.setToolTipText("Unit spice metabolism increase per infection");
		ssDiseaseCostSpice.addActionListener(this);

		ssDiseaseCostTrade = new JTextField(3);
		ssDiseaseCostTrade.setActionCommand("CSPACE DISEASE COST TRADE");
		ssDiseaseCostTrade.setToolTipText("Penalty applied to trades - not implemented yet!!");
		ssDiseaseCostTrade.addActionListener(this);
		
		//Draws First Line of Sugarscape Panel
		JPanel ss0aPanel = new JPanel(new FlowLayout());
		ss0aPanel.setBorder(blackline);
		JPanel ss0bPanel = new JPanel(new FlowLayout());
		ss0bPanel.setBorder(blackline);
		JPanel ss0cPanel = new JPanel(new FlowLayout());
		ss0cPanel.setBorder(blackline);
		JPanel ss0Panel = new JPanel(new FlowLayout());
		ss0Panel.setPreferredSize(new Dimension(580,44));
		ss0Panel.setBorder(blackline);
		ss0Panel.add(ss0aPanel);
		ss0Panel.add(ss0bPanel);
		ss0Panel.add(ss0cPanel);
		sScapePanel.add(ss0Panel);
		
		//Draws Second Line of Sugarscape Panel
		JPanel ss1Panel = new JPanel(); 
		ss1Panel.setLayout(new BoxLayout(ss1Panel, BoxLayout.Y_AXIS));
		ss1Panel.setPreferredSize(new Dimension(580,100));
		ss1Panel.setBorder(blackline);
		JPanel ss1aPanel = new JPanel();
		ss1aPanel.setLayout(new BoxLayout(ss1aPanel, BoxLayout.X_AXIS));
		ss1aPanel.setBorder(blackline);	//paneEdge
		JPanel ss1bPanel = new JPanel();
		ss1bPanel.setLayout(new BoxLayout(ss1bPanel, BoxLayout.X_AXIS));

		JPanel ss2Panel = new JPanel(); 
		ss2Panel.setLayout(new FlowLayout());
		ss2Panel.setPreferredSize(new Dimension(580,40));
		JPanel ss2aPanel = new JPanel();
		ss2aPanel.setLayout(new GridLayout(3,2,2,2));
		ss2aPanel.setBorder(blackline);
		JPanel ss2bPanel = new JPanel();
		ss2bPanel.setLayout(new GridLayout(3,2,2,2));
		ss2bPanel.setBorder(blackline);
		JPanel ss2cPanel = new JPanel();
		ss2cPanel.setLayout(new GridLayout(3,4,2,2));
		ss2cPanel.setBorder(blackline);
		ss2Panel.add(ss2aPanel);
		ss2Panel.add(ss2bPanel);
		ss2Panel.add(ss2cPanel);

		ss1bPanel.add(ss2Panel);
		ss1Panel.add(ss1aPanel);
		ss1Panel.add(ss1bPanel);
		sScapePanel.add(ss1Panel);

		JPanel ss3Panel = new JPanel(new FlowLayout());
		sScapePanel.add(ss3Panel);
		ss3Panel.setPreferredSize(new Dimension(580,30));
		ss3Panel.setBorder(blackline);

		JPanel ss4Panel = new JPanel(new FlowLayout());
		sScapePanel.add(ss4Panel);
		ss4Panel.setPreferredSize(new Dimension(580,30));
		ss4Panel.setBorder(blackline);

		ss0aPanel.add(new JLabel("Cols"));
		ss0aPanel.add(ssCols);
		ss0aPanel.add(new JLabel("Rows"));
		ss0aPanel.add(ssRows);
		ss0bPanel.add(new JLabel("Max: Cols"));
		ss0bPanel.add(ssMaxCols);
		ss0bPanel.add(new JLabel("Rows"));
		ss0bPanel.add(ssMaxRows);
		ss0cPanel.add(new JLabel("Density"));
		ss0cPanel.add(ssDensity);
		
		ss1aPanel.add(new JLabel(" Coordinates"));
		ss1aPanel.add(new JLabel("     "));
		ss1aPanel.add(new JLabel("Panels")); 
		ss1aPanel.add(new JLabel("                "));
		ss1aPanel.add(new JLabel("Sugarscape Processes")); 
		ss1aPanel.add(new JLabel(filler(48)));

		ss2aPanel.add(new JLabel(" Show"));
		ss2aPanel.add(ssCoordShow); 
		ss2aPanel.add(new JLabel(" Detail"));
		ss2aPanel.add(ssCoordDetail); 
		ss2aPanel.add(new JLabel(" Size")); 
		ss2aPanel.add(ssCoordSize); 
		ss2bPanel.add(new JLabel(" Height")); 
		ss2bPanel.add(ssPanelHeight); 
		ss2bPanel.add(new JLabel(" Max")); 
		ss2bPanel.add(ssPanelMax); 
		ss2bPanel.add(new JLabel(" Min"));
		ss2bPanel.add(ssPanelMin); 
		ss2cPanel.add(new JLabel(" Gather"));
		ss2cPanel.add(ssProcGather); 
		ss2cPanel.add(new JLabel(" Mate"));
		ss2cPanel.add(ssProcMate);
		ss2cPanel.add(new JLabel(" Barter"));
		ss2cPanel.add(ssProcTrade);
		ss2cPanel.add(new JLabel(" Seasons")); 
		ss2cPanel.add(ssProcSeasons);
		ss2cPanel.add(new JLabel(" Pollution"));
		ss2cPanel.add(ssProcPollution);
		ss2cPanel.add(new JLabel(" Inherit"));
		ss2cPanel.add(ssProcInherit); 
		ss2cPanel.add(new JLabel(" Culture")); 
		ss2cPanel.add(ssProcCulture);
		ss2cPanel.add(new JLabel(" Disease"));
		ss2cPanel.add(ssProcDisease);
		ss2cPanel.add(new JLabel(" Combat"));
		ss2cPanel.add(ssProcCombat); 

		ss3Panel.add(new JLabel("Fertility - Sugar"));
		ss3Panel.add(ssFertSugar);
		ss3Panel.add(new JLabel("  Spice"));
		ss3Panel.add(ssFertSpice);
		ss3Panel.add(new JLabel("  Season Duration"));
		ss3Panel.add(ssSeasonDuration);

		ss4Panel.add(new JLabel(" Diseases "));
		ssDiseaseCount.setToolTipText(
			"Total number of diseases existing on Sugarscape");
		ss4Panel.add(ssDiseaseCount);
		
		ss4Panel.add(new JLabel("Disease Len->Min "));
		ss4Panel.add(ssDiseaseLenMin);
				
		ss4Panel.add(new JLabel(" Max "));
		ss4Panel.add(ssDiseaseLenMax);
		ss4Panel.add(new JLabel(filler(6)));

		
		
		
		
		
		ss4Panel.add(new JLabel("Cost - Sugar"));
		ssDiseaseCostSugar.setToolTipText("Unit sugar metabolism increase per infection");
		ss4Panel.add(ssDiseaseCostSugar);
		ss4Panel.add(new JLabel(" Spice"));
		ssDiseaseCostSpice.setToolTipText("Unit sugar metabolism increase per infection");
		ss4Panel.add(ssDiseaseCostSpice);
//		ss4Panel.add(new JLabel(" Trade"));
//		ssDiseaseCostTrade.setToolTipText("Penalty applied to trades - not implemented yet!!");
//		ss4Panel.add(ssDiseaseCostTrade);

		updateSscapePanel();
	}
	
	public void updateSscapePanel()
	{
		ssCols.setText(Integer.toString(cellCols));
		ssRows.setText(Integer.toString(cellRows));
		ssMaxCols.setText(Integer.toString(GoLconst.GRID_COLS_MAX));
		ssMaxCols.setEditable(false);
		ssMaxRows.setText(Integer.toString(GoLconst.GRID_ROWS_MAX));
		ssMaxRows.setEditable(false);
		ssDensity.setText(Float.toString(GoLconst.DENSITY_FACTOR));
		
		ssCoordShow.setText(Boolean.toString(GoLconst.GRID_COORD_SHOW));
		ssCoordDetail.setText(Integer.toString(GoLconst.GRID_COORD_DETAIL));
		ssCoordSize.setText(Float.toString(GoLconst.GRID_COORD_FONT));
		ssCoordColor.setText(GoLconst.GRID_COORD_COLOR);

		ssPanelHeight.setText(Integer.toString(GoLconst.TEXTAREA_HEIGHT));
		ssPanelMax.setText(Integer.toString(GoLconst.TEXTAREA_MAX));
		ssPanelMin.setText(Integer.toString(GoLconst.TEXTAREA_MIN));
		
		ssProcGather.setText(Boolean.toString(GoLconst.INITIATE_GATHER));
		ssProcMate.setText(Boolean.toString(GoLconst.INITIATE_MATING));
		ssProcTrade.setText(Boolean.toString(GoLconst.INITIATE_BARTER));
		ssProcSeasons.setText(Boolean.toString(GoLconst.INITIATE_SEASONS));
		ssProcPollution.setText(Boolean.toString(GoLconst.INITIATE_POLLUTION));
		ssProcInherit.setText(Boolean.toString(GoLconst.INITIATE_INHERITANCE));
		ssProcCulture.setText(Boolean.toString(GoLconst.INITIATE_CULTURE));
		ssProcDisease.setText(Boolean.toString(GoLconst.INITIATE_DISEASE));
		//ssProcCombat.setText(Boolean.toString(GoLconst.INITIATE_COMBAT));
		
		ssFertSugar.setText(Float.toString(GoLconst.GRID_FERTILITY_SUGAR));
		ssFertSpice.setText(Float.toString(GoLconst.GRID_FERTILITY_SPICE));
		ssSeasonDuration.setText(Integer.toString(GoLconst.SEASON_DURATION));
		
		ssDiseaseCount.setText(Integer.toString(GoLconst.CELLSPACE_DISEASE_COUNT));
		ssDiseaseLenMin.setText(Integer.toString(GoLconst.DISEASE_SIZE_MIN));
		ssDiseaseLenMax.setText(Integer.toString(GoLconst.DISEASE_SIZE_MAX));
		
		
		ssDiseaseCostSugar.setText(Float.toString(GoLconst.DISEASE_COST_SUGAR));
		ssDiseaseCostSpice.setText(Float.toString(GoLconst.DISEASE_COST_SPICE));
//		ssDiseaseCostTrade.setText(Float.toString(GoLconst.DISEASE_COST_TRADE));
	}
	
	public void createCellPanel()
	{
		ceBground = new JTextField(2);
		ceBground.setActionCommand("GRIDCELL BG");
		ceBground.setToolTipText("Cell background color");
		ceBground.addActionListener(this);
		
		ceBar = new JTextField(2);
		ceBar.setActionCommand("GRIDCELL BAR");
		ceBar.setToolTipText("Bar(showing resource levels) color");
		ceBar.addActionListener(this);
		
		ceSenior = new JTextField(2);
		ceSenior.setActionCommand("ZEN COLOR SENIOR");
		ceSenior.setToolTipText("Senior citizen color");
		ceSenior.addActionListener(this);
		
		ceChild = new JTextField(2);
		ceChild.setActionCommand("ZEN COLOR CHILD");
		ceChild.setToolTipText("Child citizen color");
		ceChild.addActionListener(this);
		
		ce3D = new JTextField(2);
		ce3D.setActionCommand("GRIDCELL 3D");
		ce3D.setToolTipText("Switch for 3D display");
		ce3D.addActionListener(this);
		
		ceLimitSugar = new JTextField(2);
		ceLimitSugar.setActionCommand("GRIDCELL LIMIT SUGAR");
		ceLimitSugar.setToolTipText("Switch controlling sugar accumulation in cells");
		ceLimitSugar.addActionListener(this);
		
		ceLimitSpice = new JTextField(2);
		ceLimitSpice.setActionCommand("GRIDCELL LIMIT SPICE");
		ceLimitSpice.setToolTipText("Switch controlling spice accumulation in cells");
		ceLimitSpice.addActionListener(this);
		
		ceSugarMax = new JTextField(2);
		ceSugarMax.setActionCommand("GRIDCELL SUGAR MAX");
		ceSugarMax.setToolTipText("Max sugar accumulated in cell, if limit is applied");
		ceSugarMax.addActionListener(this);
		
		ceSpiceMax = new JTextField(2);
		ceSpiceMax.setActionCommand("GRIDCELL SPICE MAX");
		ceSpiceMax.setToolTipText("Max spice accumulated in cell, if limit is applied");
		ceSpiceMax.addActionListener(this);
		
		ceRenewSuSummer = new JTextField(2);
		ceRenewSuSummer.setActionCommand("GRIDCELL SUGAR RENEW SUMMER");
		ceRenewSuSummer.setToolTipText("Rate of sugar renewal per cycle during summer");
		ceRenewSuSummer.addActionListener(this);
		
		ceRenewSuWinter = new JTextField(2);
		ceRenewSuWinter.setActionCommand("GRIDCELL SUGAR RENEW WINTER");
		ceRenewSuWinter.setToolTipText("Rate of sugar renewal per cycle during winter");
		ceRenewSuWinter.addActionListener(this);
		
		ceRenewSpSummer = new JTextField(2);
		ceRenewSpSummer.setActionCommand("GRIDCELL SPICE RENEW SUMMER");
		ceRenewSpSummer.setToolTipText("Rate of spice renewal per cycle during summer");
		ceRenewSpSummer.addActionListener(this);
		
		ceRenewSpWinter = new JTextField(2);
		ceRenewSpWinter.setActionCommand("GRIDCELL SPICE RENEW WINTER");
		ceRenewSpWinter.setToolTipText("Rate of spice renewal per cycle during winter");
		ceRenewSpWinter.addActionListener(this);
		
		cePolluSugar = new JTextField(2);
		cePolluSugar.setActionCommand("GRIDCELL POLLUTION PRODUCTION SUGAR");
		cePolluSugar.setToolTipText("Rate of pollution per sugar unit collected from a cell");
		cePolluSugar.addActionListener(this);
		
		cePolluSpice = new JTextField(2);
		cePolluSpice.setActionCommand("GRIDCELL POLLUTION PRODUCTION SPICE");
		cePolluSpice.setToolTipText("Rate of pollution per spice unit collected from a cell");
		cePolluSpice.addActionListener(this);
		
		cePolluDispersal = new JTextField(2);
		cePolluDispersal.setActionCommand("GRIDCELL POLLUTION DISPERSION UNIT");
		cePolluDispersal.setToolTipText("Rate of pollution dispersion in cells during each cycle ");
		cePolluDispersal.addActionListener(this);
		
		//Draws First Line of Cell Panel
		JPanel ce0Panel = new JPanel();
		ce0Panel.setLayout(new BoxLayout(ce0Panel, BoxLayout.X_AXIS));
		ce0Panel.setPreferredSize(new Dimension(580,30));
		ce0Panel.setBorder(doubleBlack);

		JPanel ce1Panel = new JPanel();
		ce1Panel.setLayout(new BoxLayout(ce1Panel, BoxLayout.X_AXIS));
		ce1Panel.setPreferredSize(new Dimension(580,30));
		ce1Panel.setBorder(doubleBlack);

		JPanel ce2Panel = new JPanel();
		ce2Panel.setLayout(new GridLayout(1,2,1,1));
		ce2Panel.setPreferredSize(new Dimension(580,66));
		ce2Panel.setBorder(doubleBlack);
		JPanel ce2aPanel = new JPanel();
		ce2aPanel.setLayout(new GridLayout(2,5,5,5));
		ce2aPanel.setBorder(blackline);
		JPanel ce2bPanel = new JPanel();
		ce2bPanel.setLayout(new GridLayout(2,5,5,5));
		ce2bPanel.setBorder(blackline);
		ce2Panel.add(ce2aPanel);
		ce2Panel.add(ce2bPanel);

		JPanel ce3Panel = new JPanel();
		ce3Panel.setLayout(new BoxLayout(ce3Panel, BoxLayout.X_AXIS));
		ce3Panel.setPreferredSize(new Dimension(580,30));
		ce3Panel.setBorder(doubleBlack);

		cellPanel.add(ce0Panel);
		//cellPanel.add(ce1Panel);
		cellPanel.add(ce2Panel);
		cellPanel.add(ce3Panel);

		ce0Panel.add(new JLabel(" Color: "));
		ce0Panel.add(new JLabel(" B'ground "));
		ce0Panel.add(ceBground);
		ce0Panel.add(new JLabel(" Bar "));
		ce0Panel.add(ceBar);
		ce0Panel.add(new JLabel(" Senior "));
		ce0Panel.add(ceSenior);
		ce0Panel.add(new JLabel(" Child "));
		ce0Panel.add(ceChild);
		ce0Panel.add(new JLabel(" 3D "));
		ce0Panel.add(ce3D);
		ce0Panel.add(new JLabel(filler(50)));
		ce2aPanel.add(new JLabel(" Sugar:"));
		ce2aPanel.add(new JLabel(" Limit Sugar"));
		ce2aPanel.add(ceLimitSugar);
		ce2aPanel.add(new JLabel(" Max"));
		ce2aPanel.add(ceSugarMax);
		ce2aPanel.add(new JLabel(" Renew: "));
		ce2aPanel.add(new JLabel("Summer "));
		ce2aPanel.add(ceRenewSuSummer);
		ce2aPanel.add(new JLabel(" Winter"));
		ce2aPanel.add(ceRenewSuWinter);
		ce2bPanel.add(new JLabel(" Spice:"));
		ce2bPanel.add(new JLabel(" Limit Spice"));
		ce2bPanel.add(ceLimitSpice);
		ce2bPanel.add(new JLabel(" Max"));
		ce2bPanel.add(ceSpiceMax);
		ce2bPanel.add(new JLabel(" Renew: "));
		ce2bPanel.add(new JLabel("Summer "));
		ce2bPanel.add(ceRenewSpSummer);
		ce2bPanel.add(new JLabel(" Winter"));
		ce2bPanel.add(ceRenewSpWinter);
		ce3Panel.add(new JLabel(" Pollution:  "));
		ce3Panel.add(new JLabel(" Sugar "));
		ce3Panel.add(cePolluSugar);
		ce3Panel.add(new JLabel(" Spice "));
		ce3Panel.add(cePolluSpice);
		ce3Panel.add(new JLabel(" Dispersal "));
		ce3Panel.add(cePolluDispersal);		
		ce3Panel.add(new JLabel(filler(70)));
		
		updateCellPanel();
	}
	
	public void updateCellPanel()
	{
		ceBground.setText(Integer.toString(GoLconst.CELL_BG_SUMMER));
		ceBar.setText(Integer.toString(GoLconst.CELL_BAR));
		ceSenior.setText(Integer.toString(GoLconst.CITIZEN_COLOR_SENIOR));
		ceChild.setText(Integer.toString(GoLconst.CITIZEN_COLOR_CHILD));
		ce3D.setText(Boolean.toString(GoLconst.CELL_3D));
		ceLimitSugar.setText(Boolean.toString(GoLconst.LIMIT_CELL_SUGAR));
		ceLimitSpice.setText(Boolean.toString(GoLconst.LIMIT_CELL_SPICE));
		ceSugarMax.setText(Integer.toString(GoLconst.SUGAR_MAX_CELL));
		ceSpiceMax.setText(Integer.toString(GoLconst.SPICE_MAX_CELL));
		ceRenewSuSummer.setText(Float.toString(GoLconst.SUGAR_RENEW_SUMMER));
		ceRenewSuWinter.setText(Float.toString(GoLconst.SUGAR_RENEW_WINTER));
		ceRenewSpSummer.setText(Float.toString(GoLconst.SPICE_RENEW_SUMMER));
		ceRenewSpWinter.setText(Float.toString(GoLconst.SPICE_RENEW_WINTER));
		cePolluSugar.setText(Float.toString(GoLconst.POLLUTION_PRODUCTION_SUGAR));
		cePolluSpice.setText(Float.toString(GoLconst.POLLUTION_PRODUCTION_SPICE));
		cePolluDispersal.setText(Float.toString(GoLconst.POLLUTION_DISPERSION_UNIT));
	}

	public void createCitizenPanel()
	{
		ciVision = new JTextField(2);
		ciVision.setActionCommand("ZEN VISION MAX");
		ciVision.setToolTipText("Max vision value");
		ciVision.addActionListener(this);
		
		ciSex = new JTextField(1);
		ciSex.setActionCommand("ZEN SEX RATIO");
		ciSex.setToolTipText("Population sex ratio");
		ciSex.addActionListener(this);
		
		ciPersona = new JTextField(1);
		ciPersona.setActionCommand("ZEN PERSONALITY RATIO");
		ciPersona.setToolTipText("Population personality ratio");
		ciPersona.addActionListener(this);
		
		ciSuMin = new JTextField(2);
		ciSuMin.setActionCommand("ZEN SUGAR MIN");
		ciSuMin.setToolTipText("Minimum initial allotment of sugar");
		ciSuMin.addActionListener(this);
		
		ciSuMax = new JTextField(3);
		ciSuMax.setActionCommand("ZEN SUGAR MAX");
		ciSuMax.setToolTipText("Maximum initial allotment of sugar");
		ciSuMax.addActionListener(this);
		
		ciSuMetab = new JTextField(2);
		ciSuMetab.setActionCommand("ZEN METABOLISM MAX SUGAR");
		ciSuMetab.setToolTipText("Max sugar metabolism");
		ciSuMetab.addActionListener(this);
		
		ciSpMin = new JTextField(2);
		ciSpMin.setActionCommand("ZEN SPICE MIN");
		ciSpMin.setToolTipText("Min initial spice allotment");
		ciSpMin.addActionListener(this);
		
		ciSpMax = new JTextField(3);
		ciSpMax.setActionCommand("ZEN SPICE MAX");
		ciSpMax.setToolTipText("Max initial spice allotment");
		ciSpMax.addActionListener(this);
		
		ciSpMetab = new JTextField(2);
		ciSpMetab.setActionCommand("ZEN METABOLISM MAX SPICE");
		ciSpMetab.setToolTipText("Max spice metabolism");
		ciSpMetab.addActionListener(this);
		
		ciLifeMin = new JTextField(2);
		ciLifeMin.setActionCommand("ZEN LIFE EXPECTANCY MIN");
		ciLifeMin.setToolTipText("Min life expectancy");
		ciLifeMin.addActionListener(this);
		
		ciLifeMax = new JTextField(2);
		ciLifeMax.setActionCommand("ZEN LIFE EXPECTANCY MAX");
		ciLifeMax.setToolTipText("Max life expectancy");
		ciLifeMax.addActionListener(this);
		
		ciSugarPoor = new JTextField(2);
		ciSugarPoor.setActionCommand("ZEN SUGAR LEVEL POOR");
		ciSugarPoor.setToolTipText("Poverty level for sugar");
		ciSugarPoor.addActionListener(this);
		
		ciSpicePoor = new JTextField(2);
		ciSpicePoor.setActionCommand("ZEN SPICE LEVEL POOR");
		ciSpicePoor.setToolTipText("Poverty level for spice");
		ciSpicePoor.addActionListener(this);
		
		ciMateMaleMin = new JTextField(1);
		ciMateMaleMin.setActionCommand("ZEN MATING MALE MIN");
		ciMateMaleMin.setToolTipText("Mating age min for males");
		ciMateMaleMin.addActionListener(this);
		
		ciMateMaleMax = new JTextField(1);
		ciMateMaleMax.setActionCommand("ZEN MATING MALE MAX");
		ciMateMaleMax.setToolTipText("Mating age max for males");
		ciMateMaleMax.addActionListener(this);
		
		ciMateMaleGap = new JTextField(1);
		ciMateMaleGap.setActionCommand("ZEN MATING MALE GAP");
		ciMateMaleGap.setToolTipText("Min gap required between children");
		ciMateMaleGap.addActionListener(this);
		
		ciMateFemaleMin= new JTextField(1);
		ciMateFemaleMin.setActionCommand("ZEN MATING FEMALE MIN");
		ciMateFemaleMin.setToolTipText("Mating age min for females");
		ciMateFemaleMin.addActionListener(this);
		
		ciMateFemaleMax = new JTextField(1);
		ciMateFemaleMax.setActionCommand("ZEN MATING FEMALE MAX");
		ciMateFemaleMax.setToolTipText("Mating age max for females");
		ciMateFemaleMax.addActionListener(this);
		
		ciMateFemaleGap = new JTextField(1);
		ciMateFemaleGap.setActionCommand("ZEN MATING FEMALE GAP");
		ciMateFemaleGap.setToolTipText("Min gap required between children");
		ciMateFemaleGap.addActionListener(this);

		ciColorType = new JTextField(1);
		ciColorType.setActionCommand("ZEN COLOR TYPE");
		ciColorType.setToolTipText("1->Wealth, 2->Group, 3->Cost of disease");
		ciColorType.addActionListener(this);

		ciCultureTagLen = new JTextField(1);
		ciCultureTagLen.setActionCommand("ZEN TAG LENGTH");
		ciCultureTagLen.setToolTipText("Length of citizen's culture tag");
		ciCultureTagLen.addActionListener(this);

		ciCultureTagDivider = new JTextField(1);
		ciCultureTagDivider.setActionCommand("ZEN TAG DIVIDER");
		ciCultureTagDivider.setToolTipText("Sticky/Volatile tag division");
		ciCultureTagDivider.addActionListener(this);

		ciCultureTagFlipType = new JTextField(1);
		ciCultureTagFlipType.setActionCommand("ZEN TAG FLIP TYPE");
		ciCultureTagFlipType.setToolTipText("1->absorb, 2->propogate, 3->ranking");
		ciCultureTagFlipType.addActionListener(this);

		ciCultureTagMinStickyFlip = new JTextField(1);
		ciCultureTagMinStickyFlip.setActionCommand("ZEN MIN STICKY FLIP");
		ciCultureTagMinStickyFlip.setToolTipText(
			"Neighbors needed to change sticky tags");
		ciCultureTagMinStickyFlip.addActionListener(this);

		ciDiseaseInitial = new JTextField(1);
		ciDiseaseInitial.setActionCommand("ZEN DISEASE COUNT INITIAL");
		ciDiseaseInitial.setToolTipText(
			"Max diseases that citizen is born with");
		ciDiseaseInitial.addActionListener(this);

		ciImmuneSysLen = new JTextField(1);
		ciImmuneSysLen.setActionCommand("ZEN IMMUNE STRING");
		ciImmuneSysLen.setToolTipText("Length of immune system string");
		ciImmuneSysLen.addActionListener(this);

		gCount = GoLconst.MAX_GROUPS;	//GoLconst.groupCount();
		boolean lastGroup = false; 
		ciGroupInterval = new JTextField[gCount];
		ciGroupName		= new JTextField[gCount];
		ciGroupColor	= new JTextField[gCount*3];
		int a,b,c;
		for(int i=0; i<gCount; i++)
		{	a=i*3;	b=a+1;	c=b+1;
			ciGroupInterval[i] = new JTextField(2);
			ciGroupName[i] = new JTextField(4);
			ciGroupColor[a] = new JTextField(2);
			ciGroupColor[b] = new JTextField(2);
			ciGroupColor[c] = new JTextField(2);

			if(i < GoLconst.CITIZEN_GROUP_INTERVAL.length)
				ciGroupInterval[i].setText(Integer.toString(GoLconst.CITIZEN_GROUP_INTERVAL[i]));
			else
			{	lastGroup = true;
				ciGroupInterval[i].setText(Integer.toString(GoLconst.CITIZEN_CULTURE_TAG));
			}

			ciGroupInterval[i].setActionCommand("ZEN GRP INTERVAL " + i);
			ciGroupInterval[i].addActionListener(this);

			ciGroupName[i].setText(GoLconst.CITIZEN_GROUP_NAME[i]);
			ciGroupName[i].setActionCommand("ZEN GRP NAME " + i);
			ciGroupName[i].addActionListener(this);

			ciGroupColor[a].setText(Integer.toString(GoLconst.CITIZEN_GROUP_COLOR[a]));
			ciGroupColor[a].setActionCommand("ZEN GRP COLOR " + a);
			ciGroupColor[a].addActionListener(this);
			ciGroupColor[b].setText(Integer.toString(GoLconst.CITIZEN_GROUP_COLOR[b]));
			ciGroupColor[b].setActionCommand("ZEN GRP COLOR " + b);
			ciGroupColor[b].addActionListener(this);
			ciGroupColor[c].setText(Integer.toString(GoLconst.CITIZEN_GROUP_COLOR[c]));
			ciGroupColor[c].setActionCommand("ZEN GRP COLOR " + c);
			ciGroupColor[c].addActionListener(this);
		}
		ciKinMate = new JTextField(1);
		ciKinMate.setActionCommand("ZEN MATING KIN ALLOW");
		ciKinMate.setToolTipText("Allow parents, children & siblings to mate");
		ciKinMate.addActionListener(this);
		
		ciInheritInitial = new JTextField(1);
		ciInheritInitial.setActionCommand("ZEN INHERIT INITIAL WEALTH");
		ciInheritInitial.setToolTipText("Children inherit parents initial inheritance or current wealth");
		ciInheritInitial.addActionListener(this);
		
		ciSurnamesUnique = new JTextField(1);
		ciSurnamesUnique.setActionCommand("ZEN CREATE UNIQUE SURNAMES");
		ciSurnamesUnique.setToolTipText("Ensure unique surnames for initial population");
		ciSurnamesUnique.addActionListener(this);
		
		ciSurnamesFather = new JTextField(1);
		ciSurnamesFather.setActionCommand("ZEN INHERIT FAMILY FATHER");
		ciSurnamesFather.setToolTipText("Children take father's name");
		ciSurnamesFather.addActionListener(this);
				
		//Pane #1 of 3 on Citizen Panel
		JPanel ci1Panel = new JPanel();
		ci1Panel.setLayout(new BoxLayout(ci1Panel, BoxLayout.Y_AXIS));
		ci1Panel.setPreferredSize(new Dimension(600,90));
		ci1Panel.setBorder(blackline);
			JPanel ci1aPanel = new JPanel();
			ci1aPanel.setLayout(new BoxLayout(ci1aPanel, BoxLayout.X_AXIS));
			ci1aPanel.add(new JLabel( filler(9) + "General"));
			ci1aPanel.add(new JLabel(filler(26)));
			ci1aPanel.add(new JLabel("Sugar"));
			ci1aPanel.add(new JLabel(filler(29)));
			ci1aPanel.add(new JLabel("Spice"));
			ci1aPanel.add(new JLabel(filler(22)));
			ci1aPanel.add(new JLabel("Mating: Male"));
			ci1aPanel.add(new JLabel(filler(12)));
			ci1aPanel.add(new JLabel("Female"));
			ci1aPanel.add(new JLabel(filler(10)));
			JPanel ci1bPanel = new JPanel();
			ci1bPanel.setLayout(new BoxLayout(ci1bPanel, BoxLayout.X_AXIS));
			ci1bPanel.setBorder(doubleBlack);	//blackline
				JPanel ci1b1Panel = new JPanel();
				ci1b1Panel.setLayout(new GridLayout(3,2));
				//ci1b1Panel.setBorder(blackline);
				ci1b1Panel.add(new JLabel(" Vision"));
				ci1b1Panel.add(ciVision);
				ci1b1Panel.add(new JLabel(" M:F"));
				ci1b1Panel.add(ciSex);
				ci1b1Panel.add(new JLabel(" Bull:Bear"));
				ci1b1Panel.add(ciPersona);
			
				JPanel ci1b2Panel = new JPanel();
				ci1b2Panel.setLayout(new GridLayout(3,2));
				//ci1b2Panel.setBorder(blackline);
				ci1b2Panel.add(new JLabel(" Min"));
				ci1b2Panel.add(ciSuMin);
				ci1b2Panel.add(new JLabel(" Max"));
				ci1b2Panel.add(ciSuMax);
				ci1b2Panel.add(new JLabel(" Met'blsm"));
				ci1b2Panel.add(ciSuMetab);
	
				JPanel ci1b3Panel = new JPanel();
				ci1b3Panel.setLayout(new GridLayout(3,2));
				//ci1b3Panel.setBorder(blackline);
				ci1b3Panel.add(new JLabel(" Min"));
				ci1b3Panel.add(ciSpMin);
				ci1b3Panel.add(new JLabel(" Max"));
				ci1b3Panel.add(ciSpMax);
				ci1b3Panel.add(new JLabel(" Met'blism"));
				ci1b3Panel.add(ciSpMetab);
		
				JPanel ci1b4Panel = new JPanel();
				ci1b4Panel.setLayout(new GridLayout(3,2));
				//ci1b4Panel.setBorder(blackline);
				ci1b4Panel.add(new JLabel(" Min" + filler(6)));
				ci1b4Panel.add(ciMateMaleMin);
				ci1b4Panel.add(new JLabel(" Max" + filler(6)));
				ci1b4Panel.add(ciMateMaleMax);
				ci1b4Panel.add(new JLabel(" Gap" + filler(6)));
				ci1b4Panel.add(ciMateMaleGap);
		
				JPanel ci1b5Panel = new JPanel();
				ci1b5Panel.setLayout(new GridLayout(3,2));
				//ci1b5Panel.setBorder(blackline);
				ci1b5Panel.add(new JLabel(" Min" + filler(6)));
				ci1b5Panel.add(ciMateFemaleMin);
				ci1b5Panel.add(new JLabel(" Max" + filler(6)));
				ci1b5Panel.add(ciMateFemaleMax);
				ci1b5Panel.add(new JLabel(" Gap" + filler(6)));
				ci1b5Panel.add(ciMateFemaleGap);
			ci1bPanel.add(ci1b1Panel);
			ci1bPanel.add(ci1b2Panel);
			ci1bPanel.add(ci1b3Panel);
			ci1bPanel.add(ci1b4Panel);
			ci1bPanel.add(ci1b5Panel);
		ci1Panel.add(ci1aPanel);
		ci1Panel.add(ci1bPanel);

		//Pane #2 of 3 on Citizen Panel
		JPanel ci2Panel = new JPanel();
		ci2Panel.setLayout(new BoxLayout(ci2Panel, BoxLayout.Y_AXIS));
		ci2Panel.setPreferredSize(new Dimension(600,110));
		ci2Panel.setBorder(blackline);
			JPanel ci2aPanel = new JPanel();
			ci2aPanel.setLayout(new BoxLayout(ci2aPanel, BoxLayout.X_AXIS));
			
			ci2aPanel.add(new JLabel(" Color"));
			//ciColorType = new JTextField(1);
			ci2aPanel.add(ciColorType);
			
			ci2aPanel.add(new JLabel(" Culture Len"));
			//ciCultureTagLen = new JTextField(1);
			ci2aPanel.add(ciCultureTagLen);
			
			ci2aPanel.add(new JLabel(" Divider"));
			//ciCultureTagDivider = new JTextField(1);
			ci2aPanel.add(ciCultureTagDivider);
			
			ci2aPanel.add(new JLabel(" Direction"));
			//ciCultureTagFlipType = new JTextField(1);
			ci2aPanel.add(ciCultureTagFlipType);
			
			ci2aPanel.add(new JLabel(" Overwhelm"));
			//ciCultureTagMinStickyFlip = new JTextField(1);
			ci2aPanel.add(ciCultureTagMinStickyFlip);
		
			ci2aLbl7 = new JLabel(" Diseases");
			ci2aPanel.add(ci2aLbl7);
			//ciDiseaseInitial = new JTextField(1);
			ci2aPanel.add(ciDiseaseInitial);
	
			ci2aLbl8 = new JLabel(" Imm.Sys Len");
			ci2aPanel.add(ci2aLbl8);
			//ciImmuneSysLen = new JTextField(1);
			ci2aPanel.add(ciImmuneSysLen);

			//ci2aPanel.add(new JLabel(filler(70)));
			JPanel ci2bPanel = new JPanel();
			ci2bPanel.setLayout(new BoxLayout(ci2bPanel, BoxLayout.X_AXIS));
			ci2bPanel.add(new JLabel(" Intervals"));
			gCount = GoLconst.MAX_GROUPS; //GoLconst.groupCount();
			lastGroup = false;
			for(int i=0; i<gCount; i++)
				if(!lastGroup && Integer.parseInt(ciGroupInterval[i].getText()) 
									< GoLconst.CITIZEN_CULTURE_TAG)
					ci2bPanel.add(ciGroupInterval[i]);
				else
				{	ci2bPanel.add(ciGroupInterval[i]);
					if(lastGroup)
					{	ciGroupInterval[i].hide();	}
					else
					{	lastGroup = true;	}
				} 

			JPanel ci2cPanel = new JPanel();
			gCount = GoLconst.groupCount();
			if(gCount <= 4)
				ci2cPanel.setLayout(new GridLayout(2,gCount+1,5,0));
			else
				ci2cPanel.setLayout(new GridLayout(2,gCount,0,0));
			ci2cPanel.setBorder(doubleBlack);
					
			ci2cPanel.setPreferredSize(new Dimension(600,60));
			if(gCount <= 4)
				ci2cPanel.add(new JLabel(" Name"));
			for(int i=0; i<gCount; i++)
				ci2cPanel.add(ciGroupName[i]);
			if(gCount <= 4)
				ci2cPanel.add(new JLabel(" RGBcode"));
			ciColorPanel = new JPanel[gCount];			
			for(int i=0; i<gCount; i++)
			{	ciColorPanel[i] = new JPanel();
				//ciColorPanel[i].setBorder(blackline);
				ciColorPanel[i].add(ciGroupColor[i*3]);
				ciColorPanel[i].add(ciGroupColor[i*3+1]);
				ciColorPanel[i].add(ciGroupColor[i*3+2]);
				ci2cPanel.add(ciColorPanel[i]);
			}
		ci2Panel.add(ci2aPanel);
		ci2Panel.add(ci2bPanel);
		ci2Panel.add(ci2cPanel);
			
		JPanel ci4Panel = new JPanel();
		ci4Panel.setLayout(new BoxLayout(ci4Panel, BoxLayout.X_AXIS));
		ci4Panel.setPreferredSize(new Dimension(600,20));
		ci4Panel.setBorder(blackline);
		
		ci4Panel.add(new JLabel(" Allow Kin Mate "));
		ci4Panel.add(ciKinMate);
		ci4Panel.add(new JLabel(" Inherit Initial Wealth "));
		ci4Panel.add(ciInheritInitial);
		ci4Panel.add(new JLabel(filler(5)));
		ci4Panel.add(new JLabel(" Unique Surnames "));
		ci4Panel.add(ciSurnamesUnique);
		ci4Panel.add(new JLabel(" Father's Surname "));
		ci4Panel.add(ciSurnamesFather); 
		
		citizenPanel.add(ci1Panel);
		citizenPanel.add(ci2Panel);
		citizenPanel.add(ci4Panel);
		updateCitizenPanel();
	}
	
	public void updateCitizenPanel()
	{
		ciVision.setText(Integer.toString(GoLconst.VISION_MAX));
		ciSex.setText(Float.toString(GoLconst.SEX_RATIO));
		ciPersona.setText(Float.toString(GoLconst.PERSONALITY_RATIO));
		ciSuMin.setText(Integer.toString(GoLconst.SUGAR_MIN_CITIZEN));
		ciSuMax.setText(Integer.toString(GoLconst.SUGAR_MAX_CITIZEN));
		ciSuMetab.setText(Integer.toString(GoLconst.METABOLISM_MAX_SUGAR));
		ciSpMin.setText(Integer.toString(GoLconst.SPICE_MIN_CITIZEN));
		ciSpMax.setText(Integer.toString(GoLconst.SPICE_MAX_CITIZEN));
		ciSpMetab.setText(Integer.toString(GoLconst.METABOLISM_MAX_SPICE));
		
		ciLifeMin.setText(Integer.toString(GoLconst.LIFE_EXPECTANCY_MIN));
		ciLifeMax.setText(Integer.toString(GoLconst.LIFE_EXPECTANCY_MAX));
		ciSugarPoor.setText(Integer.toString(GoLconst.SUGAR_LEVEL_POOR));
		ciSpicePoor.setText(Integer.toString(GoLconst.SPICE_LEVEL_POOR));
		ciMateMaleMin.setText(Integer.toString(GoLconst.MATING_MALE_MIN));
		ciMateMaleMax.setText(Integer.toString(GoLconst.MATING_MALE_MAX));
		ciMateMaleGap.setText(Integer.toString(GoLconst.MATING_MALE_GAP));
		ciMateFemaleMin.setText(Integer.toString(GoLconst.MATING_FEMALE_MIN));
		ciMateFemaleMax.setText(Integer.toString(GoLconst.MATING_FEMALE_MAX));
		ciMateFemaleGap.setText(Integer.toString(GoLconst.MATING_FEMALE_GAP));
		
		ciColorType.setText(Integer.toString(GoLconst.CITIZEN_COLOR));
		ciCultureTagLen.setText(Integer.toString(GoLconst.CITIZEN_CULTURE_TAG));
		ciCultureTagDivider.setText(Integer.toString(GoLconst.CITIZEN_CULTURE_DIVIDE));
		ciCultureTagFlipType.setText(Integer.toString(GoLconst.CITIZEN_CULTURE_TRANSFER));
		ciCultureTagMinStickyFlip.setText(Integer.toString(GoLconst.CITIZEN_CULTURE_OVERWHELM));
		ciDiseaseInitial.setText(Integer.toString(GoLconst.DISEASE_COUNT_INITIAL));
		ciImmuneSysLen.setText(Integer.toString(GoLconst.CITIZEN_IMMUNE_STRING));
//		ciDiseaseLenMin.setText(Integer.toString(GoLconst.DISEASE_SIZE_MIN));
//		ciDiseaseLenMax.setText(Integer.toString(GoLconst.DISEASE_SIZE_MAX));
		
		gCount = GoLconst.groupCount();
		int a,b,c;
		for(int i=0; i<gCount; i++)
		{	a=i*3;	b=a+1;	c=b+1;
			ciGroupInterval[i].setText(Integer.toString(GoLconst.CITIZEN_GROUP_INTERVAL[i]));
			ciGroupName[i].setText(GoLconst.CITIZEN_GROUP_NAME[i]);
			ciGroupColor[a].setText(Integer.toString(GoLconst.CITIZEN_GROUP_COLOR[a]));
			ciGroupColor[b].setText(Integer.toString(GoLconst.CITIZEN_GROUP_COLOR[b]));
			ciGroupColor[c].setText(Integer.toString(GoLconst.CITIZEN_GROUP_COLOR[c]));
			ciGroupName[i].setBackground(new Color(GoLconst.CITIZEN_GROUP_COLOR[a],
													GoLconst.CITIZEN_GROUP_COLOR[b],
													GoLconst.CITIZEN_GROUP_COLOR[c]));
		}
		ciKinMate.setText(Boolean.toString(GoLconst.MATING_KIN_ALLOW));
		ciInheritInitial.setText(Boolean.toString(GoLconst.INHERIT_INITIAL_WEALTH));
		ciSurnamesUnique.setText(Boolean.toString(GoLconst.CREATE_UNIQUE_SURNAMES));
		ciSurnamesFather.setText(Boolean.toString(GoLconst.INHERIT_FAMILY_FATHER));
	}
	
	public void createConfigPanel()
	{	deDebug = new JTextField(1);
		deDebug.setActionCommand("DEBUG ALL");
		deDebug.addActionListener(this);
		
		deCriticalError = new JTextField(1);
		deCriticalError.setActionCommand("DEBUG CRITICAL ERROR");
		deCriticalError.addActionListener(this);

		deCmdFeedback = new JTextField(1);
		deCmdFeedback.setActionCommand("DEBUG CMD FEEDBACK");
		deCmdFeedback.addActionListener(this);

		deProgFlow = new JTextField(1);
		deProgFlow.setActionCommand("DEBUG PROGRAM FLOW");
		deProgFlow.addActionListener(this);

		deSugarSearch = new JTextField(1);
		deSugarSearch.setActionCommand("DEBUG SEARCH SUGAR");
		deSugarSearch.addActionListener(this);

		deProductionSugar = new JTextField(1);
		deProductionSugar.setActionCommand("DEBUG SUGAR PRODUCTION");
		deProductionSugar.addActionListener(this);

		deProductionSpice = new JTextField(1);
		deProductionSpice.setActionCommand("DEBUG SPICE PRODUCTION");
		deProductionSpice.addActionListener(this);

		deMating = new JTextField(1);
		deMating.setActionCommand("DEBUG MATING");
		deMating.addActionListener(this);

		deMateSearch = new JTextField(1);
		deMateSearch.setActionCommand("DEBUG MATING SEARCH");
		deMateSearch.addActionListener(this);

		deMateSelection = new JTextField(1);
		deMateSelection.setActionCommand("DEBUG MATING SELECTION");
		deMateSelection.addActionListener(this);

		deChildBirth = new JTextField(1);
		deChildBirth.setActionCommand("DEBUG MATING BIRTH");
		deChildBirth.addActionListener(this);

		dePollution = new JTextField(1);
		dePollution.setActionCommand("DEBUG POLLUTION");
		dePollution.addActionListener(this);

		deTime = new JTextField(1);
		deTime.setActionCommand("DEBUG PROCESS TIME");
		deTime.addActionListener(this);

		deInheritance = new JTextField(1);
		deInheritance.setActionCommand("DEBUG INHERITANCE");
		deInheritance.addActionListener(this);

		deBarter = new JTextField(1);
		deBarter.setActionCommand("DEBUG BARTER");
		deBarter.addActionListener(this);

		deBarterSort = new JTextField(1);
		deBarterSort.setActionCommand("DEBUG BARTER SORT");
		deBarterSort.addActionListener(this);

		deBarterExchange = new JTextField(1);
		deBarterExchange.setActionCommand("DEBUG BARTER EXCHANGE");
		deBarterExchange.addActionListener(this);

		deCulture = new JTextField(1);
		deCulture.setActionCommand("DEBUG CULTURE");
		deCulture.addActionListener(this);

		deCultureTransmit = new JTextField(1);
		deCultureTransmit.setActionCommand("DEBUG CULTURE TRANSMIT");
		deCultureTransmit.addActionListener(this);

		deCultureVolatile = new JTextField(1);
		deCultureVolatile.setActionCommand("DEBUG CULTURE VOLATILE");
		deCultureVolatile.addActionListener(this);

		deCultureSticky = new JTextField(1);
		deCultureSticky.setActionCommand("DEBUG CULTURE STICKY");
		deCultureSticky.addActionListener(this);

		deDisease = new JTextField(1);
		deDisease.setActionCommand("DEBUG DISEASE");
		deDisease.addActionListener(this);

		deDiseaseCost = new JTextField(1);
		deDiseaseCost.setActionCommand("DEBUG DISEASE COST");
		deDiseaseCost.addActionListener(this);

		deDiseaseTreat = new JTextField(1);
		deDiseaseTreat.setActionCommand("DEBUG DISEASE TREAT");
		deDiseaseTreat.addActionListener(this);

		deDiseaseTransmit = new JTextField(1);
		deDiseaseTransmit.setActionCommand("DEBUG DISEASE TRANSMIT");
		deDiseaseTransmit.addActionListener(this);

		//Draws First Line of Config Panel
		JPanel co0Panel = new JPanel();
		co0Panel.setLayout(new BoxLayout(co0Panel, BoxLayout.X_AXIS));
		co0Panel.setPreferredSize(new Dimension(580,28));
		co0Panel.setBorder(doubleBlack);
		JPanel co1Panel = new JPanel();
		co1Panel.setLayout(new BoxLayout(co1Panel, BoxLayout.X_AXIS));
		co1Panel.setPreferredSize(new Dimension(580,28));
		co1Panel.setBorder(doubleBlack);
		JPanel co2Panel = new JPanel();
		co2Panel.setLayout(new BoxLayout(co2Panel, BoxLayout.X_AXIS));
		co2Panel.setPreferredSize(new Dimension(580,28));
		co2Panel.setBorder(doubleBlack);
		JPanel co3Panel = new JPanel();
		co3Panel.setLayout(new BoxLayout(co3Panel, BoxLayout.X_AXIS));
		co3Panel.setPreferredSize(new Dimension(580,28));
		co3Panel.setBorder(doubleBlack);
		JPanel co4Panel = new JPanel();
		co4Panel.setLayout(new BoxLayout(co4Panel, BoxLayout.X_AXIS));
		co4Panel.setPreferredSize(new Dimension(580,28));
		co4Panel.setBorder(doubleBlack);
		JPanel co5Panel = new JPanel();
		co5Panel.setLayout(new BoxLayout(co5Panel, BoxLayout.X_AXIS));
		co5Panel.setPreferredSize(new Dimension(580,28));
		co5Panel.setBorder(doubleBlack);
		JPanel co6Panel = new JPanel();
		co6Panel.setLayout(new BoxLayout(co6Panel, BoxLayout.X_AXIS));
		co6Panel.setPreferredSize(new Dimension(580,28));
		co6Panel.setBorder(doubleBlack);
		
		configPanel.add(co0Panel);
		configPanel.add(co1Panel);
		configPanel.add(co2Panel);
		configPanel.add(co3Panel);
		configPanel.add(co4Panel);
		configPanel.add(co5Panel);
		configPanel.add(co6Panel);
		
		co0Panel.add(new JLabel(" Debug All "));
		co0Panel.add(deDebug);
		co0Panel.add(new JLabel(" Critical Errors "));
		co0Panel.add(deCriticalError);
		co0Panel.add(new JLabel(" Command Feedback "));
		co0Panel.add(deCmdFeedback);
		co0Panel.add(new JLabel(" Program Flow "));
		co0Panel.add(deProgFlow);
		
		co1Panel.add(new JLabel(" Gather "));
		co1Panel.add(deSugarSearch);
		co1Panel.add(new JLabel(" Sugar Production "));
		co1Panel.add(deProductionSugar);
		co1Panel.add(new JLabel(" Spice Production "));
		co1Panel.add(deProductionSpice);
		co1Panel.add(new JLabel(" Pollution "));
		co1Panel.add(dePollution);

		co2Panel.add(new JLabel(" Mating "));
		co2Panel.add(deMating);
		co2Panel.add(new JLabel(" Mating Search "));
		co2Panel.add(deMateSearch);
		co2Panel.add(new JLabel(" Mating Selection "));
		co2Panel.add(deMateSelection);
		co2Panel.add(new JLabel(" Child Birth "));
		co2Panel.add(deChildBirth);
		co2Panel.add(new JLabel(filler(10)));

		co3Panel.add(new JLabel(" Barter "));
		co3Panel.add(deBarter);
		co3Panel.add(new JLabel(" Barter Sort "));
		co3Panel.add(deBarterSort);
		co3Panel.add(new JLabel(" Barter Exchange "));
		co3Panel.add(deBarterExchange);
		co3Panel.add(new JLabel(filler(55)));
		
		co4Panel.add(new JLabel(" Culture: All "));
		co4Panel.add(deCulture);
		co4Panel.add(new JLabel(" Transmit "));
		co4Panel.add(deCultureTransmit);
		co4Panel.add(new JLabel(" Volatile "));
		co4Panel.add(deCultureVolatile);
		co4Panel.add(new JLabel(" Sticky "));
		co4Panel.add(deCultureSticky);


		co5Panel.add(new JLabel(" Disease: All "));
		co5Panel.add(deDisease);
		co5Panel.add(new JLabel(" Cost "));
		co5Panel.add(deDiseaseCost);
		co5Panel.add(new JLabel(" Treat "));
		co5Panel.add(deDiseaseTreat);
		co5Panel.add(new JLabel(" Infect "));
		co5Panel.add(deDiseaseTransmit);
		//co5Panel.add(new JLabel(filler(110)));
		
		co6Panel.add(new JLabel(" Time Tracking "));
		co6Panel.add(deTime);
		co6Panel.add(new JLabel(" Inheritance "));
		co6Panel.add(deInheritance);
		co6Panel.add(new JLabel(filler(110)));
		
		updateConfigPanel();
	}
	
	public void updateConfigPanel()
	{
		deDebug.setText(Boolean.toString(GoLconst.DEBUG));
		deCriticalError.setText(Boolean.toString(GoLconst.DEBUG_CRITICAL_ERROR));
		deCmdFeedback.setText(Boolean.toString(GoLconst.DEBUG_CMD_FEEDBACK));
		deProgFlow.setText(Boolean.toString(GoLconst.DEBUG_PROGRAM_FLOW));
		//deCitizenBirth = new JTextField(1);
		//deCitizenDeath = new JTextField(1);
		deSugarSearch.setText(Boolean.toString(GoLconst.DEBUG_SEARCH_SUGAR));
		deProductionSugar.setText(Boolean.toString(GoLconst.DEBUG_SUGAR_PRODUCTION));
		deProductionSpice.setText(Boolean.toString(GoLconst.DEBUG_SPICE_PRODUCTION));
		deMating.setText(Boolean.toString(GoLconst.DEBUG_MATING));
		deMateSearch.setText(Boolean.toString(GoLconst.DEBUG_MATING_SEARCH));
		deMateSelection.setText(Boolean.toString(GoLconst.DEBUG_MATING_SELECTION));
		deChildBirth.setText(Boolean.toString(GoLconst.DEBUG_MATING_BIRTH));
		dePollution.setText(Boolean.toString(GoLconst.DEBUG_POLLUTION));
		deBarter.setText(Boolean.toString(GoLconst.DEBUG_BARTER));
		deBarterSort.setText(Boolean.toString(GoLconst.DEBUG_BARTER_SORT));
		deBarterExchange.setText(Boolean.toString(GoLconst.DEBUG_BARTER_EXCHANGE));
		
		deCulture.setText(Boolean.toString(GoLconst.DEBUG_CULTURE));
		deCultureTransmit.setText(Boolean.toString(GoLconst.DEBUG_CULTURE_TRANSMIT));
		deCultureVolatile.setText(Boolean.toString(GoLconst.DEBUG_CULTURE_VOLATILE));
		deCultureSticky.setText(Boolean.toString(GoLconst.DEBUG_CULTURE_STICKY));
		
		deDisease.setText(Boolean.toString(GoLconst.DEBUG_DISEASE));
		deDiseaseCost.setText(Boolean.toString(GoLconst.DEBUG_DISEASE_COST));
		deDiseaseTreat.setText(Boolean.toString(GoLconst.DEBUG_DISEASE_TREAT));
		deDiseaseTransmit.setText(Boolean.toString(GoLconst.DEBUG_DISEASE_TRANSMIT));
		
		deTime.setText(Boolean.toString(GoLconst.DEBUG_PROCESS_TIME));
		deInheritance.setText(Boolean.toString(GoLconst.DEBUG_INHERITANCE));
	}
}

/*
 * Created on Feb 11, 2004
 */
final class ControlStartStop implements ActionListener
{	private final Sugarscape sugarscape;
	/**
	 * @param Sugarscape
	 */
	ControlStartStop(Sugarscape sugarscape)
	{
		this.sugarscape = sugarscape;
	}
	public void actionPerformed(ActionEvent e)
		{	this.sugarscape.actionControls((String)sugarscape.startstopButton.getActionCommand());	}
}

/*
 * Created on Feb 11, 2004
 */
final class ControlNext implements ActionListener
{	private final Sugarscape sugarscape;
	/**
	 * @param Sugarscape
	 */
	ControlNext(Sugarscape sugarscape)
	{
		this.sugarscape = sugarscape;
	}
	public void actionPerformed(ActionEvent e)
		{	this.sugarscape.actionControls((String)sugarscape.nextButton.getActionCommand());	}
}

/*
 * Created on Feb 11, 2004
 */
final class ControlChoice implements ActionListener
{	private final Sugarscape sugarscape;
	/**
	 * @param Sugarscape
	 */
	ControlChoice(Sugarscape sugarscape)
	{
		this.sugarscape = sugarscape;
	}
	public void actionPerformed(ActionEvent e)
		{	this.sugarscape.actionControls((String)sugarscape.c.getSelectedItem());	}
}

/*
 * Created on Feb 11, 2004
 */
final class ControlSpeed implements ActionListener
{	private final Sugarscape sugarscape;
	/**
	 * @param Sugarscape
	 */
	ControlSpeed(Sugarscape sugarscape)
	{
		this.sugarscape = sugarscape;
	}
	public void actionPerformed(ActionEvent e)
		{	this.sugarscape.actionControls((String)sugarscape.speed.getSelectedItem());	}
}